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
 *   This story instantiates the Muslim Shariat reading of the contested
 *   marriage-authority kernel in Indian personal law: family law authority
 *   for Muslims derives not from a codified civil statute administered by
 *   civil courts, but from Shariat as interpreted by community personal law
 *   boards and applied by qazis in parallel dar-ul-qaza tribunals. The
 *   reading is distinguished from its siblings by its adjudicative locus
 *   (community tribunal rather than civil court), its gender-equity profile
 *   (unilateral male-initiated divorce, permitted polygamy, unequal
 *   inheritance shares), and the contested, episodic character of state
 *   intervention (Shah Bano 1985, the 1986 Muslim Women's Act reversal,
 *   Shayara Bano 2017 invalidating instant triple talaq). This story authors
 *   ONLY this reading's structure — the Hindu codified, Christian canonical,
 *   Parsi communal, and secular civil readings are separate constraints with
 *   their own ε and stakeholder sets, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - muslim_personal_law_boards: primary agenda_setter — interprets Shariat, resists codification, captures political and institutional standing from being recognized interpretive authority
 *   - qazis_and_dar_ul_qaza_networks: adjudicative agenda_setter/beneficiary — runs the parallel tribunal system, collects fees and legitimacy
 *   - muslim_wives_subject_to_unilateral_talaq, co_wives_in_polygamous_households, muslim_daughters_and_widows_in_inheritance: primary payers — bear the gender-differentiated costs structurally
 *   - indian_state_and_constitutional_courts: excluded/observer — holds formal constitutional authority but intervenes only episodically against organized resistance
 *   - muslim_womens_reform_organizations: excluded voice — advocates reform, lacks standing inside the interpretive process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law (Shariat) Marriage/Family Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '763d6fc0-0fbe-4d36-be1c-3114d4c37be2').
narrative_ontology:cs_kernel_codification('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', distributed).
narrative_ontology:cs_authority_grounding('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', lineage).
narrative_ontology:cs_interpretation_layer_present('763d6fc0-0fbe-4d36-be1c-3114d4c37be2').
narrative_ontology:cs_reading_relation('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', foundational, community_interpretive_sovereignty_over_shariat).
narrative_ontology:cs_axiom_status(community_interpretive_sovereignty_over_shariat, holdable).
narrative_ontology:cs_axiom_grounding('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', community_interpretive_sovereignty_over_shariat, theological).
narrative_ontology:cs_axiom('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', secondary, unilateral_male_initiated_dissolution_permissible).
narrative_ontology:cs_axiom_status(unilateral_male_initiated_dissolution_permissible, holdable).
narrative_ontology:cs_axiom_grounding('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', unilateral_male_initiated_dissolution_permissible, theological).
narrative_ontology:cs_reference_frame('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', colonial_era_personal_law_settlement).
narrative_ontology:cs_drift_state('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', post_shayara_bano_2017, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('763d6fc0-0fbe-4d36-be1c-3114d4c37be2', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis_and_dar_ul_qaza_networks).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_household_heads).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_wives_subject_to_unilateral_talaq).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, co_wives_in_polygamous_households).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_daughters_and_widows_in_inheritance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Represents itself as guardian and interpreter of Shariat for the community, lobbies against codification or state reform of Muslim personal law, and issues authoritative fatwas and model guidance that qazis and community members treat as binding. Faces no exit cost from maintaining current doctrine and gains standing, funding, and political leverage from being the recognized interpretive authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, agenda_setter,
    organized, generational, arbitrage, national).

% Adjudicates marriage, divorce, and maintenance disputes through community tribunals (dar-ul-qaza) that operate parallel to and sometimes instead of civil courts, collecting fees and social authority from being the forum of first resort. Depends on the community continuing to route disputes to them rather than to state courts for both income and legitimacy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis_and_dar_ul_qaza_networks, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, qazis_and_dar_ul_qaza_networks, beneficiary).

% Holds the unilateral power to pronounce talaq and to contract polygamous marriage under the prevailing interpretation, with comparatively low procedural or financial cost to exercising either. Retains full civil exit options (can access secular courts, migrate jurisdictions, or simply not exercise these powers) that the women in the same household structurally lack.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_household_heads, beneficiary,
    moderate, biographical, mobile, local).

% Can be divorced through instantaneous or minimally-mediated pronouncement (historically triple talaq, now partially curtailed by statute but still contested in community-tribunal practice), with maintenance and custody outcomes set largely by community-tribunal norms rather than uniform civil standard. Formal recourse to constitutional courts exists on paper but requires resources, literacy, and social risk-tolerance most do not have, and community pressure discourages using it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_wives_subject_to_unilateral_talaq, payer,
    powerless, biographical, trapped, local).

% Shares a husband under a reading of Shariat that permits polygamy without requiring the first wife's consent, absorbing reduced maintenance share, reduced attention, and reduced social standing as a structural feature of the marriage rather than a negotiated exception. Exiting means forfeiting marital status, housing, and often custody standing inside the community's own adjudicative norms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, co_wives_in_polygamous_households, payer,
    powerless, biographical, trapped, local).

% Receives a fixed, generally smaller Quranic share of inheritance relative to male heirs under the applied interpretation, administered by family and community consensus rather than gender-neutral civil succession rules. Contesting the division means contesting family and community authority simultaneously, which most do not do.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_daughters_and_widows_in_inheritance, payer,
    powerless, generational, trapped, local).

% Holds constitutional authority over marriage and civil rights but has historically deferred to the Muslim Personal Law (Shariat) Application Act framework, intervening only episodically (Shah Bano, triple talaq invalidation, Muslim Women Act amendments) and each time facing organized political resistance framing intervention as an attack on minority religious autonomy. Structurally present but functionally excluded from routine adjudication, which happens inside community tribunals the state does not administer.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, indian_state_and_constitutional_courts, excluded,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, indian_state_and_constitutional_courts, observer).

% Advocates for codification, gender-equitable reinterpretation of Shariat, or access to secular alternatives, but is not treated as an authoritative interpretive voice by the personal law boards and has limited standing inside the qazi adjudication process it seeks to reform.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_womens_reform_organizations, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Muslim community members a recognized, religiously-legitimate, low-cost, culturally-fluent forum for resolving marriage, divorce, maintenance, and inheritance disputes without requiring engagement with an unfamiliar or historically distrusted civil court system, and preserves communal religious identity against assimilationist uniform civil code pressure.
% TRANSFER_FUNCTION: Moves decision-making authority and negotiating leverage in marriage, divorce, and inheritance from women within the community to male household heads and community religious authorities, and moves adjudicative fees and social capital from disputing parties to qazis and personal law boards.
% ABSENT_VOICES: Muslim women's reform organizations and individual women seeking gender-equitable outcomes are structurally outside the interpretive process: personal law boards are dominated by male religious scholars, and dissenting women's voices are treated as external critics of the community rather than participants in it. The Indian state's constitutional equality apparatus is present in principle but has been repeatedly forced into a defensive posture by organized political framing of intervention as anti-minority.
% DISAPPEARANCE_RATIONALE: If the Shariat-derived authority structure disappeared overnight and all Muslim marriages fell under a uniform secular civil code, unilateral talaq without judicial process would end, polygamy would require the same restrictions as for other communities, inheritance shares would equalize by sex, and community tribunals would lose their adjudicative role and fee base — a substantial and contested rearrangement, not a null change.
% FOUNDING_PROBLEM: Colonial and post-independence India needed to reconcile a religiously and culturally plural population with a unified legal system without triggering communal conflict or appearing to impose majority religious norms on minorities; personal law preservation (culminating in the 1937 Shariat Application Act) was framed as protecting minority religious freedom and community self-governance.
% FOUNDING_PROBLEM_CORROBORATION: Personal law boards and communal leaders attest the founding problem — protecting minority religious autonomy against majoritarian legal imposition — remains fully live. Independent sources outside the benefiting parties dispute this: the Law Commission of India, multiple Supreme Court benches (Shah Bano 1985, Shayara Bano 2017), and Muslim women's rights organizations attest that the protective rationale has been substantially repurposed into a shield for gender-inequitable outcomes that the founding framers did not anticipate as its primary function; no source outside the personal law boards themselves attests the current gender-differentiated practice as the original or necessary content of the founding compromise.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate-high (0.58) because the arrangement genuinely solves a coordination problem (accessible, culturally legitimate dispute resolution for a religious minority) while also structurally transferring bargaining power and material shares from women to men within the same mechanism — this dual character is exactly the tangled_rope signature. Suppression (0.60) reflects both external barriers (social and economic cost of exiting to civil courts, community ostracism risk) and the entrenchment of dar-ul-qaza as the practical forum of first resort. Accessibility collapse is moderate (0.50): secular courts exist in principle (Special Marriage Act, constitutional writ jurisdiction) but are not practically accessible to most affected women given cost, literacy, and social risk. Resistance is substantial (0.55) — organized women's reform movements, periodic litigation (Shah Bano, Shayara Bano), and legislative reform attempts (2019 Muslim Women (Protection of Rights on Marriage) Act) all constitute real, sustained resistance, distinguishing this from a settled mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Personal law boards and qazis sit near the full-beneficiary end: they set and administer the interpretive framework, collect fees/standing, and bear essentially no cost from its continuation (organized power, arbitrage/constrained exit respectively). Male household heads benefit structurally (unilateral talaq and polygamy rights) while retaining full exit options into the same civil system the arrangement denies women meaningful access to. Wives, co-wives, and female heirs sit near the full-target end: powerless, trapped exit, and the specific transfers (divorce initiation, maintenance share, inheritance share) run systematically against them. The state and reform organizations are excluded observers/advocates rather than parties who gain or pay, reflecting their structural absence from the routine adjudicative process despite formal jurisdiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting a religious minority's self-governance against majoritarian legal imposition in a newly independent, communally anxious state — was genuinely live in 1937 and remains partially live today given continued communal tension around uniform civil code proposals. But the founding_problem_status is authored as contested rather than dead: the protective function persists for some purposes (religious identity preservation, community dispute-resolution access) while having been substantially repurposed to shield internally gender-inequitable outcomes the original framers may not have specifically intended as the arrangement's core content. This is precisely the tangled_rope case the classification exists to prevent from being flattened into either 'pure minority-rights coordination' or 'pure patriarchal extraction' — both a real coordination function and a real, asymmetric, actively-enforced transfer coexist in the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_law_vs_state_capture_ambiguity,
    'Is the persistence of Shariat-derived personal law authority a genuine exercise of constitutionally-protected minority religious autonomy, or has it become a captured mechanism through which male community authorities and personal law boards extract disproportionate control over women''s marital and inheritance outcomes under religious cover?',
    'Comparative analysis of outcomes for Muslim women under community-tribunal adjudication versus outcomes for similarly-situated women under the secular civil code reading, controlling for socioeconomic status; and tracking whether internal reform movements (progressive fatwa councils, women qazis) succeed in shifting interpretive practice from within, which would support the genuine-autonomy reading, versus continued suppression of internal reform, which would support the capture reading.',
    'If genuine autonomy with internally-available reform paths, the arrangement is closer to a rope with contested internal politics; if the capture reading dominates and internal reform is systematically blocked, the tangled_rope classification understates the extraction and a snare classification would be more accurate for the divorce/inheritance sub-functions specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_law_vs_state_capture_ambiguity, conceptual, 'Whether Shariat personal law authority is protected autonomy or captured extraction under religious cover.').

omega_variable(
    state_intervention_legitimacy_contest,
    'Does episodic constitutional court intervention (Shah Bano, Shayara Bano, the 2019 Act) represent legitimate constitutional correction of a rights-violating practice, or majoritarian erosion of minority self-governance using gender-equity language as pretext?',
    'Track the political coalition composition and stated rationale behind each intervention and its aftermath (the 1986 legislative reversal following Shah Bano is a key data point); assess whether interventions are followed by improved outcomes for the named victim groups or by backlash that worsens their practical position.',
    'If interventions are read as pretextual majoritarianism, the excluded state seat''s structural position shifts from good-faith excluded observer toward an interested party with its own extraction interest, which would require re-authoring the state stakeholder''s role and could shift the network reading of secular_civil_reading''s relationship to this constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_intervention_legitimacy_contest, preference, 'Whether state intervention in Muslim personal law is legitimate rights correction or majoritarian pretext — genuinely contested along political lines.').

omega_variable(
    framing_kernel_vs_interpretation_layer,
    'Is the correct framing kernel here ''Shariat itself'' (a fixed textual/traditional source) or ''the personal law boards'' contemporary interpretation of Shariat'' (a live, politically-contingent interpretive layer that could itself shift)? These two framings could produce different cs_structure classifications: the former suggests fixed_text/lineage with low revisability; the latter suggests formalized/practice with an active, contestable interpretive body.',
    'Examine whether internal doctrinal reform movements (e.g., progressive scholars proposing codified reform of talaq procedure) gain traction within the personal law board structure itself, versus being externally imposed by courts/legislature.',
    'Choosing the ''fixed Shariat'' framing understates the personal law board''s discretionary interpretive power and its role as an extraction-capable agenda_setter; choosing the ''contemporary interpretation'' framing (adopted here) correctly locates agency and potential reformability inside the board/qazi structure rather than treating the outcome as textually determined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_kernel_vs_interpretation_layer, conceptual, 'Whether to frame the kernel as fixed religious text or as the board''s contemporary, contestable interpretation of it — this story adopts the latter framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1937, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1955, 0.12).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(marr_tr_t2001, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(marr_tr_t2017, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(marr_be_t1937, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1937, 0.42).
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1955, 0.45).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(marr_be_t2001, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2001, 0.53).
narrative_ontology:measurement(marr_be_t2017, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2017, 0.5).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1937, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1937, 0.35).
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1955, 0.4).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(marr_su_t2001, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(marr_su_t2017, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__muslim_shariat_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five community-indexed readings of the marriage_authority_kernel, each authored as a separate constraint with its own ε, beneficiaries, victims, and classification per the ε-invariance principle. The muslim_shariat_reading shows the highest measured extraction and suppression among the five owing to its unilateral-talaq/polygamy/unequal-inheritance profile and its history of the most politically contested state intervention. secular_civil_reading functions as the counterfactual 'world_rearranges' endpoint referenced in this story's disappearance_verdict. All five readings should be treated as siblings under the shared kernel_id marriage_authority_kernel, not as competing measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
