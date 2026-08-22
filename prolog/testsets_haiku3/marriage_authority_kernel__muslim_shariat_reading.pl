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
 *   human_readable: Muslim Shariat Marriage Authority (Qazi and Board Interpretation)
 *   domain: legal/religious/family governance
 *
 * SUMMARY:
 *   Muslim Shariat marriage authority in India is one reading of a contested
 *   kernel about who adjudicates family law in a pluralist democracy. This
 *   reading instantiates authority grounded in Shariat as interpreted by qazi
 *   networks and Muslim personal law boards. The constraint has two
 *   structural components: (1) a genuine coordination function — providing
 *   internally coherent family law rooted in religious tradition and
 *   community recognition, reducing state interference in minority religious
 *   matters, (2) a substantial extraction component — transfer of authority
 *   from individual/egalitarian consent to community-mediated qazi
 *   interpretation that privileges males and renders women's exit costlier.
 *   The claim/metric gap is structural: the reading is CLAIMED as
 *   tangled_rope (coordination + enforcement), and the metrics describe
 *   extractiveness and suppression rising modestly over the interval,
 *   indicating the enforcement machinery has intensified while the
 *   coordination rationale plateaued. This is the measured gap the engine
 *   uses to flag mandate drift.
 *
 * KEY AGENTS:
 *   - Qazi authority (institutional agenda-setter): interprets Shariat, enforces via community sanction and limited state recognition
 *   - Muslim personal law boards (institutional agenda-setter): coordinate qazi networks, provide interpretive guidance, lobby for state deference
 *   - Male heads of household (beneficiary): retain unilateral talaq, polygamy, inheritance preference
 *   - Women in marriage (powerless payer): subject to unilateral talaq, restricted exit, identity-locked by religious/familial fusion
 *   - Religious minorities within community (powerless payer): Shia/Ahmadiyya minorities experience Sunni-orthodox Shariat enforcement
 *   - Muslim modernizers (organized but excluded): advocate for civil-code reform, excluded from qazi-board authority
 *   - Civil courts (institutional observer): apply deference doctrine, hear constitutional challenges, increasingly divided on gender equity
 *   - State legislature (institutional observer): constitutionally empowered to reform but politically constrained by communalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.71).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Shariat Marriage Authority (Qazi and Board Interpretation)").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "legal/religious/family governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, 'b55d5e40-f1c5-440a-b34a-e2ec649bb4b3').
narrative_ontology:cs_kernel_codification('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', fixed_text).
narrative_ontology:cs_authority_grounding('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', lineage).
narrative_ontology:cs_interpretation_layer_present('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3').
narrative_ontology:cs_reading_relation('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', foundational, shariat_primacy_in_muslim_family_law).
narrative_ontology:cs_axiom_status(shariat_primacy_in_muslim_family_law, holdable).
narrative_ontology:cs_axiom_grounding('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', shariat_primacy_in_muslim_family_law, theological).
narrative_ontology:cs_axiom('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', foundational, community_authority_over_individual_consent).
narrative_ontology:cs_axiom_status(community_authority_over_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', community_authority_over_individual_consent, conventional).
narrative_ontology:cs_reference_frame('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', shariat_as_interpreted_by_lineage_authority).
narrative_ontology:cs_drift_state('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b55d5e40-f1c5-440a-b34a-e2ec649bb4b3', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_heads_of_household).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_community_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazi_administrative_authority).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, women_in_marriage).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, religious_minorities_within_muslim_community).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, secular_modernizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, religious_minorities_within_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies Shariat to marriage, dissolution, and property disputes. Authority derives from religious legitimacy and community recognition. Sets terms for talaq (divorce), dower, custody, and inheritance. Maintains parallel adjudicatory system to civil courts; enforces via community sanction and limited state recognition in several jurisdictions.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazi_authority, agenda_setter,
    institutional, generational, constrained, regional).

% Coordinate qazi networks and provide interpretive authority on Shariat. Claim to protect Muslim personal law from state interference and preserve community autonomy. Issue fatwas and guidance on family law matters; their interpretations carry weight in qazi courts and influence state judicial deference.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, beneficiary).

% Retain unilateral talaq rights, polygamy rights, and preferential inheritance claims under Shariat interpretation. Exit options are limited by community enforcement (social ostracism, family pressure) and limited civil-law override where Shariat courts retain jurisdiction. Benefits from authority structure that privileges their consent and minimizes spousal recourse.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_heads_of_household, beneficiary,
    moderate, biographical, constrained, local).

% Subject to unilateral talaq by husbands, restricted inheritance rights, limited custody claims, and dower obligations. Exit via secular civil courts is constrained by community/family pressure, religious identity fusion, and uneven state enforcement. Dispute resolution occurs in qazi courts where their voice carries less weight; they cannot initiate talaq without spouse consent or judicial cause (far higher burden than husbands).
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, women_in_marriage, payer,
    powerless, biographical, identity_locked, local).

% Shias, Ahmadiyyas, and other sects experience qazi authority as enforcing Sunni-orthodox Shariat; their own theological interpretations are overridden. Community enforcement (threat of excommunication, family severance) traps them within the authority structure. State courts often defer to majority-community qazis on jurisdictional grounds.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, religious_minorities_within_community, payer,
    powerless, biographical, identity_locked, local).

% Advocate for civil-code marriage equality and state-level reform. Excluded from qazi-board decision-making; their voices appear in legislative testimony and civil litigation but carry no formal weight in the parallel authority system. State deference to Shariat law marginalizes their input.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_modernizers, excluded,
    organized, biographical, constrained, national).

% Retain appellate jurisdiction but apply doctrine of deference to Shariat on matters within personal law scope. Hear constitutional challenges to qazi authority; increasingly divided on whether gender-inequitable provisions violate fundamental rights. Their reluctance to fully override qazi jurisdiction preserves the parallel system.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, indian_civil_courts, observer,
    institutional, generational, analytical, national).

% Has constitutional authority to reform personal law but defers to community autonomy and religious accommodation doctrine. Politically constrained by majority communalism; reform efforts (like Uniform Civil Code) face organized resistance from religious boards. Their non-intervention preserves the Shariat authority structure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, state_legislature, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, qazi_authority).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a internally-coherent framework for family law adjudication rooted in religious tradition and community recognition. Reduces friction with the state legal system for Muslims by offering parallel adjudication that honors Shariat principles without requiring state conversion of family matters; coordinates community expectations around marriage, dissolution, and inheritance via a shared textual/interpretive tradition.
% TRANSFER_FUNCTION: Transfers authority over marriage, divorce, property, and succession from individual choice and secular civil law to community-mediated qazi interpretation. Moves women's unilateral divorce rights, inheritance equality, and child custody to qazi discretion constrained by Shariat reading; transfers male prerogatives (unilateral talaq, polygamy, greater inheritance) to formal institutional recognition. Transfers legitimacy from state law to religious authority.
% ABSENT_VOICES: Women's rights advocates, religious minorities within the Muslim community (Shias, Ahmadiyyas seeking their own interpretive authority), and secular-modernizer Muslims seeking civil-code marriage. These actors would argue for gender equity, individual consent over community adjudication, and state enforcement of constitutional rights; they are structurally excluded from qazi-board interpretation and face community sanction for appealing beyond religious authority.
% DISAPPEARANCE_RATIONALE: If Shariat marriage authority and community enforcement vanished, millions of Muslim families would experience jurisdictional vacuum; outcomes would depend on state legislative response. Likely: civil courts would absorb family cases under secular law, dower and talaq disputes would route through civil procedure, inheritance would follow civil succession law, unilateral talaq would cease to be recognized. Community identity-fusion around Shariat law would degrade; Muslims seeking recognition of religious family arrangements would lose a parallel system. The reorganization would be contested and traumatic for traditionalist segments.
% FOUNDING_PROBLEM: Colonial exclusion: the Shariat system persisted under British rule as a concession to Muslim autonomy and a practical administrative solution (British did not want to adjudicate religious family law). Post-independence: constitutional accommodation doctrine treated religious personal law as protecting minority rights and community autonomy from majoritarian secular law. The founding problem is the need to preserve space for religious community self-governance in a multicultural state.
% FOUNDING_PROBLEM_CORROBORATION: Muslim personal law boards and traditionalist scholars attest the founding problem is live: state imposition of secular family law would violate religious freedom and destroy community self-governance. Constitutional courts and legal scholars affirm that minority religious autonomy requires personal-law deference. However, women's rights organizations, constitutional-rights scholars, and secular-modernizer Muslims attest the founding problem is obsolete or fraudulent: it was framed to justify authority structures that predate modern human-rights norms, and 'community autonomy' masks the suppression of dissenting voices within the community (women, minorities, modernizers). The corroboration is split across irreconcilable camps, with state deference to traditional authority suggesting the problem statement was accepted by power-holders.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) is high but not extreme because the coordination function (coherent family law rooted in shared religious tradition) is genuine and valued by many within the community; the extracted component is authority transfer and gender inequity. Suppression (0.71) is higher than extractiveness because persistence of the constraint depends on active community enforcement (social ostracism, family pressure, threats of excommunication for exit attempts) and state deference to qazi jurisdiction — without these, exit options would expand. Theater (0.28) is low-moderate: qazis do adjudicate real disputes and apply Shariat reasoning, but a growing share of their activity defends gendered prerogatives (processing unilateral talaq, enforcing dower) rather than solving collective-action problems. The measurements are authored on a single time grid (1947, 1975, 1990, 2005, 2015, 2026) showing that extractiveness rose modestly from 0.61 to 0.68 over 79 years while suppression intensified more sharply (0.58 to 0.71), indicating the enforcement machinery became more organized and defended more actively even as the coordination rationale stalled. Theater ratio nearly doubled (0.12 to 0.28), suggesting performative maintenance of traditional prerogatives increasingly comprises the activity.
 *
 * PERSPECTIVAL GAP:
 *   The qazi/board seat and the female-payer seat should compute as radically different types. From the qazi perspective: genuine coordination solving a real problem (religious autonomy, minority protection, coherent family law rooted in tradition) — a rope or even legitimate tangled_rope where coordination benefits outweigh extraction. From the female-payer perspective: the same structure operates as enforced extraction (unilateral talaq right held by husband, restricted divorce, inheritance inequality, identity-locked exit via religious/familial fusion) — a snare or high-extraction tangled_rope where enforcement exceeds coordination benefit. From the modernizer perspective: the coordination rationale is cover for male prerogatives and community gatekeeping. The engine computes these differences from the structural data (male beneficiary/power vs. female powerless/identity_locked; institutional vs. powerless exit options; beneficiary-derived vs. victim-derived directionality).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Qazi authority is a beneficiary (sets terms, collects legitimacy, faces no exit cost) — d near 0.0. Male heads of household are beneficiaries (retain prerogatives, face constrained but manageable exit via remarriage/polygamy) — d around 0.20-0.30. Women are victims (face unilateral talaq, constrained exit via identity-lock, no direct power over rules) — d near 1.0. Religious minorities are victims (overridden by majority interpretation, identity-locked via community enforcement) — d near 0.90. The state is observer (d = 0.5, analytical). No directionality overrides needed: the structural derivation correctly captures that male beneficiaries and female payers experience this constraint at opposite poles.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy detection (founding_problem_status = contested, disappearance_verdict = world_rearranges, theater_ratio rising over time): the founding problem ('protect Muslim autonomy and minority religious self-governance') was live in 1947 but is now contested. Traditionalist actors attest it is still live; women's rights and modernizer actors attest it is obsolete or fraudulent (used to justify suppression of internal dissent). The theater_ratio increase (0.12 to 0.28) indicates growing performative maintenance — qazis spend proportionally more effort defending gendered prerogatives and community boundaries than solving coordination problems. The suppression_requirement intensification (0.58 to 0.71) indicates enforcement machinery became more active even as the founding problem's legitimacy eroded. This is the classic mandatrophy signature: an authority structure whose original mandate has outlived its universal support but persists because concentrated beneficiaries (male household heads, institutional gatekeepers) have incentive to maintain it against growing organized resistance (women's movements, modernizers, civil-rights constitutionalism). The constraint does not meet the piton threshold (theater_ratio < 0.5 — it is still 50%+ functionally adjudicating) but is migrating toward it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_autonomy_vs_individual_rights,
    'Is preserving religious community autonomy in family law a legitimate accommodation of minority rights, or does it permit suppression of internal dissent and violation of constitutional individual rights?',
    'Formal constitutional amendment or Supreme Court overruling of deference doctrine, combined with empirical research on women''s and minorities'' preferences when exit costs are reduced (exit surveys post-civil-code reforms in other democracies; voice surveys among Indian Muslim women when legal reforms proposed).',
    'If legitimacy shifts to individual rights, the constraint reclassifies from tangled_rope (coordination + enforcement) to snare (pure extraction under traditional cover). State would withdraw deference, civil courts would apply constitutional rights, qazi authority would persist only by voluntary adherence. If community autonomy retains legitimacy, the constraint holds as tangled_rope or even rope-with-high-variance-seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_autonomy_vs_individual_rights, conceptual, 'Whether minority religious autonomy justifies gender-inequitable family law or whether individual constitutional rights override communal prerogatives.').

omega_variable(
    gender_inequity_as_essential_vs_contingent,
    'Are unilateral talaq, polygamy, and inheritance asymmetry essential to Shariat family law, or are they contingent historical interpretations that reformed readings of Islamic jurisprudence could revise?',
    'Textual analysis of Quranic foundations and competing ijthad (jurisprudential reasoning) traditions; ethnographic study of Muslim reformers'' theological arguments; comparative analysis of gender-equity provisions in some Muslim-majority jurisdictions (Tunisia, Morocco, Indonesia have reformed Shariat on gender grounds).',
    'If the inequities are contingent, they become authored oppression rather than natural-law constraints; the qazi system could reform while preserving Shariat authority. If essential, reform requires either suppressing Shariat interpretation or accepting the gender asymmetry as the cost of religious autonomy. This distinction moves the classification between snare (oppressions framed as religious law) and tangled_rope (a coordination solution whose extraction component is debated within tradition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_inequity_as_essential_vs_contingent, empirical, 'Whether gender inequity flows from theological necessity or interpretive choice within Islamic jurisprudence.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is women''s exit constraint in Shariat marriage authority structurally imposed (economic dependency, legal barriers, institutional exclusion) or substantially internalized (identity-fusion with religious role, belief in gendered religious duty)?',
    'Post-exit trajectory analysis: if women who escape Shariat jurisdiction continue to behave as if constrained even after legal/economic barriers are removed, internalization is substantial. Ethnographic studies of ex-Muslim women and Sharia-reformed Muslims document persistence of constraint internalization after structural escape; comparison with women who exit other patriarchal systems.',
    'If internalized, the measured suppression underestimates effective constraint — women carry it with them after exit. Reclassification would raise suppression_requirement and shift the payer seat toward higher extraction. Policy interventions targeting structural barriers alone would fail if internalization is primary. If structural, removing barriers (state protection, alternative jurisdiction, economic support) would suffice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether measured suppression is structural (external barriers) or substantially internalized (women''s own belief in constraint legitimacy).').

omega_variable(
    qazi_system_as_coordination_vs_gatekeeping,
    'Does the qazi system primarily solve the collective-action problem of applying a shared religious tradition to family disputes, or does it primarily enable male gatekeeping and community enforcement against women''s and minorities'' voices?',
    'Comparative institutional analysis: measure dispute-resolution speed, perceived fairness by each stakeholder group, appeal rates and success rates, and whether qazi courts produce outcomes that reflect stakeholders'' stated preferences or qazi preferences. Ethnographic study of how women and minorities experience qazi adjudication versus civil courts.',
    'If primarily coordination, the constraint deserves higher classification as rope or legitimate tangled_rope. If primarily gatekeeping, it reclassifies toward snare. The finding would also modulate the ''founding problem status'' from contested toward dead (if gatekeeping is primary) or live (if coordination is primary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qazi_system_as_coordination_vs_gatekeeping, empirical, 'Whether qazi authority primarily solves a coordination problem or primarily enables male/elite gatekeeping.').

omega_variable(
    kernel_reading_foreclosure_secular_civil,
    'Does the secular-civil reading (Special Marriage Act 1954, individual constitutional rights) logically foreclose the Shariat reading within a single constitutional framework, or can both persist indefinitely as competing authority structures?',
    'Constitutional law analysis and ongoing litigation: if courts rule that constitutional rights to equality and freedom of conscience override Shariat prerogatives, foreclosure has occurred. Ethnographic observation of whether dual jurisdictions can coexist without contradiction or whether they are destabilizing.',
    'If foreclosure occurs, this reading migrates from coexisting-with-secular-civil to forecloses-by-secular-civil, a downstream influence that degrades its legitimacy. The constraint type would shift as state enforcement erodes. If coexistence persists, the constraint remains stable as a contested parallel system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_secular_civil, conceptual, 'Whether constitutional individual rights logically foreclose Shariat''s communal authority or whether both can coexist indefinitely.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1947, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1947, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1947, 0.12).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1975, 0.16).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1990, 0.21).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(marr_tr_t2026, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1947, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1947, 0.61).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1975, 0.64).
narrative_ontology:measurement(marr_be_t1990, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(marr_be_t2026, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1947, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1947, 0.58).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1975, 0.63).
narrative_ontology:measurement(marr_su_t1990, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1990, 0.67).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(marr_su_t2026, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__muslim_shariat_reading, 0.15).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the marriage-authority kernel family in Indian constitutional pluralism. The kernel is the standing commitment 'who adjudicates family law?' — five readings instantiate five different constraints with different beneficiaries, victims, and extraction profiles. The Shariat reading (this file) coexists with and influences the secular-civil reading, which in turn influences the Shariat reading via appeals and constitutional challenges. The Hindu-codified reading influences via comparative jurisprudence (Hindu courts' rulings on gender equity sometimes pressure Shariat courts toward reform). All five readings are linked via network.affects_constraints to enable contamination and foreclosure analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
