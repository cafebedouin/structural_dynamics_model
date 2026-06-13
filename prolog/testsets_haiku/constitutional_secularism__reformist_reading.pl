% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: State Affirmative Duty to Eliminate Oppressive Religious Practices (Reformist Reading)
 *   domain: constitutional/political/religious
 *
 * SUMMARY:
 *   This constraint instantiates the REFORMIST READING of the contested
 *   kernel 'constitutional secularism': the state has an affirmative duty to
 *   eliminate religious practices oppressing marginalized groups, even when
 *   those practices are defended as core to religious autonomy. This is ONE
 *   reading of how constitutional secularism operates — it emphasizes social
 *   justice and equality as trumping community religious freedom. Sibling
 *   readings (principled_intervention_reading, strict_neutrality_reading)
 *   dispute whether the state MUST intervene, MAY intervene conditionally, or
 *   MUST remain neutral; they are live alternative positions held by
 *   different courts, constituencies, and constitutional traditions. This
 *   story captures the REFORMIST position as a structurally specific
 *   constraint with its own ε, beneficiary/victim profile, and enforcement
 *   dynamics. The reading's core claim is that when religious autonomy
 *   conflicts with equality/dignity protections for oppressed subgroups, the
 *   state not only MAY but MUST side with liberation. The constraint is
 *   CLAIMED as tangled_rope because it coordinates protection of vulnerable
 *   members AND extracts from religious communities that lose self-governance
 *   authority. The metrics describe substantial extraction (0.71 at interval
 *   end) and rising enforcement requirement (0.68 suppression at end),
 *   reflecting the reading's aggressive posture toward traditional religious
 *   authority.
 *
 * KEY AGENTS:
 *   - Scheduled castes and oppressed subgroups: structural targets of oppressive practices; powerless individually but beneficiaries of state intervention; trapped exit (cannot leave caste/community of birth).
 *   - Women oppressed by religious practice: beneficiaries of intervention; moderate organized power; constrained exit (cultural, economic, family ties).
 *   - Religious conservatives and orthodox practitioners: payer seat; organized power; constrained exit (identity-locked to tradition).
 *   - Religious community governance bodies: institutional payer + secondary agenda-setter; lose autonomy; constrained exit (cannot dissolve without ending community identity).
 *   - Constitutional courts: primary agenda-setter; set the boundaries of which practices are 'oppressive'; operational authority to enforce the reading.
 *   - State enforcement apparatus: secondary agenda-setter; execute interventions; create the visible suppression cost.
 *   - Secular reform advocates: beneficiaries (ideological validation, policy influence); powerful; arbitrage exit (can operate in secular domains).
 *   - Religious minorities: excluded from beneficiary framing; fear selective application; identity-locked exit; moderate power organized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.71).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.68).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "State Affirmative Duty to Eliminate Oppressive Religious Practices (Reformist Reading)").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional/political/religious").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5').
narrative_ontology:cs_kernel_codification('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', fixed_text).
narrative_ontology:cs_authority_grounding('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', lineage).
narrative_ontology:cs_interpretation_layer_present('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5').
narrative_ontology:cs_reading_relation('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_axiom('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', foundational, state_affirmative_duty_to_protect_equality).
narrative_ontology:cs_axiom_status(state_affirmative_duty_to_protect_equality, holdable).
narrative_ontology:cs_axiom_grounding('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', state_affirmative_duty_to_protect_equality, deontological).
narrative_ontology:cs_axiom('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', foundational, oppressed_group_equality_trumps_religious_autonomy).
narrative_ontology:cs_axiom_status(oppressed_group_equality_trumps_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', oppressed_group_equality_trumps_religious_autonomy, deontological).
narrative_ontology:cs_reference_frame('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', constitutional_equality_primacy).
narrative_ontology:cs_drift_state('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', contemporary_post_decades_of_litigation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5e535d0a-35d4-4cd0-8f33-bbfbc8d4dfb5', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, religious_minorities_within_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_oppressed_by_tradition).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, community_autonomy_advocates).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, orthodox_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes_and_oppressed_subgroups).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_oppressed_by_religious_practice).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, secular_reform_advocates).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives_and_orthodox_practitioners).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_community_governance_bodies).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, constitutional_social_justice).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, equality_hierarchy_principle).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, state_protective_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically subjected to oppressive religious and caste practices within their own communities (untouchability, exclusion from temple access, forced occupational roles, sexual abuse normalized by tradition). Under this reading, the state treats their liberation from these practices as superseding the religious autonomy claims of their oppressors. They benefit from judicial recognition and state intervention against inherited oppression, though enforcement is incomplete and contested.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes_and_oppressed_subgroups, beneficiary,
    powerless, generational, trapped, national).

% Subject to practices justified by religious doctrine: denial of inheritance, forced marriage, polygamy, sexual coercion, exclusion from worship spaces, and restrictions on clothing and movement. This reading names state intervention against such practices as an affirmative duty, even when framed as core to religious identity. Benefits from legal remedies, though exit from oppressive communities remains structurally constrained.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_oppressed_by_religious_practice, beneficiary,
    moderate, generational, constrained, national).

% Operate within religious and cultural traditions that include practices now classified by courts as oppressive under the reformist reading. Bears the cost of state intervention: practices they regard as central to faith are restricted, communities lose autonomy over internal governance, practitioners face legal jeopardy. Their objections are that the state is subordinating their religious freedom to an external, majoritarian morality.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives_and_orthodox_practitioners, payer,
    organized, generational, identity_locked, national).

% Previously exercised quasi-sovereign authority over internal disciplinary and normative matters (family law, ritual purity, inheritance within the community). Under this reading, their authority is subordinated to constitutional principles of equality and dignity; courts override their decisions on matters they considered internal. They simultaneously administer community life (secondary agenda-setter role) while bearing the constraint's costs (payer role).
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_community_governance_bodies, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, religious_community_governance_bodies, agenda_setter).

% Interpret and enforce the reformist reading through case-by-case adjudication, determining which practices are 'oppressive' and when state intervention is justified. Set the operational boundaries of the constraint through doctrine development (e.g., which discriminations are 'core' to religion vs. 'incidental'). Their authority makes the constraint enforceable; their doctrine shapes what counts as a legitimate exception.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Civil-society and intellectual sectors that argue religious practices perpetuate oppression and state intervention is justified. They are net beneficiaries of the reading (it validates their frame and empowers legal reform), though they do not directly collect institutional rents — their benefit is ideological vindication and policy influence.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, secular_reform_advocates, beneficiary,
    powerful, generational, arbitrage, national).

% Minority-faith communities that fear the precedent of state judgment over 'oppressive' practices will be applied selectively — practices minority religions view as core will be scrutinized, while dominant-majority practices receive deference. They are structurally excluded from the beneficiary coalition framing but recognize the constraint as creating asymmetric risk for religious minorities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_minorities_navigating_majority_pressure, excluded,
    moderate, generational, identity_locked, national).

% Police, prosecution, social services, and regulatory bodies that execute state interventions against declared oppressive practices. They administer the constraint operationally: arrest practitioners of forbidden traditions, remove children from communities, prosecute religious leaders, enforce court orders. The reading imposes affirmative duties on state apparatus, expanding its role in religious life.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents coordinated oppression of subgroups within religious communities by subordinating community autonomy to constitutional principles of equality and dignity. Solves the problem that unregulated internal community governance perpetuates inherited oppression — the coordination problem is: how do we protect weaker members when the community itself is the oppressor? The reformist answer is state enforcement of equality principles overrides community self-governance.
% TRANSFER_FUNCTION: Moves authority and legitimacy from religious/community governance bodies to constitutional courts and state apparatus; transfers the cost of institutional change from oppressed subgroups (who previously bore oppression) to community practitioners and leaders (who must abandon or modify practices). Transfer is not monetary but institutional: loss of autonomy, exposure to legal jeopardy, reputational cost, reduced capacity to socialize new members into traditional practices.
% ABSENT_VOICES: Religious minorities fear selective application and are not at the negotiating table — the framework assumes good-faith, universal state enforcement, which minorities (especially non-dominant faiths) doubt. Practitioners whose cultures are targeted are not represented in the beneficiary framing. Secular religious skeptics who question whether state intervention can be non-coercive are not part of the coalition. International religious-freedom advocates often object but operate outside the national jurisdiction. Voices from within oppressed communities that value both liberation from some practices AND cultural continuity (not radical abandonment of religion) are marginalized by the binary framing.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, oppressed subgroups would lose court-backed protection against inherited practices; community governance bodies would regain de facto autonomy; practices now legally prohibited would likely resume; the institutional balance shifts decisively toward community self-governance. Conversely, if it fully persists as written, religious communities lose internal authority and become wholly subordinated to state-constitutional definitions of permissible belief and practice. The constraint's presence structures the entire institutional landscape of religious freedom vs. equality.
% FOUNDING_PROBLEM: Historical oppression of scheduled castes, women, and minorities by majoritarian-religious institutions and practices, perpetuated through community autonomy and legal pluralism. The founding problem is that religious freedom protections, interpreted as community autonomy, became shields for entrenched oppression — weaker members inside communities had no recourse because the law deferred to 'religious practice.' State intervention was needed to break the cycle.
% FOUNDING_PROBLEM_CORROBORATION: Oppressed groups and reform advocates testify the founding problem is live and ongoing. Religious conservatives counter that the 'problem' is being redefined by majoritarian standards imposed on minority practices; what was called oppression is being reframed as such by external judges. Constitutional courts split: some opinions treat the founding problem as fundamentally unresolved (ongoing caste discrimination, child marriage, denial of women's inheritance), others suggest legal reforms have substantially addressed the core harms and further intervention risks majoritarian overreach. International human-rights bodies (UN committees, International Labour Organization on forced labor in caste contexts) corroborate the founding problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.71) because the constraint transfers authority from communities to courts, costs compliance with state-defined morality, and subordinates practitioners' religious autonomy to external equality standards. The constraint is sustained by active enforcement — police action against practitioners, prosecution, removal of children, regulatory oversight — so suppression is substantial (0.68). Theater ratio is moderate (0.42) because there is a genuine coordination function (protecting oppressed subgroups from inherited oppression), but a growing share of enforcement activity defends the state's institutional power over religious life rather than protecting victims — as the constraint matures, the ratio of theater-to-function increases (measurements show rise from 0.25 to 0.42 across the interval). Suppression requirement also rises (0.48 to 0.68) because as practitioners resist and communities organize defensive structures, the state must intensify enforcement to maintain the constraint's operation. Accessibility_collapse is moderate (0.62) because religious conservatives retain some exit options (private practice, relocation to permissive jurisdictions, legal contestation), but the practical burden of non-compliance is steep. Resistance is high (0.74) because this reading meets sustained opposition from organized religious communities, constitutional skeptics, and international religious-freedom advocates. The one shared time grid ensures every metric is authored at the same six points (t=0,8,16,24,32,40), preventing the OQ-105 misalignment problem.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (courts, state apparatus) and the beneficiary seat (oppressed subgroups) should compute this as genuine coordination with external enforcement cost. The payer seats (religious conservatives, community leaders) should compute this as pure extraction with a coordination cover story — they do not choose to participate; they are compelled, and the 'protection' framing masks what they experience as cultural eradication. Same structural constraint, radically different computed types depending on seat. The engine derives directionality from the beneficiary/victim/exit declarations and computes per-seat classification; the authored claim of tangled_rope sits between the agenda-setter's rope-like beneficiary logic and the payer's snare-like victim logic. This gap is where the classification work happens.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled castes, women, and oppressed subgroups are declared beneficiaries: they receive protection without running the system (the courts and state run it), and they are net beneficiaries in the engine's measure (extraction flows FROM them by the formal claim is false — they are being extracted FROM by historical practice; the state intervention STOPS that extraction). Their directionality is low (d near 0.1-0.2 range: beneficiary direction). Religious conservatives and community leaders are declared victims: they pay the cost of lost autonomy, legal jeopardy, enforcement burden, reduced ability to transmit tradition. They are trapped or identity-locked (cannot exit without abandoning religious identity), so their directionality is high (d near 0.8-0.9: target direction). The engine will compute asymmetric extraction structure from these declarations — the constraint redistributes from religious autonomy toward state-enforced equality. Note: the structural data (beneficiary/victim + power/exit) does the work of directionality; the commentary does not compute d but explains WHY these groups occupy their declared roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of constitutional secularism is 'protect weaker members from oppression by ensuring equality and dignity under law.' The reformist reading operationalizes this as 'state must override religious autonomy when practices oppress.' After ~25-30 years of judicial development (mid-interval), the original mandate (liberation from caste oppression, protection of women) is substantially achieved in legal form — courts have prohibited untouchability, denied inheritance discrimination, criminalized child marriage, protected women's rights to divorce and property. Yet enforcement continues to intensify, and theater ratio rises: the constraint persists not primarily because the founding problem is live (though contested parties argue it is), but because state institutions and reform constituencies have interests in maintaining and expanding the constraint's scope. A mandatrophy candidate: the underlying coordination problem (protecting vulnerable members) has been substantially solved by the legal remedies, but the constraint persists and extracts (loss of community autonomy, criminalization of practitioners, cultural subordination) because the institutional apparatus and ideological commitment to reformist intervention create path-dependent expansion. This is not conclusive mandatrophy (the founding problem is genuinely contested — religious conservatives and minorities argue oppression is ongoing, courts argue cases are still being litigated), but the measurement series showing rising extraction and suppression requirement despite stable or declining reports of the founding problem's acute severity is a mandatrophy warning signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oppression_definition_contestation,
    'What counts as an ''oppressive'' religious practice that justifies state intervention? Is the definition universal, culturally contextual, or determined by the dominant majority''s morality?',
    'Track divergence in judicial opinions on borderline cases (dietary restrictions, clothing norms, ritual practices claimed to be identity-core vs. discriminatory in effect). Observe whether practices of dominant-majority religions receive same scrutiny as minority-faith practices. Comparative constitutional analysis across jurisdictions with different definitions.',
    'If ''oppression'' is defined primarily through majority secular standards, the constraint becomes a mechanism for cultural imperialism and majoritarian suppression of minority religions. If definition is narrow and empirically grounded in demonstrated harm to identifiable individuals, the constraint can operate as genuine protection. The ω is the irreducible ambiguity in the reading''s core concept.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oppression_definition_contestation, conceptual, 'Who defines ''oppression'' and by what standard?').

omega_variable(
    selective_enforcement_asymmetry,
    'Is state intervention applied equally across religious communities, or do dominant-majority religions receive de facto deference while minority religions face heightened scrutiny?',
    'Empirical audit: comparative case law analysis of prosecution rates and severity of penalties for identical practices across religious communities. Study enforcement patterns over time. Compare resource allocation for intervention against different traditions.',
    'If enforcement is systematically asymmetric, the constraint functions as a snare on religious minorities disguised as protection. If enforcement is genuinely universal, the constraint operates closer to its tangled_rope claim. Asymmetry would also trigger the committer axis question: is this reformist reading being applied as stated, or is it a disguise for majoritarian religious dominance?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_asymmetry, empirical, 'Does enforcement treat religious communities equally?').

omega_variable(
    exit_via_assimilation_or_genuine_exit,
    'Can practitioners exit the constraint by leaving the religious community while maintaining their religious belief, or does the constraint force assimilation into majoritarian secular norms as the only real exit?',
    'Observe post-exit trajectories of people who leave communities subject to state intervention: do they maintain religious practice in secular framework, or is continued practice suppressed by state enforcement outside community context? Track whether state intervenes in individual religious practice or only in community-governed practice.',
    'True exit (practice religion independently while exiting community oppression) suggests the constraint is genuinely protective. Forced assimilation (religious practice itself becomes illegal outside communities, or individuals report state hostility to continuing practice after exit) indicates the constraint is extractive toward religion itself, not just toward oppressive practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_via_assimilation_or_genuine_exit, empirical, 'Is exit available, or is assimilation forced?').

omega_variable(
    sibling_reading_framings,
    'This constraint is one reading of ''constitutional_secularism'' kernel. Two sibling readings instantiate different constraints: principled_intervention (state MAY intervene conditionally) and strict_neutrality (state must remain equal distance). What structural disagreement among these readings is irreducible, and what is empirical/political?',
    'Map reading divergences: (1) factual dispute about whether oppression is ongoing (empirical), (2) normative dispute about whether state intervention is justified even if oppression exists (philosophical), (3) institutional dispute about whether courts can execute intervention without majoritarian bias (structural). Identify which divergences would persist even if facts were settled.',
    'If divergences are primarily empirical, evidence and facts matter: establish founding problem status, enforcement asymmetry, exit patterns. If divergences are structural/normative (courts asked to resolve philosophical disagreements about religious freedom vs. equality), the constraint is inherently contestable. The ω names the committer axis: which aspects of the reading are contestable by design, and which are resolvable disputes?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framings, conceptual, 'Why do sibling readings exist as live alternatives rather than being foreclosed by evidence or logic?').

omega_variable(
    women_subgroup_agency_heterogeneity,
    'Among women oppressed by religious practice, what proportion genuinely support state intervention vs. experience it as external imposition? Is the beneficiary group homogeneous in its preference for the constraint?',
    'Ethnographic and survey data from women within communities subject to intervention. Track women''s own stated preferences for change vs. state-imposed change. Observe whether reform occurs through women''s internal organizing or external state action; whether women''s movements led intervention or followed it.',
    'If women''s own movements led the constraint''s development, it is more authentically beneficiary-aligned. If state intervention is imposed against the preferences of many women in affected communities (who prefer incremental internal reform or cultural continuity with selective change), the beneficiary framing becomes suspect and the constraint may extract from a complex heterogeneous group in the name of protecting a subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_subgroup_agency_heterogeneity, empirical, 'Do women benefit or experience the constraint as external imposition?').

omega_variable(
    committer_reading_decomposition,
    'Is ''constitutional secularism'' truly one kernel with multiple readings, or does the reformist reading represent such a different constitutional project (''secularism as state enforcement of equality'') that it is a different kernel from strict neutrality (''secularism as state non-interference'')? Could these be decomposed into separate kernels?',
    'Consult constitutional theory and judicial doctrine: do courts explicitly treating these as alternative readings of a single principle exist? Or do courts treating them as incompatible constitutional projects? Examine whether shared textual reference (constitutional provisions on ''secularism'' or ''free exercise'') unites the readings or divides them.',
    'If genuinely one kernel with live readings, the omegas and committer frame capture the structure. If actually two separate kernels (secular-state-as-neutral vs. secular-state-as-reformist), this story should be decomposed and the sibling-reading relations reconsidered. The uncertainty is whether ''constitutional secularism'' is a contested interpretation of a shared authority or a label for two different constitutive projects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_decomposition, conceptual, 'Is this one kernel or two?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_secularism__reformist_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_secularism__reformist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__reformist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_secularism__reformist_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(cons_tr_t32, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_secularism__reformist_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_secularism__reformist_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__reformist_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_secularism__reformist_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement_basis(cons_be_t32, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_secularism__reformist_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_secularism__reformist_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__reformist_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_secularism__reformist_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement_basis(cons_su_t32, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__reformist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, caste_discrimination_prohibition).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, womens_inheritance_rights_enforcement).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, child_marriage_criminalization).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'constitutional_secularism'. The sibling readings (strict_neutrality_reading, principled_intervention_reading) represent live alternative positions on how much state authority extends over religious life. All three instantiate different constraints from the same constitutional text because they disagree on the kernel's core meaning. They are linked as family members: each reading claims authority over the same institutional terrain (religious freedom regulation), but operationalizes it differently. The reformist reading is the most extractive toward religious autonomy; strict_neutrality is least extractive; principled_intervention is intermediate. The decomposition follows DP-001 ε-invariance: different readings produce different ε values because they authorize different state actions and impose different costs on religious communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__reformist_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
