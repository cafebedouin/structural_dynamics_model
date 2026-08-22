% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority under Special Marriage Act 1954
 *   domain: law/constitutional/family
 *
 * SUMMARY:
 *   The Special Marriage Act 1954 grounds marriage authority in a secular
 *   civil code derived from constitutional individual rights (Articles 14,
 *   15, 21, 25–28 of the Indian Constitution) rather than religious law. This
 *   is ONE reading of the marriage authority kernel — competing readings
 *   ground the same domain of authority (who can marry, how marriage is
 *   dissolved, what property rights flow from it) in Hindu codified law,
 *   Muslim Shariat, Christian canon law, or Parsi communal custom. The
 *   secular civil reading instantiates the highest gender equity across the
 *   readings, enables inter-religious marriage without conversion, and treats
 *   marriage as a civil contract subject to state regulation. The cost is
 *   borne by religious community bodies (whose authority is narrowed) and by
 *   women trapped in community-law marriages by social identity lock (who
 *   cannot afford the exit cost even though it is legally available). The
 *   constraint is CLAIMED as Tangled Rope: it solves a genuine coordination
 *   problem (inter-religious marriage legality) AND it extracts from those it
 *   displaces (religious authorities, women identity-locked in community
 *   law). The measurement series shows extractiveness rising moderately from
 *   1954 (when the act was new and community law still dominated) to the
 *   present (as civil-act adoption increases and courts push back against
 *   discriminatory community practices), then plateauing as the new
 *   equilibrium stabilizes.
 *
 * KEY AGENTS:
 *   - Inter-religious couples — primary beneficiaries of legal marriage access without conversion or community approval
 *   - Civil courts — agenda-setters; adjudicate marriage validity and dissolution under the secular act
 *   - Religious community bodies (Hindu temple boards, mosque committees, church councils, Parsi assemblies) — primary payers; authority narrowed but not eliminated
 *   - Women in community-law marriages — paradox: formally have exit option via the act but socially cannot afford it (identity locked); bear the cost of the constraint's refusal to mandate uniformity
 *   - Constitutional state — agenda-setter; vindicates secular authority as the lawful framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.38).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.42).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority under Special Marriage Act 1954").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "law/constitutional/family").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '012268a1-7737-49bb-858e-7c8f787b9980').
narrative_ontology:cs_kernel_codification('012268a1-7737-49bb-858e-7c8f787b9980', formalized).
narrative_ontology:cs_authority_grounding('012268a1-7737-49bb-858e-7c8f787b9980', extraction).
narrative_ontology:cs_interpretation_layer_present('012268a1-7737-49bb-858e-7c8f787b9980').
narrative_ontology:cs_reading_relation('012268a1-7737-49bb-858e-7c8f787b9980', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('012268a1-7737-49bb-858e-7c8f787b9980', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('012268a1-7737-49bb-858e-7c8f787b9980', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('012268a1-7737-49bb-858e-7c8f787b9980', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('012268a1-7737-49bb-858e-7c8f787b9980', foundational, marriage_is_civil_contract_not_sacrament).
narrative_ontology:cs_axiom_status(marriage_is_civil_contract_not_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('012268a1-7737-49bb-858e-7c8f787b9980', marriage_is_civil_contract_not_sacrament, conventional).
narrative_ontology:cs_axiom('012268a1-7737-49bb-858e-7c8f787b9980', foundational, gender_equality_overrides_religious_tradition_in_marital_rights).
narrative_ontology:cs_axiom_status(gender_equality_overrides_religious_tradition_in_marital_rights, holdable).
narrative_ontology:cs_axiom_grounding('012268a1-7737-49bb-858e-7c8f787b9980', gender_equality_overrides_religious_tradition_in_marital_rights, deontological).
narrative_ontology:cs_axiom('012268a1-7737-49bb-858e-7c8f787b9980', secondary, individual_choice_to_exit_community_law_is_constitutional_right).
narrative_ontology:cs_axiom_status(individual_choice_to_exit_community_law_is_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('012268a1-7737-49bb-858e-7c8f787b9980', individual_choice_to_exit_community_law_is_constitutional_right, deontological).
narrative_ontology:cs_reference_frame('012268a1-7737-49bb-858e-7c8f787b9980', constitutional_individual_rights_supremacy).
narrative_ontology:cs_drift_state('012268a1-7737-49bb-858e-7c8f787b9980', contemporary_post_2000s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('012268a1-7737-49bb-858e-7c8f787b9980', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, gender_equity_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, civil_courts).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_community_bodies).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, women_exiting_community_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equal_dissolution).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, constitutional_state).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, women_in_community_law_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, orthodox_religious_interpreters).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_individual_rights_supremacy).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, secular_state_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Couples of different religious backgrounds can marry under the Special Marriage Act without converting or seeking community approval. They gain uniform civil marriage registration, transparent property rights in marital dissolution, and freedom to define their union without religious authority intervention. For them, the secular civil reading is the path to legal recognition of their chosen partnership.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, mobile, national).

% Women who marry under the Special Marriage Act gain equal property division on divorce, equal guardianship rights, and no gender-based disability in inheritance or contract (all theoretically true; enforcement varies by court). Compared to community-law frameworks where they face unequal property claims and custody presumptions favoring men, the secular act offers formal gender equality. The cost is choosing this path in the first place (often inter-religious, which entails community rupture).
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equal_dissolution, beneficiary,
    moderate, biographical, constrained, national).

% Civil courts adjudicate marriage validity, dissolution, and property rights under the Special Marriage Act 1954. They operate as the institutional translator of constitutional authority into family law practice. Courts have used this power to strike down discriminatory community-law practices (instant talaq, restitution of conjugal rights) even for those in community-law marriages, extending constitutional gender-equality norms beyond the civil act itself. They collect prestige, authority, and institutional power from administering the secular framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% The Union legislature and executive implement the Special Marriage Act as an expression of Article 44 (Directive Principle toward uniform civil code) and Parts III–IV (constitutional rights to equality and religious freedom, with reasonable limits). The state benefits from authority legitimated by constitutional law rather than by religious tradition. The state vindicates secular governance as the source of family law authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_state, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, constitutional_state, beneficiary).

% Hindu temple boards, Christian church councils, Muslim personal law boards, and Parsi community assemblies see their authority over marriage legitimation and dissolution narrowed where members opt for the civil act. They cannot prevent followers from marrying under the secular act; the act is legally optional for everyone. But the availability of the option weakens their de facto monopoly on marriage authority within their communities. They retain prestige and power where members stay within community law (still the majority), but they incur loss of exclusive authority and reduced ability to enforce conformity. Their organizational role is constrained by the secular framework's existence.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_community_bodies, payer,
    organized, generational, constrained, national).

% Women married under community law (Hindu, Muslim, Christian, Parsi) who face unequal property, custody, or divorce terms cannot switch regimes without exiting the marriage entirely and remarrying — an economically and socially prohibitive exit. Their identity as community members, their claim on kin property and old-age support, their social standing in family and extended kinship networks, and often their economic security are fused with community-law status. The Special Marriage Act offers a legal option they formally possess but practically cannot access without bearing catastrophic social and economic cost. Their situation illustrates the constraint's extraction: they cannot afford the legal path to equality offered by the secular reading, so they remain trapped in community law despite its legal availability as an alternative. This is the deepest cost of the constraint: it offers freedom that is not actually accessible to those most in need of it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_in_community_law_marriages, payer,
    powerless, biographical, identity_locked, national).

% Fundamentalist and orthodox interpreters of Hindu, Christian, Islamic, and Parsi law see the secular civil act as illegitimate (a violation of religious law) and as a threat to community authority and identity. They cannot prevent followers from marrying under the act, but they can (and do) enforce community exclusion of those who do. They are formally excluded from adjudicating disputes between civil-act couples and from legitimating civil marriages in their traditions. Their structural authority is delegitimated by the secular framework's claim that marriage is a civil contract, not a religious sacrament. They are trapped: they can resist and enforce exclusion, but they cannot prevent the secular alternative from existing.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, orthodox_religious_interpreters, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, orthodox_religious_interpreters, excluded).

% Feminist and gender-equity advocates argue that the pluralist coexistence of civil and community law is inadequate; they demand that the Special Marriage Act be extended into a UNIFORM CIVIL CODE that replaces community law entirely, making gender-equal property, custody, and dissolution terms mandatory for all citizens. They are excluded from the current inter-generational bargain (which accepts pluralism and treats the civil act as an option, not a mandate). They lobby legislatively and litigate for broader constitutional gender-equality protections, but they are structurally excluded from the authority to set the rules of the marriage authority kernel itself. Their voices are present in public discourse and court filings but are not seats at the table of state policy on religion and personal law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_demanding_uniform_civil_code, excluded,
    organized, biographical, constrained, national).

% Reform movements within Hindu, Muslim, Christian, and Parsi traditions (liberal interpreters, gender-egalitarian clerics, modern community leaders) argue for reinterpreting their own community law to align with gender equality and religious freedom. They are excluded from the formal authority structures that adjudicate their readings (courts recognize the conservative readings; religious authorities resist reinterpretation). They occupy a structural position between the secular civil framework and the orthodox community framework, advocating for modernization of community law without full exit to civil law. Their constraint is that they are heard but not empowered — their interpretive authority is contested within their own communities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_community_reform_movements, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, civil_courts).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Special Marriage Act 1954 solves the coordination problem of permitting legal marriage across religious boundaries without requiring conversion, community approval, or religious adjudication. It provides a uniform, secular, state-administered marriage registration and dissolution system that enables inter-community partnership and provides a single legal framework for property rights and inheritance.
% TRANSFER_FUNCTION: The constraint transfers authority from religious community bodies to civil courts for marriage validity, dissolution, and property adjudication. It also transfers (de jure) marriage opportunity from the jurisdiction of community law to the civil state — any citizen can now opt into the civil regime. The de facto transfer is constrained by social cost: choosing civil marriage incurs community exclusion, particularly for women, who lose kin support and social standing.
% ABSENT_VOICES: Women in community-law marriages who cannot afford the social cost of exit are excluded: they have no voice in the design of the framework they cannot legally access. Orthodox religious authorities are structurally excluded from adjudicating civil-act marriages; their voices are present in resistance discourse but not in the authority structure. Persons seeking to marry within their birth community law (the default option for those not inter-religious) are not excluded — they can still choose community law — but the civil act's availability as an alternative means their choice to stay is now made explicit rather than default, which shifts the social dynamics of authority.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act disappeared, inter-religious couples would be forced back to either converting to the partner's religion (or seeking community exemption, where available), to cohabiting outside law, or to accepting that their marriage is not legally recognized. The civil courts' role in marriage adjudication would shrink to community-law disputes. Women unable to exit community law would lose the option (however costly) of legal remedy outside community authority. Community bodies would regain de facto monopoly on marriage legitimation within their boundaries.
% FOUNDING_PROBLEM: The founding problem was the structural illegality and constitutional incoherence of marriage across religious communities in a post-independence secular state. Before 1954, Hindu-Muslim marriage, Hindu-Christian marriage, and other inter-religious unions had no legal recognition under Indian law; they existed in a legal void. The Special Marriage Act created a legal path for inter-religious marriage and established secular civil courts as the authority for all such unions.
% FOUNDING_PROBLEM_CORROBORATION: The civil state and constitutional courts attest the founding problem was genuine and remains live: inter-religious couples continue to seek marriage outside community law, and the civil option remains necessary for their legal recognition. However, the problem's scope is contested: religious authorities and orthodox interpreters argue the founding problem was never binding on their communities (that secular law has no authority over community marriage), so the Special Marriage Act is not a solution to a real problem but an imposition. Gender-equity advocates attest the founding problem extends beyond inter-religious marriage to unequal property and custody rights within community law — a problem the civil act only partially solves (those trapped by social cost cannot access it).
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint genuinely solves the inter-religious coordination problem while simultaneously displacing religious authority. The displacement is not coercive in the narrow sense (no one is forced to marry under the act) but is structurally coercive for those exiting community law (social exclusion enforced by kin and community). Suppression (0.42) models the social cost of choosing the secular path: family pressure, community ostracism, economic vulnerability (particularly for women), loss of inheritance under community law. Theater (0.28) is moderate-low: the civil courts genuinely adjudicate real disputes, but part of the institutional work (defending secular authority against religious claims) is performative legitimation rather than problem-solving. The measurement series rises in extractiveness and suppression through the 1960s–1980s as civil-act adoption increased and as courts began invalidating discriminatory community-law practices (e.g., instant talaq, restitution of conjugal rights). The series plateaus after the 2000s as a new equilibrium emerges: community law still predominates for those within religious traditions, civil law is now expected for inter-religious couples, and courts have established settled precedent on constitutionality.
 *
 * PERSPECTIVAL GAP:
 *   From the civil-court and state seat, the constraint is coordinate: it solved a real problem (legalizing inter-religious marriage) and operates legitimately under constitutional authority. From the religious community seat, the constraint is extractive: it narrows their authority to define marriage and legitimation within their tradition, and it does so by force of state law they never accepted. From the woman trapped in community law by social cost, the constraint is a cruel mockery of access: it offers legal exit but makes the social cost of taking it prohibitive. The engine computes these divergent directionalities from the power/exit/beneficiary data: the civil-court and state seats derive d near beneficiary (they set the rules, they collect authority); the religious-body seats derive d near target (their authority shrinks, their cost is loss of jurisdiction); women in community law derive high d (they cannot afford the exit option) but with a critical difference — their exit_options are identity_locked, which modulates their effective extraction differently than a straightforward trapped agent.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil courts and the constitutional state are beneficiaries (d ≈ 0.2): they gain authority, institutional prestige, and the power to define marriage legitimacy. Inter-religious couples are beneficiaries (d ≈ 0.3): they gain legal access to marriage without conversion. Religious community bodies are payers (d ≈ 0.75): their authority is narrowed, their membership control is weakened where followers opt for civil marriage, and they incur cost in the form of institutional prestige loss and reduced capacity to enforce community marriage norms. Women in community-law marriages who cannot exit (d ≈ 0.85) bear the highest cost: the constraint's existence as a legal option they cannot afford makes their position worse off than if the constraint did not exist (they would be trapped by default; now they are trapped by choice, a form of internalized suppression). Women who DO exit (d ≈ 0.4) are net beneficiaries despite the social cost because they gain property and custody rights superior to community law.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic in the classical sense (founding problem dead, arrangement persists as theater). The founding problem (inter-religious marriage legality) remains live and contested. However, there is a secondary mandatrophy: the constraint was also implicitly justified as a step toward uniform civil code (Article 44, constitutional aspiration), which would make all community law frameworks obsolete. This secondary mandate has not been pursued (political barriers from coalition government, religious minority protection concerns), so the constraint persists as compromise (coexistence of civil and community law) rather than as progress toward the original constitutional goal. This is not theater but rather incomplete transition — the constraint remains functional, but its framing as temporary or progressive has been abandoned. The feminist mandate (uniform civil code as necessary for gender equality) is also live and contested: gender-equity advocates argue the constraint should expand to replace community law, not coexist with it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_foreclosure_possibility,
    'Does the secular civil reading logically foreclose the religious readings (all five of them), or do they coexist as live alternatives held by different parties?',
    'Examine whether a single Indian citizen or household can simultaneously honor both their religious reading of marriage authority AND the secular civil reading — e.g., can a woman who marries under the Special Marriage Act still be considered married under Hindu law by her community? If yes, they coexist (not foreclosed); if no, test whether the foreclosure is logical (one premise directly contradicts the other) or merely institutional (the state enforces exclusivity). The legal fact: a woman can marry under the Special Marriage Act without converting, so her marriage is valid under civil law but invalid under Hindu law, if that law requires her birth-community status. This is coexistence-with-cost, not logical foreclosure.',
    'If the readings logically foreclose each other, the secular reading is a competitor that should displace the others (mandatrophy of religious readings); if they coexist, the secular reading is one option among several, and the constraint''s type should account for incomplete dominance. Currently modeled as coexists_with (the engine may later compute foreclosure if the readings'' foundational axioms prove contradictory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_foreclosure_possibility, conceptual, 'Whether the secular civil and religious readings logically contradict or coexist as live positions.').

omega_variable(
    social_cost_source_structural_vs_cultural,
    'Is the high social cost of exiting community law a structural feature of the secular civil reading itself (the reading requires severance from community authority), or is it a cultural fact about Indian kinship that would obtain under any reading?',
    'Compare exit costs across readings: test whether women exiting within Hindu law (say, via divorce under the Hindu Marriage Act and remarriage to a Hindu) face similar kinship-severance costs. If yes, the high cost is cultural (kinship structure, not the reading). If no, the secular reading uniquely imposes severance because it severs from the community law framework itself, not just from a marriage relationship.',
    'If structural to the secular reading, the extraction cost is intrinsic to how the reading operates (it requires exit from community authority structures); if cultural, the extraction cost is a side effect of Indian kinship but not generated by the reading''s logic. This affects whether the suppression metric (0.42) represents the reading''s design or the reading''s operation in a specific cultural context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_source_structural_vs_cultural, empirical, 'Whether exit-cost is built into the secular reading or is a contingent feature of Indian kinship.').

omega_variable(
    gender_equity_axiom_overridden_status,
    'Is the gender-equity axiom of the secular civil reading truly ''holdable'' in contemporary Indian law, or has it been partially overridden by constitutional courts'' reluctance to strike down discriminatory community-law provisions that are technically optional (not mandatory)?',
    'Examine Supreme Court judgments: where courts have struck down instant talaq and restitution of conjugal rights as unconstitutional (strong gender protection) versus where courts have upheld community-law property division as valid for those who opt into community law (weak enforcement of civil-law gender equity across readings). If courts actively strike down discriminatory practices even in community law, the axiom is holdable (gender equity is enforced); if courts defer to community law practitioners'' choice, the axiom is overridden in practice (gender equity applies only to civil-act marriages, not to most marriages in India).',
    'If overridden, the secular civil reading''s foundational claim (gender equity as a legal right) applies only to the subset of citizens who can afford to exit community law, making the reading''s gender-equity axiom partially foreclosed in practice. If holdable, the reading maintains its foundational commitment even as it coexists with other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equity_axiom_overridden_status, empirical, 'Whether the secular reading''s gender-equity foundation is actively enforced or limited to civil-act cases.').

omega_variable(
    identity_lock_mechanism_internalized_vs_structural,
    'For women identity-locked in community-law marriages, is the suppression of exit internalized (belief that they belong to the community, fear of autonomy, fusion of self with community role) or structural (economic dependence on kin, formal disability of property claims outside community law, geographic isolation)?',
    'Post-exit trajectory: if women who leave community law and remarry under the Special Marriage Act report persistent identity-lock symptoms (guilt, sense of displacement, psychological identity crisis) despite structural barriers being removed, internalization is significant. If they report rapid adaptation and no psychological residue despite structural barriers being high, the lock is primarily structural. The evidence base: interviews with women who have made the transition (rare, but documented in sociological literature).',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (0.42) suggests, and decoupling the exit option from the family/community cost will not alone free trapped women. If structural, fixing the constraint requires lowering the economic and kinship cost of exit, which the civil framework alone cannot do (requires kinship norm change or economic policy). This informs omega_C conceptual design for the reading: what would count as successfully operationalizing the secular reading''s promise?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized_vs_structural, empirical, 'Whether identity lock is internalized psychology or structural kinship/economic fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__secular_civil_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__secular_civil_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__secular_civil_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__secular_civil_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(marr_tr_t40, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__secular_civil_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(marr_tr_t50, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__secular_civil_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(marr_tr_t60, observed).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__secular_civil_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement_basis(marr_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(marr_be_t40, observed).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement_basis(marr_be_t50, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(marr_be_t60, observed).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 70, 0.38).
narrative_ontology:measurement_basis(marr_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(marr_su_t40, observed).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement_basis(marr_su_t50, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(marr_su_t60, observed).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 70, 0.42).
narrative_ontology:measurement_basis(marr_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__secular_civil_reading, 0.18).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage authority kernel. The kernel is contested across five structurally distinct constraint stories (secular_civil_reading, hindu_codified_reading, muslim_shariat_reading, christian_canonical_reading, parsi_communal_reading). Each reading grounds marriage authority in a different framework (constitutional rights, Hindu scripture, Shariat, Christian canon, Parsi custom) and instantiates different ε, beneficiary/victim structures, and type classifications. The readings coexist in Indian law as a pluralist framework where citizens can choose which reading applies to them (with varying social and economic costs). Each story should be authored independently with its own ε-invariant referent (the standing arrangement under that reading, not the hypothetical alternate readings); network links register the sibling relationships without forcing them into a single classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__secular_civil_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
