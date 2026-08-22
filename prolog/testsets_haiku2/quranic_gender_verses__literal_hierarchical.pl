% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Quranic Gender Verses (4:11, 2:282, 4:34) — Literal Hierarchical Reading
 *   domain: legal/religious/gender
 *
 * SUMMARY:
 *   This constraint is the LITERAL HIERARCHICAL reading of the contested
 *   kernel formed by Quranic verses 4:11 (inheritance), 2:282 (testimony),
 *   and 4:34 (marital authority). Under this reading, the verses establish
 *   timeless divine legal ordinances establishing male guardianship,
 *   differentiated inheritance and testimony weights, and hierarchical family
 *   authority as divine mandate rather than historical accommodation. Male
 *   household heads and religious scholars/courts are the structural
 *   beneficiaries: they gain interpretive authority, resource control, and
 *   legal enforcement power. Women—particularly wives, daughters, and those
 *   designated disobedient—are the structural payers: they bear reduced
 *   inheritance, discounted testimony, constrained legal autonomy, and
 *   subordination to male guardianship with limited exit. The reading has
 *   high extractiveness because the constraint transfers durable resource and
 *   authority control from women to men via religious legitimation.
 *   Suppression is high because exit carries catastrophic costs (apostasy,
 *   family rupture, social death). The measurement series shows
 *   extractiveness and suppression remarkably stable across 1400 years, with
 *   theater_ratio rising over time as administrative enforcement became more
 *   bureaucratized and interpretive justifications more elaborate—a signal of
 *   institutional theatricality layering onto the core extraction. This
 *   stability is diagnostically important: the constraint shows no signs of
 *   wearing away under internal pressure; its persistence derives from
 *   continuous active enforcement and scholars' ongoing reaffirmation of the
 *   literal reading.
 *
 * KEY AGENTS:
 *   - male_household_heads (powerful): gain guardianship authority, resource control, double inheritance shares, unilateral divorce rights
 *   - religious_courts_and_scholars (institutional): enforce and interpret the verses; their authority depends on the literal reading's binding status
 *   - women_under_guardianship (moderate, identity_locked): bear legal subordination, constrained inheritance, reduced testimony, and identity fusion with familial role
 *   - female_heirs (powerless, trapped): automatically subject to half-share inheritance rule; cannot exit
 *   - disobedient_wives (powerless, identity_locked): subject to correction (ta'dib) mechanism; marriage exit is legally blocked in many contexts
 *   - contextual_egalitarian_interpreters (organized, excluded): would overturn the reading via historical contextualization; excluded from binding authority
 *   - progressive_abrogation_advocates (organized, excluded): would supersede these verses via naskh doctrine; excluded from enforcing jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.82).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.88).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Quranic Gender Verses (4:11, 2:282, 4:34) — Literal Hierarchical Reading").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "legal/religious/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '6febdf5c-32eb-4aa4-ab22-eda4a28d6780').
narrative_ontology:cs_kernel_codification('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', fixed_text).
narrative_ontology:cs_authority_grounding('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', lineage).
narrative_ontology:cs_interpretation_layer_present('6febdf5c-32eb-4aa4-ab22-eda4a28d6780').
narrative_ontology:cs_reading_relation('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', foundational, male_guardianship_timeless_ordinance).
narrative_ontology:cs_axiom_status(male_guardianship_timeless_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', male_guardianship_timeless_ordinance, deontological).
narrative_ontology:cs_axiom('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', secondary, gender_hierarchy_as_divine_mercy).
narrative_ontology:cs_axiom_status(gender_hierarchy_as_divine_mercy, holdable).
narrative_ontology:cs_axiom_grounding('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', gender_hierarchy_as_divine_mercy, theological).
narrative_ontology:cs_reference_frame('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', divine_legal_hierarchy_immutable).
narrative_ontology:cs_drift_state('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', contemporary_gender_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6febdf5c-32eb-4aa4-ab22-eda4a28d6780', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_courts_and_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_under_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, disobedient_wives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_seeking_exit).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, divine_ordinance_male_authority).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, structured_gender_hierarchy_as_mercy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive structurally enhanced legal authority under this reading: guardianship rights over wives (wilayah), double inheritance shares (in 4:11 context), right to unilateral divorce (talaq), and authority to mandate wifely obedience or discipline (in 4:34 reading). This authority is framed as divinely mandated responsibility, not privilege—guardians are obligated to provide maintenance (nafaqah) and just treatment. Their exit from the framework requires apostasy or repudiation of the reading itself.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    powerful, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter).

% Interpret and enforce the verses as binding legal ordinance. Scholars (ulama) derive fiqh (jurisprudential rules) from these verses; courts apply those rules to resolve disputes over inheritance, marriage, divorce, and women's legal capacity. Their institutional authority—their standing to declare law—is substantially reinforced by the claim that these verses contain timeless divine commands requiring no contextual reinterpretation. Departure from the literal reading would require scholarly consensus (ijma) to overturn established precedent, a high institutional bar.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_courts_and_scholars, agenda_setter,
    institutional, civilizational, mobile, universal).

% Subject to male guardianship (wilayah) in marriage, property, and legal capacity. Under this reading, a woman's testimony is weighted at half that of a man's in financial matters (2:282), her inheritance share is typically half that of an equivalently-positioned male heir (4:11), and her obedience to her husband is a Quranic obligation (4:34 as literally read). Exit from the arrangement requires either apostasy (which carries severe legal and social consequences including potential death penalty in some jurisdictions), family rupture (divorce initiated by the male, or a woman abandoning family and community), or geographic relocation to jurisdictions that do not enforce these rules. Identity is deeply fused with familial role and religious community.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_under_guardianship, payer,
    moderate, biographical, identity_locked, universal).

% Bear the direct financial cost of the 4:11 verse as literally interpreted: daughters receive half the share of sons in inheritance. They have no ability to negotiate or exit this rule; it applies automatically upon a male relative's death. They may not contest it without directly contradicting sacred text as their community understands it, which carries social and legal sanctions.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_heirs, payer,
    powerless, biographical, trapped, universal).

% Subject to the correction mechanism articulated in 4:34 as this reading interprets it: a husband may employ ta'dib (discipline) up to and including physical striking (darb) for manifest disobedience. The verse's language permits multiple interpretations (some scholars read 'darb' as symbolic or limited; others as permitting beating), but the literal hierarchical reading authorizes the husband's corrective authority without explicit constraint. Women face legal barriers to divorce (in some jurisdictions requiring husband consent or court proof of cause), limiting exit even from abusive situations. Identity is bound to marital status and family honor.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, disobedient_wives, payer,
    powerless, biographical, identity_locked, universal).

% Would argue these verses must be read within their 7th-century Arabian historical context and reinterpreted in light of overarching Quranic principles of equity (maqasid al-sharia). They are structurally excluded from binding interpretation authority in jurisdictions that enforce the literal hierarchical reading; their scholarly writings and fatwas (legal opinions) do not carry legal force in such jurisdictions. They would overturn or substantially reframe the constraint if they held institutional authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, contextual_egalitarian_interpreters, excluded,
    organized, generational, constrained, global).

% Would argue these verses represent an interim stage in Quranic revelation, superseded by later verses affirming universal human dignity and equality (e.g., 49:13). They are similarly excluded from binding interpretive authority; their reading does not prevail in jurisdictions enforcing the literal hierarchical interpretation. Legal displacement of these verses through acknowledged naskh would require institutional recognition of abrogation, which the literal reading resists.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, progressive_abrogation_advocates, excluded,
    organized, generational, constrained, global).

% Those who wish to exit the constraint (through geographical migration, apostasy, or explicit rejection of the reading) face extreme costs: loss of family ties, social ostracism, legal persecution in some jurisdictions (apostasy laws), or physical danger. Their voices are effectively silenced by these exit costs; they are structurally excluded from the conversation about the verses' meaning, even as they bear the constraint's costs most directly.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_seeking_exit, payer,
    powerless, biographical, trapped, universal).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, women_seeking_exit, excluded).

% Occupy an intermediate position: they may hold the literal hierarchical reading as doctrinally correct while advocating administrative or social reforms (e.g., women's education, property rights, divorce protections) that limit its harshest applications without formally abandoning the reading. Their position is institutionally precarious—they can influence implementation without overturning the text, but they do not bind binding interpretation authority in most Sunni-majority jurisdictions.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, reformist_jurists, observer,
    institutional, generational, mobile, global).

% Countries that have adopted civil law codes largely replacing or overriding Quranic inheritance and family law with egalitarian statutes. They do not enforce the constraint; women in these jurisdictions have equal inheritance and testimony rights. Their legal frameworks create de facto competition with the literal hierarchical reading, offering exit for those with geographic mobility.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, secular_legal_jurisdictions, observer,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, divinely-authorized family hierarchy within which men bear financial obligation (nafaqah) to women and children, and women's legal and economic interests are secured through male guardianship rather than autonomous legal agency. The arrangement aims to create clear authority and mutual obligation rather than competitive individual interests within the household.
% TRANSFER_FUNCTION: Transfers resource control and legal authority from women to male household heads and religious scholars: males gain double inheritance shares, guardianship rights, unilateral divorce authority, and interpretive authority over female obedience; women lose testamentary autonomy, full inheritance, autonomous legal standing, and exit rights. Religious institutions gain authority to interpret and enforce these rules, consolidating scholarly power over family law.
% ABSENT_VOICES: Women who reject the reading are structurally excluded (identity_locked exit); contextual and progressive egalitarian interpreters are excluded from binding authority in enforcing jurisdictions. Daughters have no voice in inheritance distribution; wives designated disobedient have no legal recourse to the 'correction' mechanism described in 4:34. Their objections, if voiced, are categorized as defiance of divine law rather than legitimate contestation.
% DISAPPEARANCE_RATIONALE: If the literal hierarchical reading were abandoned in favor of egalitarian reinterpretation (or secular law), women would gain autonomous legal capacity, equal inheritance, full testamentary weight, and grounds for unilateral divorce protection. Household authority would shift from hierarchical male guardianship to partnership or individual autonomy. Male household heads would lose guardianship prerogatives and resource-control authority; religious courts would lose interpretive monopoly. The entire family law structure would reorganize around equality rather than differentiated rights.
% FOUNDING_PROBLEM: Seventh-century Arabian society lacked unified legal frameworks for protecting women's property, inheritance, and economic security. Women's vulnerability in inheritance disputes, marital instability, and economic dependency created legal chaos. These verses established clear rules: male guardians obligate themselves to financial support; inheritance is distributed according to ratio (favoring those with greater financial obligation); wives' obedience is exchanged for husbands' maintenance and just treatment.
% FOUNDING_PROBLEM_CORROBORATION: Literalist scholars argue the founding problem—women's economic vulnerability and the need for clear authority and obligation—remains live and these verses provide the solution. Egalitarian interpreters and women's rights advocates argue the founding problem was specific to 7th-century context and has been superseded by modern legal frameworks (civil codes, women's employment, autonomous economic agency) that provide the same security without hierarchy. Historians document pre-Islamic Arabian women's precarious legal status and support the framing that the verses were progressive relative to surrounding practices. Contemporary gender-egalitarian scholarship documents that equal legal capacity and shared decision-making provide superior protection and outcomes for women.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.82 reflects the high and sustained transfer of resource/authority control from women to men. It is measured not as moral claim but as structural fact: women lose autonomous legal capacity (testimony weight), economic resources (inheritance distribution), marital autonomy (divorce initiation), and bodily autonomy (correction authority) to male household heads. Suppression at 0.88 is the highest metric because the constraint's persistence depends on preventing exit: apostasy laws, family shunning, and identity fusion with the community ensure women internalize the constraint's legitimacy or face annihilation of social existence. Theater_ratio rises from 0.25 to 0.42 over the interval: early period had direct enforcement (immediate custodial authority, household power); later periods elaborated complex justificatory frameworks (extensive tafsir commentary, jurisprudential refinement, scholarly consensus declarations) that perform the constraint's legitimacy while doing less direct work. This is a signal classic of piton transitions, except the core extraction does NOT decay—extractiveness stays flat—so it is institutional amplification of a persisting snare, not piton-drift. Accessibility_collapse at 0.79 reflects that once a woman accepts the reading's framework (which identity_lock makes nearly inevitable), alternatives disappear: she cannot simultaneously hold the reading as divinely true and reject her own subordination. Resistance at 0.71 reflects the substantial and organized resistance from egalitarian interpreters, secular jurisdictions, women's movements, and reformist jurists—but this resistance has not dislodged the literal reading in enforcing jurisdictions, indicating suppression is sufficient to hold the constraint despite real organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (male household heads, religious courts) and the payer seats (women, female heirs, disobedient wives) compute radically different classifications from identical structural data. From the beneficiary seat: this is a rope or scaffold—coordination that creates stable family structure and obligates men to financial support in exchange for authority and women's obedience. The reading emphasizes mutual obligation and complementary roles. From the payer seats: this is a snare—the coordination story masks a permanent transfer of authority and resources; exit is foreclosed by identity fusion and catastrophic cost; the obligation on men is enforced by honor and doctrine, while women's compliance is enforced by law and physical discipline. The engine computes directionality separately for each seat from exit_options and beneficiary/victim declarations; the divergence is not an error but the measurement the framework exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads: d ≈ 0.1 (full beneficiary)—they collect resource and authority, have arbitrage-level exit (can abandon the reading through geographic relocation or apostasy, but rarely do because it aligns with their interests and power), and sit at powerful institutional power levels. Religious scholars and courts: d ≈ 0.15 (beneficiary)—their institutional authority is substantially reinforced by the literal reading; they can exit by adopting alternative readings, but institutional incentives (career, legitimacy, precedent) make exit costly. Women under guardianship: d ≈ 0.92 (near-total target)—they are structurally extracted from (lose resources, autonomy, control), are identity_locked (exit means apostasy or family death), have moderate power atomically but are powerless against the institutional framework, and sit at every spatial scope (universal application means no geographic escape within most Muslim-majority regions). Female heirs: d ≈ 0.98 (near-total target)—they have no choice in the inheritance rule, no meaningful exit, trapped status. Disobedient wives: d ≈ 0.95 (near-total target)—subject to correction, legally barred from unilateral divorce in many jurisdictions, identity completely fused with marital role. These directionality differences explain why the engine would classify this as a tangled_rope from institutional beneficiary seats and a snare from payer seats—not a flaw in the framework but its diagnostic power.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows classic mandatrophy features: the founding problem (women's 7th-century economic vulnerability) is contested as dead or superseded (modern legal frameworks, women's independent economic agency, secular law jurisdictions); yet the constraint persists. The literal hierarchical reading explicitly RESISTS the reinterpretation move by claiming the verses are timeless ordinances immune to historical contextualization—a defensive closure against mandatrophy dissolution. The measurement series shows no collapse of extractiveness or suppression; if anything, theater_ratio rises as institutional enforcement becomes more elaborate. This is a constraint under active mandatrophy pressure but defended by interpretive doctrine: the literal reading's appeal lies partly in its timelessness claim (it prevents the 'founding problem is solved, so the constraint should dissolve' argument). Mandatrophy resolution would require either: (1) a catastrophic exit event (mass apostasy, geographic relocation, secular law displacement) that dissolves the constraint despite doctrine, or (2) a doctrinal shift within Islamic jurisprudence toward egalitarian reinterpretation or abrogation—which would constitute a move to a sibling reading (contextual_egalitarian or progressive_abrogation), not a resolution of this reading itself. This reading cannot resolve its mandatrophy internally; it can only resist it through interpretive closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_historical_semantics,
    'Are the verses 4:11, 2:282, 4:34 best understood as timeless legal ordinances establishing permanent hierarchical principles, or as historically situated responses to 7th-century conditions that carry forward principles (gender protection, family stability) via reinterpretation?',
    'Comparative jurisprudential analysis: survey which interpretation schools (madhab) explicitly claim timelessness vs. historical adaptation, examine their reasoning for each choice, and test against contemporary Islamic scholarship and reformist arguments. Establish whether the ''timeless'' claim is exegetically grounded or doctrinally assumed.',
    'If the verses are timeless: this reading holds and mandatrophy pressure is resisted; extractiveness and institutional authority remain stable. If the verses are historical: the contextual_egalitarian reading gains credibility; this reading''s structural authority weakens; extractiveness could decline under pressure to reinterpret.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literal_vs_historical_semantics, conceptual, 'Whether the literal hierarchical reading''s core premise—timelessness—is textually grounded or doctrinally assumed. This is the primary difference between this reading and its siblings.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression at 0.88 primarily structural (legal barriers, economic dependency, geographic immobility, enforcement machinery) or internalized (women internalize the hierarchy as legitimate, making suppression portable and persistent even post-exit)?',
    'Post-exit analysis: women who apostasy, migrate to secular jurisdictions, or explicitly leave the community—do they retain suppression beliefs/anxiety (internalized) or do they recover quickly (structural)? Longitudinal study of diaspora women, post-apostasy communities, and secular-law cohorts for suppression persistence.',
    'If internalized: the constraint''s effective suppression is higher than structural measures suggest; exit alone does not dissolve it; generations of resocialization are needed for freedom. If structural: alternative jurisdictions and apostate communities show rapid suppression decline, suggesting removal is feasible and exit is a realistic option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is held in place by external barriers or internalized beliefs; critical for understanding exit options.').

omega_variable(
    testimony_weight_extraction_vs_specialization,
    'Is the 2:282 half-testimony rule (a woman''s testimony weighs half a man''s in financial matters) a mechanism of extraction from women, or a specialization rule reflecting different roles and knowledge domains?',
    'Historical analysis of application: document how courts actually applied the rule—was it used to exclude women''s testimony entirely (extraction), applied mechanically to financial matters only (specialization), or extended to non-financial domains (scope-creep extraction)? Compare to complementary rules about male-exclusive testimony domains (if any exist).',
    'If extraction: the rule transfers epistemic authority from women to men and is a vector of control. If specialization: the rule reflects functional division and could coexist with egalitarian frameworks in other domains. Scope-creep evidence would indicate doctrine-drift toward maximized extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(testimony_weight_extraction_vs_specialization, empirical, 'Whether the testimony rule is a targeted mechanism of extraction or a specialized role assignment.').

omega_variable(
    darb_correction_permissibility_range,
    'Does 4:34''s authorization of ta''dib (discipline) for disobedient wives permit physical striking (darb), and if so, what are the permissible bounds?',
    'Jurisprudential textual analysis: survey authoritative tafsir (exegesis) and fiqh (jurisprudence) opinions across major schools; document the range of interpretations from ''darb is forbidden entirely'' to ''light striking without injury'' to ''striking permissible within bounds.'' Establish whether a scholarly consensus exists or opinions remain divided. Compare to contemporary reformist and egalitarian interpretations.',
    'If darb (striking) is permissible: the constraint includes an explicit mechanism of physical coercion, raising suppression and extraction measures. If darb is forbidden or metaphorical: the constraint is less coercively brutal, though guardianship and authority remain. Consensus vs. disagreement affects whether alternative readings have scholarly standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(darb_correction_permissibility_range, empirical, 'The permissibility and bounds of physical correction under 4:34; a critical ambiguity in the constraint''s operating range.').

omega_variable(
    kernel_reading_foreclosure_status,
    'Does the literal hierarchical reading logically foreclose the contextual_egalitarian and progressive_abrogation readings within a single coherent Islamic jurisprudential framework, or can multiple readings coexist as different scholarly positions?',
    'Jurisprudential architecture analysis: examine whether Islamic law (sharia) has formal mechanisms for adjudicating competing readings (ijma [consensus], qiyas [analogy], maslaha [public interest]) that would establish one reading as binding and others as false. If such mechanisms exist, determine whether they have been applied and whether any reading has achieved binding status. If not, assess whether coexistence is structurally possible.',
    'If foreclosed: this reading''s truth-claim excludes its siblings; the constraint operates under a doctrine that mandates exclusion of alternative readings. If coexisting: multiple readings are doctrinally live; institutional fragmentation into competing schools is the normal state; no single reading has universal binding authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_status, conceptual, 'The logical and jurisprudential relationship between this reading and its siblings; central to understanding the kernel''s contest structure.').

omega_variable(
    female_apostasy_exit_cost_irreducibility,
    'Is the apostasy exit cost (potential legal death penalty, family rupture, social death) a feature intrinsic to Islam or a feature of the enforcement regime in specific jurisdictions?',
    'Comparative jurisdictional analysis: survey apostasy law across Muslim-majority and Muslim-minority jurisdictions; establish which have formal apostasy penalties, which have informal enforcement, which have abolished the penalty. Separate Islamic doctrine (does classical fiqh mandate death for apostasy? Do modern scholars?) from state law implementation.',
    'If intrinsic to Islam: women''s exit cost is inherent to the reading; identity_locked status is a feature of the constraint itself. If jurisdictional: women in liberal-law or secular jurisdictions face lower exit costs; the same constraint operates with different suppression in different places; exit is more feasible than the universal reading suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(female_apostasy_exit_cost_irreducibility, empirical, 'Whether apostasy penalties derive from Islamic doctrine or political-legal enforcement; affects exit_options evaluation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t200, quranic_gender_verses__literal_hierarchical, theater_ratio, 200, 0.28).
narrative_ontology:measurement_basis(qura_tr_t200, observed).
narrative_ontology:measurement(qura_tr_t400, quranic_gender_verses__literal_hierarchical, theater_ratio, 400, 0.32).
narrative_ontology:measurement_basis(qura_tr_t400, observed).
narrative_ontology:measurement(qura_tr_t700, quranic_gender_verses__literal_hierarchical, theater_ratio, 700, 0.37).
narrative_ontology:measurement_basis(qura_tr_t700, observed).
narrative_ontology:measurement(qura_tr_t1000, quranic_gender_verses__literal_hierarchical, theater_ratio, 1000, 0.39).
narrative_ontology:measurement_basis(qura_tr_t1000, observed).
narrative_ontology:measurement(qura_tr_t1200, quranic_gender_verses__literal_hierarchical, theater_ratio, 1200, 0.4).
narrative_ontology:measurement_basis(qura_tr_t1200, observed).
narrative_ontology:measurement(qura_tr_t1400, quranic_gender_verses__literal_hierarchical, theater_ratio, 1400, 0.42).
narrative_ontology:measurement_basis(qura_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.82).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t200, quranic_gender_verses__literal_hierarchical, base_extractiveness, 200, 0.81).
narrative_ontology:measurement_basis(qura_be_t200, observed).
narrative_ontology:measurement(qura_be_t400, quranic_gender_verses__literal_hierarchical, base_extractiveness, 400, 0.8).
narrative_ontology:measurement_basis(qura_be_t400, observed).
narrative_ontology:measurement(qura_be_t700, quranic_gender_verses__literal_hierarchical, base_extractiveness, 700, 0.78).
narrative_ontology:measurement_basis(qura_be_t700, observed).
narrative_ontology:measurement(qura_be_t1000, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1000, 0.75).
narrative_ontology:measurement_basis(qura_be_t1000, observed).
narrative_ontology:measurement(qura_be_t1200, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1200, 0.72).
narrative_ontology:measurement_basis(qura_be_t1200, observed).
narrative_ontology:measurement(qura_be_t1400, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1400, 0.82).
narrative_ontology:measurement_basis(qura_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.88).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t200, quranic_gender_verses__literal_hierarchical, suppression_requirement, 200, 0.87).
narrative_ontology:measurement_basis(qura_su_t200, observed).
narrative_ontology:measurement(qura_su_t400, quranic_gender_verses__literal_hierarchical, suppression_requirement, 400, 0.86).
narrative_ontology:measurement_basis(qura_su_t400, observed).
narrative_ontology:measurement(qura_su_t700, quranic_gender_verses__literal_hierarchical, suppression_requirement, 700, 0.85).
narrative_ontology:measurement_basis(qura_su_t700, observed).
narrative_ontology:measurement(qura_su_t1000, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1000, 0.84).
narrative_ontology:measurement_basis(qura_su_t1000, observed).
narrative_ontology:measurement(qura_su_t1200, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1200, 0.83).
narrative_ontology:measurement_basis(qura_su_t1200, observed).
narrative_ontology:measurement(qura_su_t1400, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1400, 0.88).
narrative_ontology:measurement_basis(qura_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__literal_hierarchical, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% The constraint 'quranic_gender_verses' is a contested kernel with three distinct readings: this file (literal_hierarchical), contextual_egalitarian (sibling, separate file), and progressive_abrogation (sibling, separate file). All three readings interpret the same verses (4:11, 2:282, 4:34) but arrive at radically different structural classifications. The literal_hierarchical reading produces high extractiveness and suppression; the contextual_egalitarian reading reframes the same verses as historical accommodation requiring egalitarian reinterpretation, producing lower extractiveness; the progressive_abrogation reading positions these verses as textually superseded by later egalitarian principles, also producing lower extractiveness. Each reading is a separate constraint with its own ε, stakeholder structure, and type classification. The network.affects_constraints edges link them to document that they are siblings in the same kernel rather than independent constraints. The three readings coexist as competing scholarly positions in Islamic jurisprudence; no single reading has universal binding authority across all Muslim-majority jurisdictions, though the literal_hierarchical reading remains dominant in many institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__literal_hierarchical, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
