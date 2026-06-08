% ============================================================================
% CONSTRAINT STORY: muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_muslim_shariat_reading, []).

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
 *   constraint_id: muslim_shariat_reading
 *   human_readable: Muslim Personal Law Authority via Shariat Interpretation
 *   domain: constitutional_pluralism/religious_governance/family_law
 *
 * SUMMARY:
 *   This constraint describes the institutional arrangement in which
 *   marriage, divorce, and inheritance for Muslim citizens in India,
 *   Pakistan, Bangladesh, and other post-colonial states derive authority
 *   from Shariat as interpreted by Muslim personal law boards, qazis (Islamic
 *   judges), and increasingly, organized ulema (Islamic scholars). The
 *   arrangement emerged from colonial-era recognition of 'personal law'
 *   jurisdictions — the British Raj's accommodation of religious community
 *   governance within the broader imperial administrative structure.
 *   Post-independence, states constitutionalized this arrangement, creating a
 *   plural legal system where marriage law varies by religious community:
 *   Hindu Succession Act (codified, secular), Christian personal law
 *   (canonical), Parsi Communal Matrimonial Court (communal), and Muslim
 *   Personal Law (Shariat-based, adjudicated by qazis and boards). This
 *   reading instantiates the constraint from the position of religious
 *   authority preservation: the commitment is that Muslim communities should
 *   govern their own marriage, divorce, and inheritance law according to
 *   classical Islamic jurisprudence as interpreted by qualified Islamic
 *   authorities. The constraint exhibits tangled-rope structure: genuine
 *   coordination function (preserving community autonomy and cultural
 *   continuity) paired with asymmetric extraction (gender-based rights
 *   asymmetry, women's unilateral divorce restrictions, polygamy, inheritance
 *   disparities). Suppression has increased post-independence as the state
 *   has formalized parallel jurisdiction and religious authorities have
 *   hardened orthodoxy in response to secularization pressure. Theater ratio
 *   has risen as both religious authorities and women's rights advocates
 *   within the tradition engage in performative appeals to 'true Islam' while
 *   actual judicial practice evolves.
 *
 * KEY AGENTS:
 *   - Muslim Personal Law Boards: Institutional authority (institutional/arbitrage) — benefit from exclusive adjudicatory power; experience constraint as coordination of community governance
 *   - Qazis (Islamic Judges): Institutional authority (institutional/constrained) — exercise judicial power over marriage/divorce but face increasing pressure from state courts and internal reform movements
 *   - Women in Muslim Marriages: Primary victims (powerless/identity_locked) — identity-fused with Islamic womanhood and familial role; unilateral talaq, polygamy, inheritance asymmetry concentrated on this group
 *   - Muslim Women's Rights Advocates: Organized reform agents (organized/constrained) — seek gender-equitable reinterpretation within Islamic jurisprudence; constrained by need to maintain religious legitimacy
 *   - State Judicial System: Powerful institutional actor (powerful/mobile) — tolerates parallel jurisdiction politically but retains formal jurisdictional claim; high cost of enforcement
 *   - Classical Islamic Jurisprudential Tradition: Authority structure (institutional/analytical) — invoked as immutable source but actually sites of ongoing reinterpretation masked as transmission
 *   - Religious Minorities within Muslim Communities: Secondary victims (powerless/constrained) — Shia, Ahmadi, and other minorities subject to majoritarian Sunni interpretation enforced through qazi courts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(muslim_shariat_reading, 0.58).
domain_priors:suppression_score(muslim_shariat_reading, 0.62).
domain_priors:theater_ratio(muslim_shariat_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(muslim_shariat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(muslim_shariat_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(muslim_shariat_reading, "Muslim Personal Law Authority via Shariat Interpretation").
narrative_ontology:topic_domain(muslim_shariat_reading, "constitutional_pluralism/religious_governance/family_law").

domain_priors:requires_active_enforcement(muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(muslim_shariat_reading, '406a48fa-c7b2-4a7b-bd7e-3b90674f73b2').
narrative_ontology:cs_kernel_codification('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', formalized).
narrative_ontology:cs_authority_grounding('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', lineage).
narrative_ontology:cs_interpretation_layer_present('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2').
narrative_ontology:cs_reading_relation('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', muslim_shariat_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', muslim_shariat_reading__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', muslim_shariat_reading__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', muslim_shariat_reading__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', foundational, religious_community_autonomy_in_personal_law).
narrative_ontology:cs_axiom_status(religious_community_autonomy_in_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', religious_community_autonomy_in_personal_law, deontological).
narrative_ontology:cs_axiom('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', foundational, classical_shariat_immutability_and_authority).
narrative_ontology:cs_axiom_status(classical_shariat_immutability_and_authority, overridden).
narrative_ontology:cs_axiom_grounding('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', classical_shariat_immutability_and_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', classical_islamic_jurisprudence_as_community_authority).
narrative_ontology:cs_drift_state('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', contemporary_post_colonial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('406a48fa-c7b2-4a7b-bd7e-3b90674f73b2', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(muslim_shariat_reading, religious_authority_bodies).
narrative_ontology:constraint_beneficiary(muslim_shariat_reading, patriarchal_family_structures).
narrative_ontology:constraint_victim(muslim_shariat_reading, women_in_muslim_marriages).
narrative_ontology:constraint_victim(muslim_shariat_reading, religious_minorities_within_muslim_communities).
narrative_ontology:constraint_victim(muslim_shariat_reading, secular_law_jurisdiction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN IN MUSLIM MARRIAGES (SNARE) — Identity fused with familial and religious role; unilateral talaq, polygamy restrictions, inheritance asymmetry are experienced as structurally unchangeable from within the identity frame. Materially mobile (could relocate, seek civil courts) but cognitively trapped by religious identity, community membership, and the binding narrative that submission is religious duty. Exit requires abandoning not just the marriage but the identity constituted through Islamic womanhood as locally practiced. High experienced extraction — the constraint concentrates costs (talaq, polygamy, inheritance) on this agent with no structural remedy.
constraint_indexing:constraint_classification(muslim_shariat_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CHILDREN AND DEPENDENTS (SNARE) — Structurally trapped by custody rules, inheritance provisions, and economic dependency. No exit options — minors cannot remove themselves from jurisdictional boundaries or parental authority. Bears extraction through inheritance disadvantage and custody insecurity. Materially powerless; no advocates with standing in the qaazi court system.
constraint_indexing:constraint_classification(muslim_shariat_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MUSLIM PERSONAL LAW BOARDS AND QAZIS (ROPE) — Institutional beneficiary experiencing this constraint as coordination of Islamic community governance. Sees the constraint as solving the legitimate problem of preserving religious community autonomy and classical jurisprudential continuity. Arbitrage exit option (can invoke secular courts as fallback, though politically costly). Net beneficiary — the constraint grants adjudicatory authority, community legitimacy, and enforcement power over marriage, divorce, and inheritance matters. Experiences extraction as minimal; the constraint is experienced as functionally necessary for community coherence.
constraint_indexing:constraint_classification(muslim_shariat_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: MUSLIM WOMEN'S RIGHTS ADVOCACY (TANGLED ROPE) — Organized agents seeking reform within Islamic jurisprudential tradition. Experience the constraint as both coordinating (preserving community autonomy) and extractive (perpetuating gender asymmetries). Constrained by religious tradition (must frame reforms as reinterpretation, not rejection) and institutional power of orthodox boards. Benefit from preservation of Muslim-specific law (avoid assimilation into secular codification) while bearing costs of defending rights within restricted interpretive boundaries. High theater ratio — much effort goes to demonstrating that reform is 'truly Islamic' rather than to direct advocacy.
constraint_indexing:constraint_classification(muslim_shariat_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE JUDICIAL APPARATUS (TANGLED ROPE) — Powerful actor with mobile exit (can reclaim jurisdiction). Experiences the constraint as both coordinating (manages religious minorities without state capacity to adjudicate unfamiliar law) and extractive (loses jurisdictional control, political legitimacy, and equal-protection claims). Can appeal to secular constitutional principles or invoke Directive Principles but faces political cost of confronting religious authority. High suppression requirement — maintaining parallel jurisdiction requires active negotiation and occasional coercive enforcement (Shariat courts are de facto, not de jure; state tolerates them politically, not constitutionally).
constraint_indexing:constraint_classification(muslim_shariat_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CLASSICAL ISLAMIC JURISPRUDENTIAL AUTHORITY (PITON) — Authority structure invokes immutability of classical Shariat as fixed text and interpreters as transmitters of continuous tradition. In reality, reinterpretation happens constantly (tafsir, qiyas, ijma) but is theatrically masked as 'faithful transmission.' The constraint's claim to be unchanging Islamic law masks substantial ongoing interpretation. Theater ratio is moderate (0.48) because some genuine fidelity to jurisprudential methodology exists alongside performative appeals to tradition. Piton classification derives from the gap between declared immutability and actual adaptive reinterpretation — the authority structure maintains its legitimacy through the performance of unchangingness rather than through demonstration that the interpretations are optimal.
constraint_indexing:constraint_classification(muslim_shariat_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing perspective, religious community autonomy is treated as a foundational principle of pluralism and self-determination — the constraint emerges naturally from the commitment to allow communities to govern their internal affairs. From this view, the constraint's persistence follows from logical necessity: either the state monopolizes marriage law (violating pluralism) or communities govern it (this constraint). However, structural analysis reveals this as a false summit: the constraint's naturalness claim depends on which community interests are centered (religious authority preservation vs. gender equity) and which institutional alternatives are foreclosed (judicial review of religious adjudication is technically possible; its absence is political choice). The mountain classification naturalizes a contestable institutional choice.
constraint_indexing:constraint_classification(muslim_shariat_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(muslim_shariat_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(muslim_shariat_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(muslim_shariat_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(muslim_shariat_reading, TR),
    TR >= 0.70.

:- end_tests(muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The constraint concentrates marriage/divorce/inheritance authority in a system that exhibits clear gender asymmetries (unilateral talaq by husband, polygamy restrictions on wives, inheritance gender-weighted). The original extractiveness at the colonial period (0.42) was lower because Shariat courts operated informally with limited enforcement power. Post-independence formalization (time 25, extractiveness 0.58) and subsequent entrenchment (contemporary, 0.61) increased extraction as the state legally recognized and provided enforcement machinery for qazi courts. The extraction targets women and religious minorities within Muslim communities while benefiting religious authority bodies and patriarchal family structures. Suppression (0.62): Moderate-high and rising. Structural barriers to exit include: (1) community and family pressure to remain within Islamic marriage framework, (2) inadequate civil law alternatives (many women unaware secular courts can adjudicate marriage; others face family violence or economic coercion if they pursue secular routes), (3) custody rules that disadvantage women in secular proceedings (qazi courts sometimes award custody more leniently), (4) religious identity-lock making exit feel like apostasy. Suppression increased post-independence as the state formally recognized qazi courts, making parallel jurisdiction a stable rather than informal arrangement. Theater ratio (0.51): Moderate. The constraint contains genuine coordination function (preserving community autonomy, maintaining cultural continuity in family law) alongside performance. The theater increased from colonial period (0.35) because appeals to 'true Islam' and 'classical jurisprudence' became more visible as reinterpretation accelerated. Contemporary theater (0.51) reflects that much debate focuses on demonstrating 'Islamic authenticity' of reform proposals rather than on direct advocacy for change.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives produce divergent classifications from identical base properties. The gap reveals that 'the constraint' is experienced as coordination (Rope) from authority perspective, extraction (Snare) from victim perspective, and mixed (Tangled Rope) from state and reform perspectives. This gap is NOT a measurement error; it is the primary empirical signal. The gap exists because the constraint genuinely has both coordination function (community autonomy preservation) and extraction mechanism (gender-asymmetric authority), and different perspectives experience different proportions of each. The gap also reveals that some classifications depend on perspective-level facts: women's identity-lock status is structural (internal binding) not just material; this is why they classify as Snare with powerless/identity_locked while other trapped agents might classify as Mountain. The analytical observer's mountain classification reveals how pluralist principles ('communities should govern themselves') can naturalize contestable institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's effective extraction (χ) is derived from base extractiveness (ε = 0.58), their directionality (d) in the constraint, and the constraint's scope. Religious authorities: d ≈ 0.10 (institutional power, arbitrage exit, beneficiary status) → χ ≈ negative (net subsidy; they collect from the constraint). Women: d ≈ 0.95 (powerless, identity_locked exit, victim status) → χ ≈ very high (maximum extraction; constrained by both identity fusion and material barriers). State: d ≈ 0.60 (powerful but politically constrained in exercising power; mobile exit but high cost) → χ ≈ moderate. Reform advocates: d ≈ 0.55 (organized, constrained exit, mixed beneficiary/victim status) → χ ≈ moderate. The directionality derivation also reveals why identity_locked produces a different classification than pure constrained: a constrained agent at biographical time horizon might see the constraint as unchangeable (mountain); an identity_locked agent at the same time horizon might see it as changeable in principle (rope), because their binding mechanism is internal (identity frame) not external (material barriers). This perceptual difference is diagnostic — it reveals that the constraint is changeable, just not from within the identity frame that constitutes the agent.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandatrophy is not resolved. The foundational mandate ('preserve Muslim community autonomy in classical Shariat framework') remains asserted but increasingly hollow. The state's constitutional equal-protection mandate collides with the community-autonomy mandate. The classical jurisprudential authority's mandate ('transmit unchanging Shariat') collides with the reality of ongoing adaptive reinterpretation. This is unresolved mandatrophy: the constraint persists because the state has no political will to assert its equal-protection mandate against religious authority, not because the community-autonomy mandate is still live. The constraint's persistence is increasingly driven by institutional inertia (plural jurisdiction is entrenched) rather than mandate alignment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shariat_immutability_claim,
    'Is the constraint''s appeal to classical Shariat as fixed, unchanging law descriptively accurate, or does it mask ongoing reinterpretation and evolution?',
    'Diachronic analysis of qaazi rulings and board interpretations over 50+ years; identification of doctrinal shifts masked as ''restatement of tradition''; comparison of actual precedent patterns with authority''s claims of unchanging jurisprudence.',
    'If immutable: classical Shariat is the constraint''s true ground (mountain, mountain). If evolving: reinterpretation is ongoing but theatrically masked (piton confirmed, false summit confirmed). If selectively immutable (preserve patriarchy, evolve other domains): reveals that immutability is a strategy to protect certain beneficiaries, not a neutral principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shariat_immutability_claim, empirical, 'Whether classical Shariat is truly immutable or masks ongoing reinterpretation').

omega_variable(
    community_autonomy_vs_gender_equity_foreclosure,
    'Does the principle of religious community autonomy logically foreclose gender-equitable interpretations of Islamic law, or do they coexist as live positions within the tradition?',
    'Survey of contemporary Islamic jurisprudence identifying gender-egalitarian reinterpretations (progressive tafsir, feminist ijtihad); assessment of whether these are held as live positions by credentialed Islamic scholars or marginalized as inauthentic.',
    'If foreclose: the kernel''s core premises are incompatible (reading conflict is real, not resolvable). If coexist: multiple readings remain live, and state/judicial pressure can shift which reading becomes institutional default (the reading conflict is real but not logically determined).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_autonomy_vs_gender_equity_foreclosure, conceptual, 'Whether autonomy and gender equity are logically foreclosed or coexistable within Islamic jurisprudence').

omega_variable(
    identity_lock_vs_constrained_exit,
    'To what extent do women in Muslim marriages experiencing this constraint face material barriers (trapped/constrained) versus identity-fusion barriers (identity_locked) to exit?',
    'Qualitative data: interviews with women who have exited or remained; analysis of reported reasons for staying (fear of family violence, economic dependency, loss of custody vs. loss of religious identity, community expulsion, self-conception as Islamic wife); post-exit suppression trajectory (does external suppression persist after exit or was it internalized).',
    'If primarily material: constraint is externally imposed (snare classification stable). If primarily identity-fusion: constraint is internally maintained even when material barriers are removed (reclassification to rope at generational horizon where identity shifts occur). Mixed findings indicate that suppression has both structural and internalized components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Material vs. identity-based barriers to exit for women in Muslim marriages').

omega_variable(
    plural_jurisdiction_sustainability,
    'Is the state''s tolerance of parallel Shariat courts sustainable as a stable institutional arrangement, or does it contain inherent instability that drives toward either state monopoly or religious authority monopoly?',
    'Comparative analysis of jurisdictional conflict cases; trends in judicial review of religious court decisions; political pressure trajectories (increasing state reassertion vs. increasing religious authority entrenchment); cross-national patterns in multi-law systems.',
    'If unstable: the current tangled-rope equilibrium is temporary; the constraint will drift toward either snare (state override, gender-equity law imposed) or piton (religious authority maintains performance while state formally withdraws). If stable: parallel jurisdiction can persist as a structural arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plural_jurisdiction_sustainability, empirical, 'Whether plural jurisdiction arrangement is institutionally sustainable').

omega_variable(
    this_reading_kernel_ambiguity,
    'Is this reading (''Shariat as interpreted by Muslim community authorities'') one coherent commitment, or does it contain internal contradictions between immutability claims and adaptive reinterpretation?',
    'Structural analysis of the reading''s authority grounding: if classical texts are truly immutable, can contemporary qazis introduce new interpretations without rupturing the claim? If interpretation is ongoing, on what basis is classical jurisprudence claimed as the authority source rather than contemporary community consensus? The contradiction is real; resolution depends on which horn is chosen (foreclose reinterpretation or foreclose immutability).',
    'If internal contradiction is primary: this reading is unstable; forced choice between ''Shariat is fixed (mountain)'' and ''Shariat evolves (piton or rope).'' If contradiction is managed through layering (classical texts for legitimacy, interpretation for adaptation): the reading is stable but high-theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_kernel_ambiguity, conceptual, 'Internal coherence of Shariat-as-interpreted-by-authorities reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(muslim_shariat_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msr_theater_t0_colonial, muslim_shariat_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(msr_theater_t25_post_independence, muslim_shariat_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(msr_theater_t50_contemporary, muslim_shariat_reading, theater_ratio, 50, 0.51).

% Extraction over time
narrative_ontology:measurement(msr_extractiveness_t0_colonial_period, muslim_shariat_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(msr_extractiveness_t25_independence_era, muslim_shariat_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(msr_extractiveness_t50_contemporary, muslim_shariat_reading, base_extractiveness, 50, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(msr_suppression_t0_colonial, muslim_shariat_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(msr_suppression_t25_post_independence, muslim_shariat_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(msr_suppression_t50_contemporary, muslim_shariat_reading, suppression_requirement, 50, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(muslim_shariat_reading, 0.12).
narrative_ontology:affects_constraint(muslim_shariat_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, secular_civil_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, state_jurisdiction_contestation).

% DUAL FORMULATION NOTE:
% The marriage_authority_kernel decomposes into five constraint stories, each instantiating one reading with its own ε, beneficiary/victim structure, and classification. The muslim_shariat_reading is one member of this family. All five are linked via network.affects_constraints because they compete for institutional dominance in the same domain (family law authority). The ε-invariance principle applies: each reading would produce a different ε if measured under different observables (e.g., 'gender equity achieved' vs 'community autonomy preserved'). Each reading gets its own story rather than one story with 'measurement basis' parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(muslim_shariat_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
