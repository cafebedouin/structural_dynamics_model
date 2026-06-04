% ============================================================================
% CONSTRAINT STORY: japanese_constitution_1947__rights_catalog_1947
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_japanese_constitution_1947__rights_catalog_1947, []).

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
 *   constraint_id: japanese_constitution_1947__rights_catalog_1947
 *   human_readable: The 1947 Constitution's Rights Catalog: Individual Dignity Against Household-State Hierarchy
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The 1947 Japanese Constitution's rights chapter (Articles 1-40,
 *   especially Articles 24-28) represents the deliberate inscription of
 *   individual dignity, sex equality in marriage, worker protections, and
 *   academic freedom into Japan's governing text. This chapter out-ran its
 *   era in two senses: (1) it articulated principles (especially sex equality
 *   and social minimums) that were radical for 1947 Japan, embodying Weimar
 *   constitutional theory and American New Deal ambitions; (2) it created
 *   constitutional obligation to suppress the ie household system — the
 *   patriarchal extended family structure that had grounded social hierarchy
 *   from Tokugawa through the imperial period. The constraint exhibits a
 *   tangled-rope structure: the constitutional text provides genuine
 *   coordination benefits (legitimacy for rights claims, institutional
 *   pathways through courts, organizing basis for reform movements) while
 *   simultaneously suppressing alternative hierarchies (the ie system,
 *   firm-level wage prerogatives, state collective priority). The suppression
 *   mechanism has weakened over 75 years as enforcement through litigation,
 *   labor organizing, and civil society pressure has internalized
 *   constitutional principles into practice. However, the constraint
 *   maintains extractive force through informal persistence of household and
 *   firm hierarchies despite formal constitutional prohibition — the ie
 *   household continues to structure family decision-making informally,
 *   employers continue to resist labor standards, and the state apparatus
 *   continues to prioritize collective interest over individual rights in
 *   security and administrative contexts.
 *
 * KEY AGENTS:
 *   - Individuals claiming dignity rights: Primary beneficiaries (powerless/trapped initially, becoming mobile/constrained over time) — the constitutional text grounds their claims against household and state authority
 *   - The ie household system: Primary victim (powerless/trapped) — traditional patriarchal family structure faces statutory negation of its legal authority
 *   - Women in marriage and family: Mixed position (moderate/constrained) — benefit from constitutional sex equality but face suppression through patriarchal enforcement and economic dependency
 *   - Employers and firm hierarchies: Mixed position (powerful/constrained) — benefit from labor law coordination but face suppression of prerogatives through worker organizing and court enforcement
 *   - Workers organizing for social minimums: Beneficiaries (organized/mobile) — constitutional grounds for labor organizing and wage standards
 *   - Constitutional courts: Institutional beneficiary (institutional/arbitrage) — jurisdiction and legitimacy expand through rights adjudication
 *   - Allied occupation authority (GHQ): Institutional beneficiary (institutional/arbitrage) — coordinates post-war liberal governance norms
 *   - Civil society reform movements: Organized beneficiaries (organized/mobile) — women's organizations, labor unions, academic freedom advocates benefit from constitutional scaffolding
 *   - Imperial-bureaucratic apparatus: Mixed victim (institutional/constrained) — state prerogatives formally constrained by rights text, but enforcement gaps allow informal continuation of collective-priority governance
 *   - Analytical observer: Neutral position (analytical/analytical) — risks naturalizing the rights framework as immutable natural law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(japanese_constitution_1947__rights_catalog_1947, 0.38).
domain_priors:suppression_score(japanese_constitution_1947__rights_catalog_1947, 0.62).
domain_priors:theater_ratio(japanese_constitution_1947__rights_catalog_1947, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(japanese_constitution_1947__rights_catalog_1947, extractiveness, 0.38).
narrative_ontology:constraint_metric(japanese_constitution_1947__rights_catalog_1947, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(japanese_constitution_1947__rights_catalog_1947, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(japanese_constitution_1947__rights_catalog_1947, tangled_rope).
narrative_ontology:human_readable(japanese_constitution_1947__rights_catalog_1947, "The 1947 Constitution's Rights Catalog: Individual Dignity Against Household-State Hierarchy").
narrative_ontology:topic_domain(japanese_constitution_1947__rights_catalog_1947, "political/legal/constitutional").

domain_priors:requires_active_enforcement(japanese_constitution_1947__rights_catalog_1947).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(japanese_constitution_1947__rights_catalog_1947, 'ad73a311-b806-47aa-9869-5cc0b7c08bc8').
narrative_ontology:cs_kernel_codification('ad73a311-b806-47aa-9869-5cc0b7c08bc8', formalized).
narrative_ontology:cs_authority_grounding('ad73a311-b806-47aa-9869-5cc0b7c08bc8', extraction).
narrative_ontology:cs_interpretation_layer_present('ad73a311-b806-47aa-9869-5cc0b7c08bc8').
narrative_ontology:cs_reading_relation('ad73a311-b806-47aa-9869-5cc0b7c08bc8', japanese_constitution_1947__article_9_renunciation, coexists_with).
narrative_ontology:cs_reading_relation('ad73a311-b806-47aa-9869-5cc0b7c08bc8', japanese_constitution_1947__ghq_drafting_imposition, influences).
narrative_ontology:cs_reading_relation('ad73a311-b806-47aa-9869-5cc0b7c08bc8', japanese_constitution_1947__symbol_emperor, coexists_with).
narrative_ontology:cs_axiom('ad73a311-b806-47aa-9869-5cc0b7c08bc8', foundational, individual_dignity_sovereign_over_collective).
narrative_ontology:cs_axiom_status(individual_dignity_sovereign_over_collective, holdable).
narrative_ontology:cs_axiom_grounding('ad73a311-b806-47aa-9869-5cc0b7c08bc8', individual_dignity_sovereign_over_collective, deontological).
narrative_ontology:cs_axiom('ad73a311-b806-47aa-9869-5cc0b7c08bc8', foundational, sex_equality_in_marriage_non_negotiable).
narrative_ontology:cs_axiom_status(sex_equality_in_marriage_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ad73a311-b806-47aa-9869-5cc0b7c08bc8', sex_equality_in_marriage_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('ad73a311-b806-47aa-9869-5cc0b7c08bc8', individual_dignity_framework).
narrative_ontology:cs_drift_state('ad73a311-b806-47aa-9869-5cc0b7c08bc8', contemporary_2022, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ad73a311-b806-47aa-9869-5cc0b7c08bc8', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(japanese_constitution_1947__rights_catalog_1947, japanese_constitution_1947).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__rights_catalog_1947, individuals_claiming_dignity_rights).
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__rights_catalog_1947, women_in_marriage).
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__rights_catalog_1947, workers_seeking_social_minimums).
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__rights_catalog_1947, academic_community).
narrative_ontology:constraint_victim(japanese_constitution_1947__rights_catalog_1947, ie_system_household_codes).
narrative_ontology:constraint_victim(japanese_constitution_1947__rights_catalog_1947, patriarchal_family_authority).
narrative_ontology:constraint_victim(japanese_constitution_1947__rights_catalog_1947, state_collective_over_individual).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IE HOUSEHOLD SYSTEM (SNARE) — The traditional patriarchal family structure faces constitutional text that explicitly invalidates its legal hierarchy (Article 24: 'laws concerning family relations shall be made from the standpoint of individual dignity and the essential equality of the sexes'). The ie cannot exit this constraint; its legal authority has been statutorily negated. Yet enforcement remains uneven — informal household authority persists despite constitutional prohibition. The ie experiences maximum extraction: its legal legitimacy is structurally denied while informal power persists in practice.
constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMEN IN MARRIAGE (TANGLED ROPE) — Constitutional text grants sex equality in marriage and family relations, but enforcement depends on costly litigation, social backlash, and departure from family norms. Women benefit from constitutional grounding of equality claims but face suppression through patriarchal enforcement of informal family codes and economic dependency. The constraint is both coordinative (grounds legitimate claims) and extractive (suppresses exercise of rights through social cost).
constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYERS AND FIRM HIERARCHY (TANGLED_ROPE) — Constitutional text subjects firm-level extraction to social minimum standards and worker dignity (Articles 25–28: right to maintain minimum standards of wholesome and cultured living; right to work; right to organize). Employers benefit from legal certainty about labor relations but face suppression of their prerogatives over worker control and wage-setting. Active enforcement through labor courts and union pressure constrains firm autonomy while coordinating baseline labor standards.
constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL COURT SYSTEM (ROPE) — Courts experience the rights chapter as a coordination mechanism: Article 37 guarantees access to courts for rights claims, and Article 81 vests courts with power to declare laws unconstitutional. The court system benefits from jurisdictional expansion and institutional legitimacy derived from defending individual rights. Low effective extraction because the courts have agency and structural advantage in interpreting the rights chapter.
constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ALLIED DRAFTING AUTHORITY (ROPE) — GHQ and the American occupation power structure experience the rights chapter as coordination of post-war liberal governance norms. The chapter embeds American New Deal and Weimar constitutional theory, establishing a shared epistemic frame with occupying authority. This perspective experiences the constraint as low-extraction coordination: the occupation frame aligns with the rights text, and both benefit from the institution of democratic legitimacy.
constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVIL SOCIETY REFORM MOVEMENTS (SCAFFOLD) — Women's organizations, labor unions, academic freedom advocates, and social rights movements experience the rights chapter as a sunset structure: a constitutional commitment that creates pathway dependency toward institutional reform. The constraint has sunset logic because constitutional text provides legitimacy for organizing while acknowledging that household and firm hierarchy have not been fully dislodged. Extractiveness is low from this perspective because organized agents see agency and exit pathways (through litigation, organizing, political change) that do not require abandoning the constraint itself.
constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: IMPERIAL-BUREAUCRATIC APPARATUS (PITON) — The state apparatus (especially the Home Ministry, Interior Ministry, and remnant imperial bureaucracy) experiences the rights chapter as largely performative: the constitutional text is maintained formally while administrative practice continues patterns of social collective priority over individual rights. Theater ratio is high because the apparatus pays lipservice to rights without restructuring the deep institutional commitments to hierarchy and state prerogative. The rights chapter persists through institutional inertia — it is the occupation's legacy — rather than because the bureaucracy has internalized individual rights as governing principle.
constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, individual dignity and equality are fundamental properties of human personhood — the rights chapter merely codified what is inherent to any legitimate governance order. Rights cannot be extracted from persons any more than gravity can be extracted from matter; the constraint appears immutable. However, the structural data contradicts this mountain classification — identifiable beneficiaries (individuals against household-state) and enforcement requirements reveal this as a false summit: a contingent institutional choice naturalized as universal law.
constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(japanese_constitution_1947__rights_catalog_1947_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(japanese_constitution_1947__rights_catalog_1947, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(japanese_constitution_1947__rights_catalog_1947, TR),
    TR >= 0.70.

:- end_tests(japanese_constitution_1947__rights_catalog_1947_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits real extraction because household and firm hierarchies continue to suppress individual exercise of constitutional rights through informal mechanisms (patriarchal family pressure, economic coercion, employment precarity). However, extractiveness is not high (not > 0.46) because the constraint is genuinely coordinative in its core function — it provides legitimate framework for rights claims and institutional pathways (courts, organizing) for enforcement. The balance reflects a tangled rope: coordination function (rights grounding) co-exists with extraction mechanism (informal suppression of rights exercise). The measurement trajectory shows extractiveness declining from 0.48 (1947) to 0.33 (2022), indicating that enforcement through decades of litigation, labor organizing, and normalization of equality principles has reduced the gap between constitutional text and practice. Suppression (0.62): Moderate-high. At 1947, suppression was very high (0.75) because the ie system, patriarchal family law, employer prerogatives, and state collective authority all faced constitutional negation without enforcement mechanisms. Suppression persists through informal mechanisms (family pressure, economic coercion, employment discrimination, administrative discretion), but enforcement has grown through courts and civil society pressure. The decline from 0.75 to 0.55 reflects that legal enforcement, norm change, and generational turnover have reduced barriers to exercising constitutional rights. Theater ratio (0.55): Moderate. The imperial-bureaucratic apparatus maintains performative commitment to rights while continuing collective-priority governance in practice (high theater initially, declining as norms internalize). However, the courts and civil society movements generate substantial functional activity in rights adjudication and organizing — actual enforcement occurs, not just ritual. The theater ratio declining from 0.68 to 0.48 reflects that functional enforcement has increased relative to performative lip-service. If the rights chapter were a piton, theater would be > 0.70 consistently; instead, the declining trajectory shows the constraint moving from hybrid (snare + theater) toward genuine tangled rope (coordination + reduced suppression).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound. The ie household experiences maximum extraction (snare) — constitutional negation of its legal authority with informal persistence. Women in marriage experience mixed effects (tangled rope) — constitutional grounding of equality claims but suppression through patriarchal norms. Employers experience mixed effects (tangled rope) — labor standards coordination with suppression of wage prerogatives. Courts, civil society, and the occupation authority experience the constraint as beneficial coordination (rope) — institutional expansion and legitimacy. The bureaucratic apparatus experiences it as degraded ritual (piton) — maintained through inertia while collective-priority governance continues informally. The analytical observer risks seeing immutable natural law (mountain) — individual dignity as inherent property of personhood. The engine's false summit detector should flag the mountain perspective: if the rights chapter were truly immutable natural law, it would not require active enforcement (Article 81: courts declare laws unconstitutional; Article 37: access to courts for rights claims). Immutable laws do not need courts to defend them. The false summit reveals that 'individual dignity' is a contingent institutional commitment that benefits identifiable agents (rights claimants, courts, reform movements) against others (household and state collectives resisting the constraint).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to extraction flow. The ie household (victim + trapped) derives high d ≈ 0.90, producing high f(d) ≈ 1.28, maximum experienced extraction. Women in marriage (victims + constrained) derive moderate-high d ≈ 0.70, producing f(d) ≈ 0.90, moderate extraction. Employers (mixed + constrained) derive moderate d ≈ 0.60, producing f(d) ≈ 0.75, moderate extraction reflecting both suppression of prerogatives and coordination benefits. Courts and occupation authority (beneficiaries + arbitrage) derive low d ≈ 0.15, producing f(d) ≈ -0.01, near-zero or negative experienced extraction (they benefit without bearing cost). Civil society movements (beneficiaries + organized + mobile) derive low d ≈ 0.35, producing f(d) ≈ 0.40, low extraction because organized agents have agency and see exit pathways. The bureaucratic apparatus (institutional constrained position) derives moderate d ≈ 0.55 (constrained agents in mixed position), producing f(d) ≈ 0.75, reflecting that the apparatus experiences formal suppression of prerogatives but informal persistence of practice. No directionality overrides are required — the structural derivation from beneficiary/victim status and exit options captures the perspectival differentiation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_gap_ie_persistence,
    'Why does the ie household system persist structurally despite constitutional prohibition of patriarchal family authority?',
    'Comparative legal sociology: study enforcement patterns of Articles 24, 27, 28 in family law courts; document rate of challenges to patriarchal family decision-making vs successful constitutional claims; measure household authority persistence through surveys of family practice vs legal expectation',
    'If enforcement gap is wide (> 70% of patriarchal decisions go unchallenged): the constraint is primarily extractive (snare), with constitutional text as theater. If enforcement gap is closing (< 30% persistence): the constraint is a genuine tangled rope with weakening extraction. If enforcement gap is moderate and stratified by class/region: constraint exhibits network heterogeneity — tangled rope in urban middle-class contexts, snare in rural agricultural contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_gap_ie_persistence, empirical, 'Structural persistence of ie authority despite constitutional text').

omega_variable(
    reading_vs_article_9_tension,
    'Does the rights-catalog reading coexist with or foreclose the article_9_renunciation reading?',
    'Doctrinal analysis: can a constitutional framework hold both individual dignity rights (Article 24-28) and war renunciation (Article 9) without internal contradiction? Historical evidence: did article 9 pacifism depend on or diverge from the rights chapter''s individualism? Institutional analysis: do advocates for articles 9 and 24-28 form a unified coalition or competing factions?',
    'If coherent (coexists): the 1947 Constitution is a unified framework. If tension (influences): one reading creates pressure on the other — e.g., article 9 renunciation requires individual refusal of military service, which depends on individual rights; or article 24 equality claims require state power to enforce, which article 9 limits. If foreclosure (rare): one reading''s core logic rules out the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_article_9_tension, conceptual, 'Structural relationship between rights-catalog and article-9 readings').

omega_variable(
    ghq_authenticity_vs_japanese_aspiration,
    'Is the rights chapter authentically Japanese (articulating pre-existing reform aspirations from Meiji liberalism, Taisho democracy, socialist movements) or authentically American-imposed (articulating GHQ''s New Deal ideology)?',
    'Intellectual history: document pre-war Japanese constitutional reform proposals, Meiji-era liberal theory, socialist manifestos in 1920s-30s Japan; compare their rights frameworks to the 1947 text. Drafting history: examine GHQ''s source documents (which American constitutional models they cited), Japanese government''s revisions and resistance points, what text survived debate. Institutional analysis: trace which provisions have Japanese institutional precedent (civil rights law, labor regulation, educational autonomy) vs which are alien imports.',
    'If primarily American: legitimacy of the constraint depends on occupying power''s authority — a false summit claim becomes more defensible (the rights are imposed, not natural). If primarily Japanese aspiration: legitimacy derives from endogenous political tradition — false summit claim weakens (this is what Japan chose). If hybrid (most likely): the constraint is a real tangled rope where American occupier and Japanese reformers both benefit from the same text, but for different reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ghq_authenticity_vs_japanese_aspiration, conceptual, 'Origins and authenticity of the 1947 rights chapter').

omega_variable(
    constitutional_monarchy_coherence,
    'Can the rights chapter''s principle of individual sovereignty coexist logically with the symbol_emperor reading''s derivation of imperial authority from ''the people''s will''?',
    'Doctrinal analysis: does Article 24 (individual dignity grounds family law) + Articles 1-4 (emperor position derives from people''s will) create an incoherent dual sovereignty? Institutional analysis: when individual rights and imperial prerogative conflict (e.g., security clearance based on imperial judgment), which principle wins in actual cases? Political theory: can liberal individualism and symbolic monarchy coexist, or does one reading foreclose the other?',
    'If coherent coexistence: the 1947 Constitution is a stable unified framework. If tension/influences: the symbol_emperor reading creates legitimacy pressure on the rights reading — imperial authority can override individual claims if framed as serving ''the people''s will.'' If foreclosure: one reading logically eliminates the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_monarchy_coherence, conceptual, 'Coherence between individual rights and symbolic imperial authority').

omega_variable(
    reading_vs_ghq_drafting_imposition,
    'If this reading (rights-catalog) is authentic to Japanese aspirations, does it coexist with or foreclose the ghq_drafting_imposition reading?',
    'Institutional history: document the factuality of one-week GHQ drafting (dates, personnel, process) against the authenticity question above. If both are factually true (GHQ drafted quickly AND the text reflects Japanese aspirations), they coexist (occupier and reformers aligned). If GHQ imposed text that diverges from Japanese aspirations, ghq_drafting_imposition forecloses the authentic rights_catalog reading (can''t both be imposed by foreign power AND authentically Japanese).',
    'Determines legitimacy grounds for the rights chapter: does Japan accept the constraint because it''s their authentic choice, or because occupation power is legitimate, or because the text happens to serve both purposes?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_ghq_drafting_imposition, conceptual, 'Relationship between this reading and the ghq_drafting_imposition reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(japanese_constitution_1947__rights_catalog_1947, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jp1947rights_theater_1947, japanese_constitution_1947__rights_catalog_1947, theater_ratio, 0, 0.68).
narrative_ontology:measurement(jp1947rights_theater_1967, japanese_constitution_1947__rights_catalog_1947, theater_ratio, 20, 0.62).
narrative_ontology:measurement(jp1947rights_theater_1987, japanese_constitution_1947__rights_catalog_1947, theater_ratio, 40, 0.55).
narrative_ontology:measurement(jp1947rights_theater_2022, japanese_constitution_1947__rights_catalog_1947, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(jp1947rights_extractiveness_1947, japanese_constitution_1947__rights_catalog_1947, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(jp1947rights_extractiveness_1967, japanese_constitution_1947__rights_catalog_1947, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(jp1947rights_extractiveness_1987, japanese_constitution_1947__rights_catalog_1947, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(jp1947rights_extractiveness_2022, japanese_constitution_1947__rights_catalog_1947, base_extractiveness, 75, 0.33).

% Suppression requirement over time
narrative_ontology:measurement(jp1947rights_suppression_1947, japanese_constitution_1947__rights_catalog_1947, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(jp1947rights_suppression_1967, japanese_constitution_1947__rights_catalog_1947, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(jp1947rights_suppression_1987, japanese_constitution_1947__rights_catalog_1947, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(jp1947rights_suppression_2022, japanese_constitution_1947__rights_catalog_1947, suppression_requirement, 75, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(japanese_constitution_1947__rights_catalog_1947, identity_coordination).
narrative_ontology:affects_constraint(japanese_constitution_1947__rights_catalog_1947, japanese_constitution_1947__article_9_renunciation).
narrative_ontology:affects_constraint(japanese_constitution_1947__rights_catalog_1947, japanese_constitution_1947__ghq_drafting_imposition).
narrative_ontology:affects_constraint(japanese_constitution_1947__rights_catalog_1947, japanese_constitution_1947__symbol_emperor).

% DUAL FORMULATION NOTE:
% The 1947 Japanese Constitution is a contested kernel instantiated through four distinct constraint stories, each modeling a different structural reading of the unified text. The rights_catalog reading models the Constitution as suppressing household-state hierarchy and establishing individual dignity. The article_9 reading models it as war-renouncing and peace-constituting. The ghq_drafting reading models it as occupation-imposed foreign governance text. The symbol_emperor reading models it as preserving imperial authority derived from popular sovereignty. These are not competing observations of the same constraint but distinct constraints instantiated from the same kernel. They should be linked bidirectionally: each reading's network.affects_constraints should include the others, with commentary documenting the coherence or tension relationships (coexists_with for simultaneous holding; influences for structural pressure; forecloses for logical exclusivity — rare in this case). The epsilon values differ because the readings measure different structural properties of the unified constitutional text: extractiveness of individual rights suppression (this reading) vs extractiveness of war prohibition (article_9 reading) vs extractiveness of occupation imposition (ghq_drafting reading) vs extractiveness of symbolic imperial authority (symbol_emperor reading). Decomposition into separate stories follows the ε-invariance principle: changing which aspect of the Constitution you measure changes epsilon, signaling distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
