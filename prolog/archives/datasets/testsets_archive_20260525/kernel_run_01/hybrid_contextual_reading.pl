% ============================================================================
% CONSTRAINT STORY: hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_contextual_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Category Membership (Sex/Gender Domain Switching)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The hybrid contextual reading of category membership—deploying biological
 *   sex for medical/sports/safety contexts and gender identity for
 *   social/legal recognition contexts—is ONE READING of a contested kernel:
 *   what constitutes legitimate category membership for the woman/female
 *   category. This reading differs structurally from the sex-biology reading
 *   (which asserts sex-essentialism universally) and the gender-identity
 *   reading (which asserts gender identity as the authoritative standard).
 *   The hybrid reading instantiates a compromise framework that treats
 *   neither reading as universally authoritative but rather context-sensitive
 *   in legitimacy. The constraint exhibits tangled rope dynamics because
 *   institutional actors must maintain multiple authoritative frameworks
 *   simultaneously—coordination function (providing flexibility across
 *   domains) with asymmetric extraction (forcing powerless agents to accept
 *   subordination in domain-specific contexts). The increasing theater_ratio
 *   (0.48 → 0.64 over 20 time units) reflects growing institutional
 *   inconsistency: as the gap between declared commitment to both readings
 *   and actual deployment of sex-based categories in operational systems
 *   becomes more visible, institutions perform increasing amounts of
 *   narrative work to reconcile the contradiction without reforming the
 *   underlying systems. Both sex-essentialists and gender-identity-affirming
 *   agents are positioned as victims when their reading is subordinated in
 *   domain-specific contexts; institutional conflict-minimizers are
 *   positioned as beneficiaries because the hybrid framework allows them to
 *   avoid zero-sum choice while maintaining operational simplicity.
 *
 * KEY AGENTS:
 *   - Sex-essentialist agents (identity_locked/powerless) — Victims in legal recognition and gender-affirming social contexts; bear extraction cost of institutional subordination of their reading
 *   - Gender-identity-affirming agents (identity_locked/powerless) — Victims in medical/sports/safety contexts; bear extraction cost when biological sex becomes enforced category
 *   - Institutional context-switchers (moderate/constrained) — Must enforce two contradictory frameworks simultaneously; experience tangled rope costs of maintaining institutional fragmentation
 *   - Meta-institutional conflict managers (institutional/arbitrage) — Primary beneficiaries; benefit from flexibility without requiring choice. Regulatory bodies, legal frameworks, institutional designers.
 *   - Organized pluralist coalition (organized/mobile) — See constraint as temporary scaffold with sunset clause; advocating for institutional capacity to compartmentalize readings
 *   - Legacy category systems (institutional/constrained) — Piton perspective; persist through inertia, performing theater by claiming commitment to both readings while deploying sex-based operational categories
 *   - Analytical observer (analytical/analytical) — Risks naturalizing the institutional compromise as a law of nature rather than contingent design choice (false summit candidate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_contextual_reading, 0.52).
domain_priors:suppression_score(hybrid_contextual_reading, 0.58).
domain_priors:theater_ratio(hybrid_contextual_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_contextual_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hybrid_contextual_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hybrid_contextual_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_contextual_reading, "Hybrid Contextual Category Membership (Sex/Gender Domain Switching)").
narrative_ontology:topic_domain(hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hybrid_contextual_reading, distributed).
narrative_ontology:cs_authority_grounding(hybrid_contextual_reading, distributed).
narrative_ontology:cs_kernel_id(hybrid_contextual_reading, woman_female_category).
narrative_ontology:cs_reading_relation(hybrid_contextual_reading, sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation(hybrid_contextual_reading, gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom(hybrid_contextual_reading, foundational, category_legitimacy_is_context_sensitive).
narrative_ontology:cs_axiom_status(category_legitimacy_is_context_sensitive, holdable).
narrative_ontology:cs_axiom(hybrid_contextual_reading, foundational, biological_sex_epistemically_relevant_for_medical_safety).
narrative_ontology:cs_axiom_status(biological_sex_epistemically_relevant_for_medical_safety, holdable).
narrative_ontology:cs_axiom(hybrid_contextual_reading, foundational, gender_identity_required_for_legal_recognition_social_inclusion).
narrative_ontology:cs_axiom_status(gender_identity_required_for_legal_recognition_social_inclusion, holdable).
narrative_ontology:cs_reference_frame(hybrid_contextual_reading, compartmentalized_legitimate_readings).
narrative_ontology:cs_drift_state(hybrid_contextual_reading, contemporary_institutional_practice, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_contextual_reading, institutional_conflict_minimizers).
narrative_ontology:constraint_beneficiary(hybrid_contextual_reading, regulatory_bodies).
narrative_ontology:constraint_victim(hybrid_contextual_reading, sex_essentialists).
narrative_ontology:constraint_victim(hybrid_contextual_reading, gender_identity_affirming_agents).
narrative_ontology:constraint_victim(hybrid_contextual_reading, category_subordinated_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEX-ESSENTIALIST (SNARE) — In gender-identity-prioritized contexts (legal recognition, social/cultural spaces), the sex-essentialist position is suppressed and treated as illegitimate. The agent is identity-locked: their foundational understanding of category membership is constituted through biological sex, making the cognitive shift to gender-identity primacy require abandoning their core epistemic framework. They bear the cost of institutional subordination while the beneficiary (institutions managing legal recognition) benefits from minimizing conflicts by delegitimizing their reading. Maximum extraction from this perspective.
constraint_indexing:constraint_classification(hybrid_contextual_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: GENDER-IDENTITY-AFFIRMING (SNARE) — In medical/sports/safety contexts where biological sex is used as the operational category, gender-identity-affirming agents face institutional subordination. Their identity is fused with the claim that gender identity constitutes category membership, making them unable to accept the sex-essentialist reading within institutional frameworks that deploy it. They bear the extraction cost when biological sex becomes the enforced category. The constraint extracts from both groups symmetrically — whichever reading is subordinated in a given domain experiences snare-level dynamics.
constraint_indexing:constraint_classification(hybrid_contextual_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL CONTEXT-SWITCHER (TANGLED ROPE) — Actors within institutions (hospitals, sports governing bodies, courts, legislatures) that must navigate both medical/safety contexts and social/legal recognition contexts experience tangled rope dynamics. They benefit from the flexibility of context-switching (coordination function: different categories for different epistemic/safety purposes), but also bear the cost of maintaining contradictory authoritative frameworks. Active enforcement required: they must apply one rule set in medical contexts and another in legal contexts, creating cognitive load and institutional fragmentation.
constraint_indexing:constraint_classification(hybrid_contextual_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: META-INSTITUTIONAL CONFLICT MANAGER (ROPE) — Regulatory bodies, legal frameworks, and institutional designers that have adopted the hybrid contextual reading benefit substantially. They experience the constraint as pure coordination: they solve the legitimate problem of managing multiple legitimate epistemic contexts (biology for medicine/sports/safety, gender identity for legal recognition and social inclusion) without requiring universal commitment to one framework. They arbitrage between contexts, benefiting from institutional flexibility while avoiding the zero-sum conflict of forcing choice. Minimal extraction cost — maximum coordination benefit.
constraint_indexing:constraint_classification(hybrid_contextual_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED PLURALIST COALITION (SCAFFOLD) — International human rights organizations, medical ethics bodies, and policy coalitions that advocate for context-sensitive category deployment see this constraint as temporary coordination problem with a sunset clause. They frame the goal as: build institutional capacity for epistemic pluralism such that both sex-based and gender-identity-based categories can operate legitimately in their respective domains without requiring subordination of either reading. This is a scaffold: high initial suppression of dissent, but declining enforcement pressure as institutional norms shift toward compartmentalization and technical integration.
constraint_indexing:constraint_classification(hybrid_contextual_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY CATEGORY SYSTEM (PITON) — Traditional institutional category systems (census data, medical coding, legal documents) persist largely through inertia. They were designed for a binary sex-essentialist framework and cannot natively accommodate gender identity. The system performs theater: institutions declare commitment to gender-identity recognition while deploying sex-based categories in medical records and statistical systems. The high theater_ratio (0.64) reflects that the category-switching mechanism is largely aspirational—institutions talk about context-sensitivity but default to sex-based categorization in operational systems because that's what the legacy infrastructure supports.
constraint_indexing:constraint_classification(hybrid_contextual_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective viewing the constraint as immutable, the reading is that certain biological differences are fundamentally relevant to medical/sports/safety domains and cannot be transcended by social construction. This perspective risks being a false summit: treating a contingent institutional arrangement (the choice to use biological sex as the operational category in medicine and sports) as a law of nature. However, the structural data contradicts mountain classification—identified beneficiaries and victims indicate this is a constructed framework subject to institutional redesign, not an irreducible natural law.
constraint_indexing:constraint_classification(hybrid_contextual_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_contextual_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_contextual_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_contextual_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_contextual_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over time. The hybrid framework extracts value through institutional simplification: meta-institutional actors benefit from having two legitimate readings available without forcing choice, while both powerless agent groups (sex-essentialists and gender-identity agents) bear costs of periodic subordination. The base_extractiveness increased from 0.35 (in period when institutional commitment to both readings was weak and context-switching was emerging) to 0.52 (in period when context-switching became declared institutional policy, forcing all actors to participate in the dual-framework system). Suppression (0.58): Moderate-high. Both sex-essentialists and gender-identity-affirming agents face institutional suppression—their reading is treated as illegitimate in domain-specific contexts. The suppression is not total (58% rather than 85%+) because neither reading faces complete institutional elimination; they face compartmentalized subordination, not wholesale rejection. Theater ratio (0.64): Moderate-high and increasing. Institutions increasingly perform narrative work to reconcile commitment to both readings while maintaining operational sex-based categorization in medicine, sports, and safety. The gap between declared values (both readings legitimate) and operational deployment (sex-based categories in functional systems) requires growing amounts of theater—explanations, exceptions, technical carve-outs, and domain-specific justifications.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival divergence across all agent positions. Sex-essentialists see extraction (snare) in contexts where their category is subordinated; gender-identity-affirming agents see extraction (snare) in contexts where biological sex is enforced. Institutional context-switchers see mixed costs and benefits (tangled rope)—they benefit from flexibility but pay integration costs. Regulatory bodies see pure coordination (rope)—the hybrid framework lets them avoid choice without losing legitimacy. The organized coalition sees a temporary problem with sunset potential (scaffold)—institutional capacity for compartmentalization is building. Legacy systems see their own degraded process (piton)—the theory declares both readings legitimate while practice defaults to sex-based categorization. The analytical observer risks seeing natural law (mountain) where there is institutional design. No single perspective 'gets it right'—the presheaf over observation sites includes all six types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent position relative to the extraction flow. Sex-essentialists as victims of subordination in gender-affirming contexts: d ≈ 0.90 (high target status), f(d) ≈ 1.28, producing high experienced extraction (snare). Gender-identity-affirming agents as victims of subordination in sex-based contexts: d ≈ 0.90, f(d) ≈ 1.28, producing high experienced extraction (snare). Institutional context-switchers as moderate actors bearing coordination costs: d ≈ 0.55 (symmetric—both costs and benefits), f(d) ≈ 0.75, producing moderate experienced extraction (tangled rope consistent with moderate power). Meta-institutional conflict managers as beneficiaries with arbitrage: d ≈ 0.15 (low target status), f(d) ≈ -0.01, producing low or negative effective extraction (rope). The engine derives these values from the beneficiary/victim declarations and exit options without manual computation—the directionality overrides array is empty because structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid contextual reading resolves the mandatrophy by revealing the kernel-level disagreement: which reading of the woman/female category should be authoritative? The reading instantiated here claims that authority is context-sensitive—neither reading is universally authoritative, but each is legitimate in its epistemic domain. This is NOT a compromise that makes both groups happy; it is a compromise that extracts from both groups by forcing them to operate within a framework that treats their reading as periodically illegitimate. The tangled rope classification is appropriate because the constraint does provide a genuine coordination function (allowing different institutional contexts to operate under different rules without requiring universal conversion to one reading) alongside asymmetric extraction (forcing powerless agents to accept subordination in domain-specific contexts). The mandatrophy is resolved by recognizing that the constraint exhibits all six types legitimately—which type you see depends on which agent position you adopt. The false summit risk (mountain perspective naturalizing the hybrid framework as inherent to biology) is detected by the structural data: identified beneficiaries (institutional conflict-minimizers) indicate this is constructed institutional choice, not law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the woman/female category is legitimate as the authoritative standard: biological sex, gender identity, or context-sensitive hybrid?',
    'Examination of institutional outcomes when each reading is deployed as primary. For sex-biology: which groups are harmed by exclusion from sex-segregated spaces designed for sex-specific physiology? For gender-identity: which groups are harmed by exclusion from legal recognition and institutional inclusion? For hybrid: does context-switching reduce or increase total harm?',
    'If sex-biology reading is authoritative: gender-identity-affirming agents face snare-level subordination; entire victim set shifts. If gender-identity reading is authoritative: sex-essentialists face snare-level subordination; victims reverse. If hybrid reading is sustained: both groups experience periodic subordination in domain-specific contexts, but total suppression is lower than single-reading hegemony.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the woman/female category is institutionally authoritative').

omega_variable(
    sex_biology_relevance_domain_specificity,
    'In which specific domains is biological sex relevance epistemically necessary vs. contingent institutional choice?',
    'Domain-by-domain empirical analysis: medicine (sex-specific physiology in treatment protocols, drug metabolism, reproductive health—clearly relevant), sports (sex-segregation designed for competition fairness under specific rule sets—contingent on chosen metric), safety (sex-based violence risk assessment, occupational hazards—mixed empirical basis), legal recognition (civil rights, family formation, property—no inherent biological relevance), social inclusion (bathroom access, pronoun use, institutional space design—no biological relevance)',
    'If sex-biology is necessary only in narrower domains than currently deployed: hybrid reading''s context-switching becomes more defensible. If sex-biology is relevant across more domains than gender-identity advocates acknowledge: snare classification for gender-identity agents becomes more severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sex_biology_relevance_domain_specificity, empirical, 'Domain-specific relevance of biological sex vs. social contingency').

omega_variable(
    identity_lock_asymmetry,
    'Is the identity_lock mechanism symmetric between sex-essentialists and gender-identity-affirming agents, or do they differ in their capacity to perceive the reading as changeable?',
    'Comparative analysis of defection rates, belief-change mechanisms, and epistemic frameworks. If sex-essentialists constitute identity primarily through biological category claims and gender-identity agents through relational identity claims, they may show different flexibility. Interview data from agents who have changed positions; comparison of cognitive/institutional barriers to perspective-switching.',
    'If asymmetric: one group experiences true identity-lock (agent structure is constituted through the claim), the other experiences constraint (high-cost but cognitively available exit). Would shift one snare classification toward constrained exit. If symmetric: both groups face genuine identity-lock; snare classification for both is structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_asymmetry, empirical, 'Whether identity-lock is symmetric between sex-essentialism and gender-identity affirmation').

omega_variable(
    institutional_capacity_for_true_context_switching,
    'Can institutions genuinely maintain separate authoritative readings for different contexts, or does one reading inevitably colonize others?',
    'Institutional case studies: jurisdictions that have attempted explicit context-switching (e.g., sex-based category in medical records AND gender-identity-based category in legal documents). Track whether the readings remain compartmentalized or whether enforcement pressure pushes toward unified framework. Measure theater_ratio: if context-switching works, theater should decline. If it fails, theater should increase as institutional inconsistency becomes visible.',
    'If true compartmentalization is achievable: scaffold classification is justified—the constraint has genuine sunset potential as institutional capacity matures. If one reading inevitably dominates: hybrid reading cannot be sustained; constraint must collapse to either sex-biology or gender-identity hegemony. Extractiveness would remain high indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capacity_for_true_context_switching, empirical, 'Whether institutions can maintain legitimate context-switching or if one reading colonizes others').

omega_variable(
    false_summit_naturalization,
    'Is the mountain perspective''s claim that sex-based categorization is natural law or an analytical observer naturalizing a contingent institutional choice?',
    'Comparative institutional history: What categorization systems existed before 20th-century medical/legal standardization? Do all societies deploy sex-essentialism or do some use gender, occupation, or kinship as primary categories? Is the apparent ''naturalness'' of sex-categorization an artifact of universal institutional deployment or a discovery of invariant structure?',
    'If naturalization (mountain is false summit): the hybrid reading is more defensible—category choice is institutional design, not law of nature. If true natural law: sex-based categorization should remain primary across all contexts. Determines whether hybrid reading is pragmatic compromise or evasive theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether sex-categorization is natural law or naturalized institutional choice').

omega_variable(
    victim_set_overlap,
    'Do sex-essentialists and gender-identity-affirming agents constitute overlapping or disjoint victim sets, or are there agents who belong to both and experience extraction from both readings?',
    'Population-level analysis: Are there individuals whose relationship to biological sex and gender identity crosses existing identity categories (e.g., trans agents who acknowledge sex-relevant physiology in medical contexts AND affirm gender identity in legal contexts)? Do these agents experience both snare dynamics (subordination in whichever context prioritizes the non-matching reading) or do they benefit from the hybrid framework?',
    'If overlapping: victims of one reading may benefit from the hybrid framework in other contexts, reducing total extraction. If disjoint: hybrid constraint extracts from both groups symmetrically—total suppression may actually increase by forcing participation in contradictory frameworks simultaneously. Affects overall Tangled Rope analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_overlap, empirical, 'Overlap between sex-essentialist and gender-identity victim populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_contextual_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybr_tr_t0, hybrid_contextual_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hybr_tr_t10, hybrid_contextual_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(hybr_tr_t20, hybrid_contextual_reading, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(hybr_be_t0, hybrid_contextual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hybr_be_t10, hybrid_contextual_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(hybr_be_t20, hybrid_contextual_reading, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_contextual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hybrid_contextual_reading, 0.18).
narrative_ontology:affects_constraint(hybrid_contextual_reading, sex_biology_reading).
narrative_ontology:affects_constraint(hybrid_contextual_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% The woman/female category kernel decomposes into three structurally distinct constraint readings with different victim sets, beneficiary structures, and extractiveness profiles. Sex-biology reading (ε ≈ 0.28, snare from gender-identity perspective) asserts biological sex universally; gender-identity reading (ε ≈ 0.35, snare from sex-essentialist perspective) asserts gender identity universally; hybrid contextual reading (ε ≈ 0.52, tangled rope) attempts context-sensitive legitimacy for both. Each reading is a separate constraint story linked through network.affects_constraints. The hybrid reading influences both sibling readings by creating structural pressure toward compartmentalization and institutional capacity-building.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
