% ============================================================================
% CONSTRAINT STORY: fundamental_rights_part_iii__equality_code
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_rights_part_iii__equality_code, []).

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
 *   constraint_id: fundamental_rights_part_iii__equality_code
 *   human_readable: Constitutional Equality Code (Articles 14–18) as Connected Suppression-Dismantling Scheme
 *   domain: constitutional_law/fundamental_rights
 *
 * SUMMARY:
 *   The Indian Constitution's Articles 14–18 constitute an integrated
 *   equality code that reads as a connected scheme of suppression-dismantling
 *   rather than as discrete clauses. Article 14 guarantees equal protection
 *   of the laws; Article 15 prohibits discrimination on specified grounds
 *   (caste, religion, sex, place of birth); Article 15(4) provides
 *   affirmative action carve-outs; Article 16 mandates equal opportunity in
 *   public employment; Article 17 abolishes untouchability (a caste-specific
 *   subordination practice); Article 18 abolishes hereditary titles and
 *   status. Read together, these articles form a constitutional apparatus
 *   whose function is to suppress the hierarchical ordering of human beings
 *   by birth or status and to reconstruct the state's role from enforcer of
 *   hierarchy to guarantor of equal dignity. This reading differs
 *   fundamentally from readings that treat the articles as independent rights
 *   clauses, each subject to its own reasonable restrictions. The equality
 *   code reading emphasizes the series-logic: each article removes a specific
 *   mechanism of status-based extraction (legal classification without
 *   reason, hereditary occupation barriers, ritual subordination, title-based
 *   privilege), and together they form a connected dismantling of
 *   state-backed hierarchy. Extractiveness (~0.38) reflects the cost to
 *   status-quo beneficiaries of dismantling hereditary and classificatory
 *   advantage; suppression (~0.62) reflects the state's historical and
 *   ongoing resistance to implementation through bureaucratic inertia,
 *   political pressure from advantage-holders, and resource constraints.
 *   Theater ratio (~0.45) is moderate: implementation includes genuine
 *   doctrinal work (Supreme Court jurisprudence on what equal opportunity
 *   means) alongside performative compliance (diversity policies masking
 *   persistent informal sorting).
 *
 * KEY AGENTS:
 *   - Historically discriminated groups (caste-oppressed, religious minorities, women, untouchables): Primary beneficiary (trapped/powerless) — formally empowered by the equality code but structurally trapped by non-enforcement and accumulated disadvantage. Organizational capacity is low; exit options limited.
 *   - State apparatus (civil service, police, local administration): Primary target (powerful/constrained) — bound by constitutional equality mandate but constrained by federalism, resource limits, and political pressure from status-quo beneficiaries. Resistance is both structural (capacity gaps) and deliberate (strategic non-enforcement).
 *   - Supreme Court: Institutional interpreter (institutional/arbitrage) — has significant discretion in reading the equality code as integrated or disaggregated, substantive or formal, narrow or broad. Experiences the constraint as coordination: establishes enforceable standards.
 *   - Status-quo beneficiaries (traditionally privileged castes, religious majorities, landholding classes): Victim of constraint (powerful/mobile) — face extraction of hereditary advantage and hierarchical privilege. Can exit through legal challenge, political mobilization, or exit to alternative jurisdictions.
 *   - Social justice movement (civil rights organizations, affirmative action advocates): Organized agent (organized/mobile) — mobilizes around the equality code; can shift strategies, pivot to other causes. Organized power enables partial enforcement where state fails.
 *   - Constitution-drafting lineage (Ambedkar's vision): Authority grounding (analytical/analytical) — the equality code's meaning derives from the Constituent Assembly's deliberation and Ambedkar's explicit commentary. The reading is grounded in this historical act, not in natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_rights_part_iii__equality_code, 0.38).
domain_priors:suppression_score(fundamental_rights_part_iii__equality_code, 0.62).
domain_priors:theater_ratio(fundamental_rights_part_iii__equality_code, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_rights_part_iii__equality_code, extractiveness, 0.38).
narrative_ontology:constraint_metric(fundamental_rights_part_iii__equality_code, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fundamental_rights_part_iii__equality_code, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_rights_part_iii__equality_code, tangled_rope).
narrative_ontology:human_readable(fundamental_rights_part_iii__equality_code, "Constitutional Equality Code (Articles 14–18) as Connected Suppression-Dismantling Scheme").
narrative_ontology:topic_domain(fundamental_rights_part_iii__equality_code, "constitutional_law/fundamental_rights").

domain_priors:requires_active_enforcement(fundamental_rights_part_iii__equality_code).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fundamental_rights_part_iii__equality_code, 'eb379b34-76e4-470d-9e8c-a7de6f89479e').
narrative_ontology:cs_kernel_codification('eb379b34-76e4-470d-9e8c-a7de6f89479e', formalized).
narrative_ontology:cs_authority_grounding('eb379b34-76e4-470d-9e8c-a7de6f89479e', lineage).
narrative_ontology:cs_interpretation_layer_present('eb379b34-76e4-470d-9e8c-a7de6f89479e').
narrative_ontology:cs_reading_relation('eb379b34-76e4-470d-9e8c-a7de6f89479e', fundamental_rights_part_iii__freedoms_article_19, coexists_with).
narrative_ontology:cs_reading_relation('eb379b34-76e4-470d-9e8c-a7de6f89479e', fundamental_rights_part_iii__remedies_article_32, influences).
narrative_ontology:cs_axiom('eb379b34-76e4-470d-9e8c-a7de6f89479e', foundational, hierarchy_suppression_integrated).
narrative_ontology:cs_axiom_status(hierarchy_suppression_integrated, holdable).
narrative_ontology:cs_axiom_grounding('eb379b34-76e4-470d-9e8c-a7de6f89479e', hierarchy_suppression_integrated, deontological).
narrative_ontology:cs_axiom('eb379b34-76e4-470d-9e8c-a7de6f89479e', foundational, status_privilege_extraction_dismantling).
narrative_ontology:cs_axiom_status(status_privilege_extraction_dismantling, holdable).
narrative_ontology:cs_axiom_grounding('eb379b34-76e4-470d-9e8c-a7de6f89479e', status_privilege_extraction_dismantling, deontological).
narrative_ontology:cs_reference_frame('eb379b34-76e4-470d-9e8c-a7de6f89479e', constitutional_equality_as_hierarchy_suppression).
narrative_ontology:cs_drift_state('eb379b34-76e4-470d-9e8c-a7de6f89479e', contemporary_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb379b34-76e4-470d-9e8c-a7de6f89479e', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(fundamental_rights_part_iii__equality_code, fundamental_rights_part_iii).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fundamental_rights_part_iii__equality_code, historically_discriminated_groups).
narrative_ontology:constraint_victim(fundamental_rights_part_iii__equality_code, state_backed_hereditary_hierarchy).
narrative_ontology:constraint_victim(fundamental_rights_part_iii__equality_code, exclusionary_classification_schemes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY EXCLUDED GROUPS (SNARE) — Formally empowered by the equality code but structurally trapped by state refusal to enforce it. The articles declare non-discrimination, abolish untouchability, mandate equal opportunity — but without remedial machinery or resource allocation, these declarations remain unfulfilled. Trapped by generations of accumulated disadvantage that formal equality cannot dissolve. High suppression (state machinery actively resists implementation through bureaucratic delay, judicial passivity, political neglect), moderate extractiveness (the benefit of formal rights is captured by those already advantaged, who can afford litigation and mobilization).
constraint_indexing:constraint_classification(fundamental_rights_part_iii__equality_code, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE STATE MACHINERY (TANGLED ROPE) — Bound by the constitutional commitment to equality but constrained by federalism (enforcement delegated to states), resource limits, and political pressure from status-quo beneficiaries. The state experiences the equality code as both a coordination mandate (it genuinely enables resource redistribution and legal reform) and an extraction: enforcement costs money, disrupts existing hierarchies, and triggers resistance. The state is neither pure enforcer nor pure resister — it is caught between constitutional obligation and political constraint.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__equality_code, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SUPREME COURT (ROPE) — The court has significant arbitrage power: it can narrow or expand the equality code's scope through interpretation. It experiences the constraint as pure coordination — the articles declare the principle, the court elaborates doctrine. The court's interpretive discretion creates benefit extraction for the court's legitimacy (it becomes arbiter of fundamental rights) but also creates coordination function (landmark rulings establish enforceable standards). From the court's structural position, the constraint is best classified as rope: it solves the coordination problem of applying abstract constitutional principles to concrete disputes.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__equality_code, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BUREAUCRATIC IMPLEMENTATION (PITON) — Civil service, police, local administration, and inspectorates formally bound by equality mandate but operating through routines established before the Constitution. Bureaucratic resistance to equality is often performative — compliance theater ('we have diversity policies') masking persistent hierarchical practice. Theater ratio high because implementation procedures are ceremonial rather than functionally equalizing; suppression requirement moderate because bureaucratic inertia is easier to maintain than active coercion. The constraint persists through ritual performance of non-discrimination while actual sorting by caste, religion, gender continues through informal channels.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__equality_code, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational perspective, fundamental equality is presented as an immutable principle: it emerges naturally from the commitment to universal human dignity and self-governance. The equality code declares these rights as pre-existing (not granted by the state, merely recognized), making them appear as natural law rather than constructed constraint. However, the structural data contradicts the mountain gate — the articles were written by Ambedkar's drafting committee and ratified by the Constituent Assembly; they are contingent historical acts. The false-summit detector should flag this perspective: the 'naturalness' of equality is a reading strategy, not a structural property.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__equality_code, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SOCIAL JUSTICE MOVEMENT (SCAFFOLD) — Civil rights organizations, affirmative action advocacy, and legal aid networks experience the equality code as a coordination mechanism with a generational sunset: as successive cohorts benefit from reservation and non-discrimination, the need for compensatory measures diminishes (in principle). The movement has mobile exit options (it can pivot to other causes, other constitutions) and organized power (mass mobilization, litigation campaigns). Theater ratio moderate because advocacy combines genuine doctrinal work with performance of constitutional commitment. The sunset is not automatic — it depends on whether redistributive mechanisms (reservations, scholarships, land reform) actually close disparities or whether they become permanent extraction mechanisms themselves.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__equality_code, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_rights_part_iii__equality_code_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fundamental_rights_part_iii__equality_code, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_rights_part_iii__equality_code, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fundamental_rights_part_iii__equality_code, TR),
    TR >= 0.70.

:- end_tests(fundamental_rights_part_iii__equality_code_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The equality code extracts status privilege from hereditary advantage-holders and redistributes it (in theory) to previously excluded groups. But extraction is incomplete — most of the benefit captured by excluded groups flows through litigation and litigation-based remedies (expensive, time-consuming), while informal extraction mechanisms (caste-based job networks, religious community preference, patriarchal property transfer) persist through non-state channels. The 70-year trajectory shows slight increase (0.28 → 0.38) driven by gradual state capacity-building (affirmative action recruitment, anti-discrimination monitoring) and doctrinal refinement (Supreme Court expanding Articles 14–18 scope), but the rate of increase is slower than predictions made in 1950. Suppression (0.62): High. Measured as the state's resistance to implementation, operationalized through: (1) percentage of discrimination complaints prosecuted vs. filed; (2) enforcement action against caste discrimination in public employment vs. documented cases; (3) implementation of land reform and untouchability abolition mandates. The trajectory shows decline (0.75 → 0.62) over 70 years, driven by increased litigation, civil society monitoring, and (in recent years) electoral pressure for reservation politics. But suppression remains high because structural non-compliance is easier than enforcement — state machinery at local levels simply ignores equality mandates. Theater ratio (0.45): Moderate. Implementation includes genuine substantive work (Supreme Court doctrinal elaboration of what equal opportunity means in public employment, what reservations are valid, how untouchability manifests in modern form). But it also includes pure theater: diversity policy announcements without resource allocation, complaint mechanisms without investigation, symbols of inclusion without material change. Theater increased from 1950 (immediate post-independence, limited implementation infrastructure) through 1975–2000 (rituals of compliance without substance), then stabilized as litigation and civil society pressure forced some doctrinal specificity. Current theater ratio reflects: genuine Supreme Court activism on equality doctrine (lowers theater) balanced against bureaucratic compliance theater (raises theater).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a critical perspectival gap between the formally empowered (historically excluded groups) and the structurally trapped (same groups, trapped by non-enforcement). The equality code promises the excluded groups formal rights and remedial mechanisms — they read it as Rope (coordination of non-discrimination) or Scaffold (temporary remediation with sunset as outcome parity is achieved). But their structural position (powerless, without litigation resources, facing state machinery that resists implementation) makes them experience it as Snare — trapped by the gap between formal promise and material reality. The state experiences the constraint as Tangled Rope: it is genuinely bound by Article 14–18, but enforcement is costly and politically risky, creating simultaneous coordination function (the articles enable coherent rights policy) and extraction (the state absorbs the cost of dismantling hierarchies, faces resistance from status-quo groups, and experiences resource constraints). The Supreme Court experiences pure Rope: the articles give the court authoritative interpretive power; the court elaborates doctrine and derives institutional legitimacy from doing so. Bureaucratic implementers experience Piton: they perform compliance (diversity policies, anti-discrimination procedures) while actual sorting by caste, religion, gender continues informally. The status-quo beneficiaries experience the constraint as extractive (Snare from their perspective) — they lose hereditary advantage and privilege extraction. The analytical observer risks seeing the constraint as natural law (Mountain) — fundamental equality as an inherent principle — but the structural data reveals this as false summit: the articles are contingent historical decisions by the Constituent Assembly, not universal axioms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from the agent's power level, exit options, and structural relationship to the extraction flow. Historically excluded groups: powerless agent + trapped exit + victim status (they bear the cost of non-enforcement) → high d → high f(d) → high experienced extractiveness (χ). The extraction they experience is not extraction by the state, but extraction by status-quo beneficiaries who refuse to surrender privilege — the state's non-enforcement is the mechanism enabling this. State apparatus: powerful agent (institutional power level reflects state's monopoly on law enforcement) + constrained exit (the state cannot simply ignore the Constitution without losing legitimacy) + mixed beneficiary/victim status (the state both enforces equality and protects existing orders through non-enforcement) → moderate d → moderate f(d). Supreme Court: institutional agent + arbitrage exit (the court can narrow or broaden the equality code through interpretation; arbitrage reflects the court's discretion) + beneficiary status (the court derives legitimacy and institutional power from becoming arbiter of fundamental rights) → low d → low f(d) → low or negative χ (the court experiences the constraint as empowering, not extractive). Status-quo beneficiaries: powerful agent + mobile exit (they have resources to evade, challenge, or seek alternative jurisdictions) + victim status (they lose privilege and hierarchical advantage) → high d despite mobility → moderate-high f(d) (power mitigates the extraction they experience, but mobility doesn't fully neutralize victim status). Bureaucratic apparatus: institutional agent + constrained exit (bureaucrats cannot ignore the state) + neither pure beneficiary nor pure victim (they are agents of state policy, but they gain from preserving existing hierarchies in which they often participate) → moderate d. Directionality overrides: None. The structural derivation captures the relationships without special adjustment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_substantive_equality_indeterminacy,
    'Is the equality code a formal prohibition on status-based classification (procedural justice) or a mandate for substantive outcome parity (distributive justice)?',
    'Doctrinal analysis of Supreme Court jurisprudence; examination of whether Article 15(4) (affirmative action clause) is read as exception to Article 15(1) (non-discrimination) or as instantiation of it. Empirical measurement: trajectories of actual outcome parity by caste/religion/gender across employment, education, wealth over 70-year interval.',
    'If formal: the constraint is a procedural lock against irrational classification (extractiveness ~0.15, type: Rope). If substantive: the constraint mandates redistribution and outcome correction (extractiveness ~0.55, type: Tangled Rope). The difference is essential to classification and cannot be resolved by text alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality_indeterminacy, conceptual, 'Formal vs. substantive equality interpretation indeterminacy').

omega_variable(
    state_capacity_vs_constitutional_mandate_gap,
    'Is the non-enforcement gap between Articles 14–18 and ground-level equality a structural feature of constitutional design (federalism, separation of powers limiting executive reach) or a measure of state refusal?',
    'Comparative constitutional analysis: examine federalism structures in other equality-heavy constitutions (Germany, South Africa); interview state officials and civil servants on resource allocation constraints vs. political choice; measure correlation between judicial orders mandating equality and actual implementation by line ministries.',
    'If structural constraint on state capacity: suppression is a coordination problem requiring institutional redesign, not extraction requiring enforcement (lowers χ). If state refusal: suppression is deliberate, extractiveness rises, and the constraint moves toward Snare. Current belief: mixed — both real capacity limits and strategic refusal operate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_vs_constitutional_mandate_gap, empirical, 'Gap between constitutional equality mandate and state implementation capacity').

omega_variable(
    kernel_vs_reading_contest,
    'Is the equality code a single integrated constitutional principle (kernel reading: connected scheme of suppression-dismantling across five articles) or five separate clauses each with its own scope and reasonable restrictions (alternative reading: disaggregated clauses each subject to modification)?',
    'Doctrinal: examine Supreme Court jurisprudence treating Articles 14–18 as a unified scheme vs. as separate, independently limitable provisions. Empirical: measure whether public discourse and litigation strategy emphasize the articles'' structural coherence or treat them as independent rights. Historical: analyze Ambedkar''s own constitutional commentary on whether he intended integrated or separate application.',
    'If kernel reading (integrated scheme): modification of one article triggers reinterpretation of all; the scheme has inherent stability and internal logic. If disaggregated: each article can be narrowed independently; the constraint is more vulnerable to piecemeal erosion. This is the core distinction driving this reading selection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_contest, conceptual, 'Whether equality code is integrated kernel or disaggregated separate clauses').

omega_variable(
    caste_vs_universal_equality_framing,
    'Does the equality code anchor in the specifically Indian history of caste subordination (hereditary, enforced by state, embodied in law and ritual) or in universal human rights non-discrimination principles that happen to apply here?',
    'Textual analysis of constitutional debates: does the Constituent Assembly debate explicitly invoke caste suppression as the evil to be corrected? Close reading of Articles 15(2), 17 (untouchability abolition): do they presuppose caste-specific remediation or generic discrimination doctrine? Comparison: do post-independence constitutions with no caste history generate parallel equality codes?',
    'If caste-specific anchoring: the suppression metrics apply specifically to caste hierarchy; the beneficiary set is the caste-oppressed; the constraint''s extractiveness is tied to caste-privilege extraction. If universal anchoring: the same articles apply to religion, gender, disability, and other classifications with potentially different suppression/extraction profiles. This affects whether the equality code is a single constraint or a family of constraints decomposed by ground (caste vs. religion vs. gender).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_vs_universal_equality_framing, conceptual, 'Whether equality code is caste-specific or universally framed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_rights_part_iii__equality_code, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frp3_eq_theater_1950, fundamental_rights_part_iii__equality_code, theater_ratio, 0, 0.3).
narrative_ontology:measurement(frp3_eq_theater_1975, fundamental_rights_part_iii__equality_code, theater_ratio, 25, 0.4).
narrative_ontology:measurement(frp3_eq_theater_2000, fundamental_rights_part_iii__equality_code, theater_ratio, 50, 0.48).
narrative_ontology:measurement(frp3_eq_theater_2020, fundamental_rights_part_iii__equality_code, theater_ratio, 70, 0.45).

% Extraction over time
narrative_ontology:measurement(frp3_eq_extr_1950, fundamental_rights_part_iii__equality_code, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(frp3_eq_extr_1975, fundamental_rights_part_iii__equality_code, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(frp3_eq_extr_2000, fundamental_rights_part_iii__equality_code, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(frp3_eq_extr_2020, fundamental_rights_part_iii__equality_code, base_extractiveness, 70, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(frp3_eq_supp_1950, fundamental_rights_part_iii__equality_code, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(frp3_eq_supp_1975, fundamental_rights_part_iii__equality_code, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(frp3_eq_supp_2000, fundamental_rights_part_iii__equality_code, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(frp3_eq_supp_2020, fundamental_rights_part_iii__equality_code, suppression_requirement, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_rights_part_iii__equality_code, enforcement_mechanism).
narrative_ontology:affects_constraint(fundamental_rights_part_iii__equality_code, fundamental_rights_part_iii__freedoms_article_19).
narrative_ontology:affects_constraint(fundamental_rights_part_iii__equality_code, fundamental_rights_part_iii__remedies_article_32).

% DUAL FORMULATION NOTE:
% The equality code reading is part of a three-reading family decomposing the kernel `fundamental_rights_part_iii`. Each reading emphasizes different structural elements: equality_code emphasizes the integrated suppression-dismantling logic of Articles 14–18; freedoms_article_19 emphasizes the disaggregated liberty rights of Article 19A–19D; remedies_article_32 emphasizes the enforcement and remedy-access mechanism of Article 32. These are not the same constraint viewed from different angles — they are three distinct constraints derived from three different readings of the same constitutional kernel. The equality_code reading has extractiveness ~0.38 (moderate, driven by privilege-dismantling); the freedoms reading would have lower extractiveness (freedom rights extract less because they do not redistribute as directly); the remedies reading would have different suppression profile (enforcement-capacity-dependent). All three readings are live in contemporary constitutional jurisprudence — courts, advocates, and scholars hold different readings and debate which reading best represents the constitutional design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
