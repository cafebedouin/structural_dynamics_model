% ============================================================================
% CONSTRAINT STORY: reservation_architecture__substantive_equality_engine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reservation_architecture__substantive_equality_engine_reading, []).

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
 *   constraint_id: reservation_architecture__substantive_equality_engine_reading
 *   human_readable: Reservation Architecture as Substantive Equality Engine
 *   domain: constitutional_law/affirmative_action
 *
 * SUMMARY:
 *   The substantive-equality reading of the reservation architecture asserts
 *   that in a stratified society, formal equality (identical treatment
 *   regardless of starting condition) perpetuates the inherited hierarchy it
 *   claims to remedy. Therefore, the constitution must 'lift to level' —
 *   providing differential treatment to offset structural disadvantage. This
 *   reading interprets Articles 15(4) and 16(4) of the Indian Constitution
 *   not as narrow exceptions to formal equality but as a foundational
 *   alternative to formal equality: substantive equality is what the
 *   Constitution actually mandates when stratification is the baseline
 *   condition. The constraint embeds this reading as a constitutional
 *   mechanism that rebalances inherited advantage through reservation
 *   allocation across educational access, civil service employment, and
 *   political representation. The reading faces two structural competitors:
 *   the creamy-layer doctrine reading (which narrows the beneficiary set to
 *   the genuinely disadvantaged and claims this preserves the remedial logic)
 *   and the Mandal-expansion reading (which treats the 1989 Mandal Commission
 *   expansion as doctrinal capture by organized OBC majoritarian politics,
 *   transforming rescue into allocation). This constraint story instantiates
 *   only the substantive-equality reading, modeling it as a coherent tangled
 *   rope that combines genuine coordination (rebalancing structural
 *   disadvantage) with extraction (doctrinal suppression of formal equality,
 *   perpetual redefinition of 'backward class,' and the performance burden on
 *   beneficiaries). The measurement trajectory shows rising extractiveness
 *   and suppression over 30 years (1989-2019), reflecting the Mandal
 *   expansion and subsequent debates over OBC inclusion.
 *
 * KEY AGENTS:
 *   - Historically Excluded Communities (SC/ST initially, OBC post-Mandal): Primary victim (powerless/trapped) — bears structural disadvantage and the perpetual rebalancing requirement; also identified as primary beneficiary of the substantive-equality doctrine itself, creating internal tension
 *   - Individual Reservation Beneficiaries: Secondary agent (moderate/constrained) — gains access but experiences institutional extraction and perpetual 'exception' status performance burden
 *   - Substantive Equality Jurisprudence: Primary beneficiary (institutional/arbitrage) — gains doctrinal authority and interpretive power through the reservation mechanism
 *   - Merit-as-Neutral Claimants: Secondary victim (powerful/constrained) — experience doctrinal suppression of their formal-equality frame; see their claim recast as inherited privilege
 *   - Formal Equality Doctrine: Institutional victim (institutional/arbitrage) — functionally degraded by substantive-equality jurisprudence while maintaining performative presence in constitutional text
 *   - Creamy Layer Doctrine Coalition: Organized challenger (organized/mobile) — advocates narrow targeting but constrained by substantive-equality jurisprudence that may override their targeting logic
 *   - Constitutional Authority (Indian): Institutional framework (institutional/arbitrage) — implements substantive-equality reading while maintaining discretionary power over reservation scope and definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reservation_architecture__substantive_equality_engine_reading, 0.52).
domain_priors:suppression_score(reservation_architecture__substantive_equality_engine_reading, 0.58).
domain_priors:theater_ratio(reservation_architecture__substantive_equality_engine_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reservation_architecture__substantive_equality_engine_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reservation_architecture__substantive_equality_engine_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reservation_architecture__substantive_equality_engine_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reservation_architecture__substantive_equality_engine_reading, tangled_rope).
narrative_ontology:human_readable(reservation_architecture__substantive_equality_engine_reading, "Reservation Architecture as Substantive Equality Engine").
narrative_ontology:topic_domain(reservation_architecture__substantive_equality_engine_reading, "constitutional_law/affirmative_action").

domain_priors:requires_active_enforcement(reservation_architecture__substantive_equality_engine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reservation_architecture__substantive_equality_engine_reading, 'e66683f8-8817-4a92-a55c-7e1eec026ca5').
narrative_ontology:cs_kernel_codification('e66683f8-8817-4a92-a55c-7e1eec026ca5', formalized).
narrative_ontology:cs_authority_grounding('e66683f8-8817-4a92-a55c-7e1eec026ca5', lineage).
narrative_ontology:cs_interpretation_layer_present('e66683f8-8817-4a92-a55c-7e1eec026ca5').
narrative_ontology:cs_reading_relation('e66683f8-8817-4a92-a55c-7e1eec026ca5', reservation_architecture__creamy_layer_doctrine_reading, influences).
narrative_ontology:cs_reading_relation('e66683f8-8817-4a92-a55c-7e1eec026ca5', reservation_architecture__mandal_expansion_reading, coexists_with).
narrative_ontology:cs_axiom('e66683f8-8817-4a92-a55c-7e1eec026ca5', foundational, stratification_makes_formal_equality_unjust).
narrative_ontology:cs_axiom_status(stratification_makes_formal_equality_unjust, holdable).
narrative_ontology:cs_axiom_grounding('e66683f8-8817-4a92-a55c-7e1eec026ca5', stratification_makes_formal_equality_unjust, deontological).
narrative_ontology:cs_axiom('e66683f8-8817-4a92-a55c-7e1eec026ca5', foundational, rebalancing_inherited_advantage_is_constitutional_mandate).
narrative_ontology:cs_axiom_status(rebalancing_inherited_advantage_is_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e66683f8-8817-4a92-a55c-7e1eec026ca5', rebalancing_inherited_advantage_is_constitutional_mandate, conventional).
narrative_ontology:cs_reference_frame('e66683f8-8817-4a92-a55c-7e1eec026ca5', substantive_equality_as_constitutional_baseline).
narrative_ontology:cs_drift_state('e66683f8-8817-4a92-a55c-7e1eec026ca5', post_mandal_commission_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e66683f8-8817-4a92-a55c-7e1eec026ca5', '').
narrative_ontology:cs_kernel_id(reservation_architecture__substantive_equality_engine_reading, reservation_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reservation_architecture__substantive_equality_engine_reading, substantive_equality_jurisprudence).
narrative_ontology:constraint_beneficiary(reservation_architecture__substantive_equality_engine_reading, historically_excluded_communities).
narrative_ontology:constraint_victim(reservation_architecture__substantive_equality_engine_reading, merit_as_neutral_claimants).
narrative_ontology:constraint_victim(reservation_architecture__substantive_equality_engine_reading, formal_equality_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY EXCLUDED COMMUNITIES (SNARE) — Trapped within inherited disadvantage. Even with reservations, structural barriers (resource poverty, educational debt, social stigma, family social capital deficit) persist beyond the quota window. The reservation mechanism itself creates extraction: beneficiaries must perform 'deserving' narratives within institutional frameworks that maintain the very stratification the reservation nominally corrects. The 'lifting to level' is perpetually deferred.
constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESERVATION BENEFICIARY — INDIVIDUAL (TANGLED ROPE) — Gains access (genuine coordination benefit: opportunity to compete) but experiences extraction within that access. Must navigate institutional environments (legal education, civil service, medical school) designed for and by the historically dominant. Bears psychological and social costs of perpetual 'exception' status. Constrained by the requirement to justify the reservation through exceptional performance — the cost of exit is reversion to inherited disadvantage.
constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUBSTANTIVE EQUALITY JURISPRUDENCE (ROPE) — The reading itself (as a doctrinal framework, not as individual justices) benefits from the reservation architecture. It gains institutional legitimacy, case volume, and constitutional authority. Experiences the constraint as pure coordination: the mechanism implements the doctrine's core claim that formal equality perpetuates hierarchy. The jurisprudence has institutional arbitrage — it can navigate between strict scrutiny and rational basis review, shape legislative definitions of 'backward class,' and generate case law.
constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MERIT-AS-NEUTRAL CLAIMANTS (SNARE) — Experience maximum extraction: their formal claim to neutral merit-based selection is suppressed by the reservation mechanism. Suppression includes legal doctrine (substantive equality overrides formal equality), institutional practice (quota enforcement), and narrative frame (their merit claim is recast as inherited privilege). High χ from powerful position combined with strong suppression. The constraint's extraction mechanism operates by delegitimizing their framing.
constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL EQUALITY DOCTRINE (PITON) — The doctrine persists through institutional inertia and constitutional lip service, but its functional force in the reservation domain is substantially degraded. Courts invoke formal equality while implementing substantive equality. The doctrine maintains performative presence in constitutional text ('equality before law') while being hollowed by jurisprudential practice. Theater ratio high because formal-equality rhetoric persists despite substantive-equality implementation.
constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CREAMY LAYER DOCTRINE PROPONENTS (TANGLED ROPE) — Organized actors defending the 'creamy layer' exclusion (advanced members of backward class excluded from reservation benefit). They see a genuine coordination function (targeting benefit to those who truly need it) combined with extraction through definitional power (who counts as 'advanced'? who measures intergenerational mobility?). Mobile because they can influence judicial outcomes and legislative definition, but constrained by substantive-equality jurisprudence that may override their targeting logic.
constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL AUTHORITY — SUBSTANTIVE READING (TANGLED ROPE) — The reading's own institutional perspective. Implements substantive equality through the reservation mechanism (genuine coordination: rebalancing inherited advantage). But the mechanism also extracts through perpetual redefinition of what counts as 'backward class,' through legislative capture by organized OBC interests (post-Mandal), and through the requirement that beneficiaries perform exceptional merit to justify their access. The institution benefits from doctrinal authority and the discretionary power to adjust reservation scope.
constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — NATURAL LAW READING (MOUNTAIN) — From the universal/civilizational perspective, the constraint appears as an immutable structural feature: in a stratified society, identical treatment of unequals perpetuates hierarchy, therefore some form of differential treatment is logically necessary. This perspective risks naturalizing what is a contingent doctrinal choice. The false summit detector will identify this as the naturalization of substantive-equality doctrine itself, not as a law of nature.
constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reservation_architecture__substantive_equality_engine_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reservation_architecture__substantive_equality_engine_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reservation_architecture__substantive_equality_engine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reservation_architecture__substantive_equality_engine_reading, TR),
    TR >= 0.70.

:- end_tests(reservation_architecture__substantive_equality_engine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The substantive-equality reading generates extraction through doctrinal suppression (formal equality is hollowed), through perpetual redefinition (what counts as 'backward class'? this expanded from SC/ST to OBC post-Mandal, and remains contested), and through performance burden (beneficiaries must demonstrate they 'deserved' the access, justifying the deviation from formal equality). However, extractiveness is not as severe as pure suppression of merit (which would be ε ≥ 0.70) because the mechanism includes genuine coordination: rebalancing inherited advantage is a legitimate constitutional goal, and access to education, employment, and political representation are real benefits. The measurement trajectory shows rising extractiveness: the original substantive-equality framing (1950s-1970s) had lower extractiveness because the beneficiary set was narrower (only SC/ST) and the doctrine's remedial intent was clearer. The Mandal expansion (1989+) increased extractiveness because the beneficiary set widened to include OBC (now majority), transforming the reading from rescue to allocation, and because suppression of merit-as-neutral increased as formal-equality doctrine was more actively marginalized. Suppression (0.58): Moderate-high. The constraint operates by suppressing alternatives: formal-equality doctrine is doctrinally marginalized, merit-as-neutral claims are delegitimized, and competing readings (creamy-layer, Mandal-skeptics) are constrained by the substantive-equality jurisprudence. Suppression includes legal doctrine (strict scrutiny is relaxed for reservation regulations), institutional practice (quota enforcement with limited judicial review), and narrative frame (any opposition to reservations is recast as defense of inherited privilege). Theater ratio (0.48): Moderate. The reservation mechanism includes both functional and performative elements. Functional: it actually allocates access (this is real coordination). Performative: the perpetual redefinition of 'backward class,' the ritual invocation of formal-equality language while implementing substantive equality, and the requirement that beneficiaries perform 'deserving' narratives all constitute theater. Rising theater (0.32 → 0.48) reflects increasing performance burden and definitional complexity as the beneficiary set expanded and organized interests captured the expansion agenda.
 *
 * PERSPECTIVAL GAP:
 *   All eight perspectives operate on the same structural object — the reservation architecture and its substantive-equality doctrinal justification — but produce different classifications because the agent's position determines their experience of extraction. The historically excluded see snare (trapped, powerless, no exit from inherited disadvantage). Individual beneficiaries see tangled rope (constrained by performance requirements, but gaining access). Substantive-equality jurisprudence sees rope (pure coordination: implementing its doctrine). Merit-as-neutral see snare (suppression of their frame). Formal equality sees piton (degraded by jurisprudence, maintained by performative constitutional language). Creamy-layer proponents see tangled rope (coordination function in targeting, extraction through definitional power). Constitutional authority sees tangled rope (implementing doctrine, exercising discretionary power). The analytical observer risks seeing mountain (immutable structural feature: hierarchy requires differential treatment) — a false summit naturalizing the substantive-equality reading itself as law rather than contingent doctrinal choice. The widest gaps are between rope (beneficiary jurisprudence) and snare (trapped communities, merit-as-neutral claimants) — a gap of three type classes, suggesting the constraint is not a purely extractive snare but a hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading's directionality structure reveals why different agents classify the same constraint differently. Substantive-equality jurisprudence (beneficiary, institutional, arbitrage) experiences low effective extraction because it gains doctrinal authority and discretionary power — d ≈ 0.15. Merit-as-neutral claimants (victim, powerful, constrained) experience high extraction despite their power because suppression of their formal-equality frame is strong and they have limited exits — d ≈ 0.85. Historically excluded communities (victim and beneficiary simultaneously — this tension is irreducible) experience moderate-high extraction despite gaining access because the structural disadvantage persists beyond the reservation window and they bear performance burden — d ≈ 0.70. Formal-equality doctrine (victim, institutional, arbitrage) experiences low effective extraction in the narrow sense (the doctrine persists institutionally) but high functional extraction (its constitutional mandate is hollowed) — this incoherence is captured by the piton classification and omega ambiguity about whether suppression is doctrinal evolution or principle destruction. The creamy-layer coalition (organized, mobile, beneficiary of definitional power but constrained by the overarching substantive-equality jurisprudence) experiences moderate extraction — d ≈ 0.50.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the substantive-equality reading and its competitors (creamy-layer, Mandal-expansion skepticism) are distinct constraints with different ε values, not different perspectives on the same constraint. The substantive-equality reading (ε ≈ 0.52, Tangled Rope) combines genuine coordination (rebalancing inherited advantage) with extraction (doctrinal suppression, perpetual redefinition, performance burden). A creamy-layer reading (not generated here) would have lower ε (better targeting, less definitional drift). A Mandal-skeptic reading would have higher ε and higher suppression (treating OBC expansion as doctrinal capture and majoritarian extraction). The mandatrophy is avoided by recognizing these as siblings in a kernel contest, each with its own ε-invariant classification, linked through network relationships rather than collapsed into a single 'reservation architecture' story. The analytical observer's mountain-reading (hierarchy requires differential treatment) is a false summit: it naturalizes the substantive-equality doctrinal choice rather than seeing it as one contingent reading among competing alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generational_trajectory_threshold,
    'Over how many generations does substantive-equality rebalancing need to operate before inherited disadvantage is genuinely remedied rather than perpetually recalibrated?',
    'Longitudinal data on educational attainment, income mobility, and social capital accumulation across cohorts of reservation beneficiaries and their descendants; comparison with non-beneficiary populations controlling for initial starting conditions',
    'If remediation occurs within 2-3 generations: substantive equality is genuine coordination with a sunset horizon (moves toward Scaffold). If the requirement for rebalancing persists indefinitely: the constraint is extraction masquerading as remediation (stays Tangled Rope or moves toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_trajectory_threshold, empirical, 'Generational timeframe for substantive-equality remediation').

omega_variable(
    creamy_layer_targeting_precision,
    'Does the creamy layer doctrine actually target benefit to the genuinely disadvantaged, or does it primarily serve to narrow the beneficiary class and reduce resource claims on the state?',
    'Analysis of exclusion patterns: proportion of OBC and SC/ST applicants excluded by creamy layer definitions; correlation between creamy layer application and state fiscal capacity; legislative debate records on creamy layer expansion/contraction',
    'If targeting works: creamy layer is legitimate internal refinement of substantive equality (Rope). If primarily fiscal containment: creamy layer is extraction mechanism within the reservation apparatus (Snare from the perspective of excluded-but-disadvantaged agents).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creamy_layer_targeting_precision, empirical, 'Whether creamy layer doctrine targets benefit or contains costs').

omega_variable(
    mandate_expansion_majoritarian_capture,
    'Has the Mandal expansion (OBC reservations, 50% ceiling, caste enumeration) transformed substantive equality from a doctrine of rescue for the severely excluded into a majoritarian political allocation mechanism?',
    'Historical analysis: pre/post-Mandal beneficiary populations, representation rates of OBC vs SC/ST, legislative voting patterns on further expansion, fiscal impact on general-category access',
    'If Mandal represents genuine expansion of substantive equality: the reading adapts to broader beneficiary set while maintaining the doctrine (Rope). If Mandal represents doctrinal capture by organized majority interests: substantive equality has been instrumentalized for competitive majority politics (Snare from formal-equality perspective; Piton from original anti-caste rescue perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_expansion_majoritarian_capture, conceptual, 'Whether Mandal expansion represents doctrinal extension or majoritarian capture').

omega_variable(
    inherited_advantage_quantification,
    'What structural deficit (educational, economic, social capital) constitutes ''unequal starting condition'' that justifies rebalancing through reservation, and how is this deficit measured or presumed?',
    'Comparative analysis of different quantification approaches (group-based presumption via caste, income-based means testing, educational-debt measurement); examination of whether the presumption is empirically validated or ceremonially maintained',
    'If deficit is measurable and individual: substantive equality is a coherent doctrine rebalancing structural disadvantage (Rope). If deficit is presumed by group status without individual validation: substantive equality is operating through group-based extraction allocation (Tangled Rope with hidden victim set).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherited_advantage_quantification, empirical, 'How inherited disadvantage is measured or presumed in reservation allocation').

omega_variable(
    formal_equality_suppression_mechanism,
    'Is the suppression of formal-equality doctrine in reservation jurisprudence itself a legitimate doctrinal evolution, or does it represent the hollowing of a foundational principle?',
    'Examination of court opinions: frequency and weight given to formal-equality arguments; whether formal equality survives as limiting principle (preventing unlimited group-based allocation) or is entirely displaced by substantive-equality framing',
    'If formal equality retains limiting force: the doctrine has evolved while maintaining internal constraints (Rope or Tangled Rope with self-correction). If formal equality is suppressed entirely: substantive equality becomes unlimited group-based allocation (Snare risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_equality_suppression_mechanism, conceptual, 'Whether formal-equality suppression is doctrinal evolution or principle hollowing').

omega_variable(
    reading_committer_ambiguity,
    'Is this substantive-equality reading a genuine doctrinal position held by the Indian judiciary and Indian Constitution, or is it a post-hoc rationalization imposed by particular coalitions?',
    'Historical record: Supreme Court opinions (especially foundational cases like Kesavananda Bharati, Mandal Commission judgment), constitutional assembly debates, political movements advocating substantive equality vs formal equality',
    'If genuine reading: the constraint models how the Indian Constitution actually operates (descriptive accuracy). If coalition rationalization: the constraint models a particular doctrinal strategy that competes with other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Committer: Is substantive equality a genuine constitutional reading or coalition strategy?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reservation_architecture__substantive_equality_engine_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resv_subst_eq_tr_t0, reservation_architecture__substantive_equality_engine_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(resv_subst_eq_tr_t15, reservation_architecture__substantive_equality_engine_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(resv_subst_eq_tr_t30, reservation_architecture__substantive_equality_engine_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(resv_subst_eq_be_t0, reservation_architecture__substantive_equality_engine_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(resv_subst_eq_be_t15, reservation_architecture__substantive_equality_engine_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(resv_subst_eq_be_t30, reservation_architecture__substantive_equality_engine_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(resv_subst_eq_su_t0, reservation_architecture__substantive_equality_engine_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(resv_subst_eq_su_t15, reservation_architecture__substantive_equality_engine_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(resv_subst_eq_su_t30, reservation_architecture__substantive_equality_engine_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reservation_architecture__substantive_equality_engine_reading, resource_allocation).
narrative_ontology:affects_constraint(reservation_architecture__substantive_equality_engine_reading, reservation_architecture__creamy_layer_doctrine_reading).
narrative_ontology:affects_constraint(reservation_architecture__substantive_equality_engine_reading, reservation_architecture__mandal_expansion_reading).
narrative_ontology:affects_constraint(reservation_architecture__substantive_equality_engine_reading, formal_equality_jurisprudence).
narrative_ontology:affects_constraint(reservation_architecture__substantive_equality_engine_reading, merit_as_neutral_principle).

% DUAL FORMULATION NOTE:
% The reservation architecture kernel decomposes into three constraint stories, each with distinct ε values reflecting different doctrinal readings: substantive_equality_engine_reading (ε=0.52, Tangled Rope — coordination + extraction), creamy_layer_doctrine_reading (ε≈0.35, expected as Rope or low Tangled Rope — tighter targeting, less extraction), mandal_expansion_reading (ε≈0.68, expected as Snare or high Tangled Rope — majoritarian capture, higher extraction). The three stories are linked via network.affects_constraints because they compete within a single constitutional kernel and because changes in one reading's doctrinal authority (e.g., Supreme Court reinterpreting 'backward class') directly affect the structural parameters of the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reservation_architecture__substantive_equality_engine_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
