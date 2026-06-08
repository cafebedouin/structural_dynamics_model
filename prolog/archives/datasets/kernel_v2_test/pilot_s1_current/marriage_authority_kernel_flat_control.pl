% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel_flat_control
 *   human_readable: Legitimacy Basis for Marriage Authority in Indian Constitutional Framework
 *   domain: constitutional_law/religious_governance/family_law
 *
 * SUMMARY:
 *   The Indian Constitution's legitimacy basis for marriage, divorce,
 *   inheritance, and custody adjudication rests on an unresolved structural
 *   contradiction: Articles 14-15 guarantee equality regardless of religion,
 *   while Article 25 protects the right to practice religion and preserve
 *   community autonomy. Article 44 directs the state to enact a Uniform Civil
 *   Code, but this mandate remains unenacted for over 75 years. Instead,
 *   personal laws (Hindu Marriage Act, Muslim Personal Law, Christian
 *   Marriage Act, Sikh Gurdwara Act) govern family matters by religious
 *   affiliation, delegating state authority to religious institutions and
 *   traditional authorities. The constraint extracts from those whose
 *   identity or life choices diverge from religious law prescriptions —
 *   primarily women (restricted divorce, unequal inheritance), same-sex
 *   couples (unrecognized marriage), and citizens seeking secular legal
 *   frameworks. Simultaneously, the system coordinates genuine functions: it
 *   provides predictable marriage and inheritance frameworks, respects
 *   community autonomy, and reduces state enforcement burden. The state
 *   judiciary is caught between enforcement of personal law (preserving
 *   religious autonomy) and enforcement of constitutional equality
 *   (protecting individual rights). The theater ratio has increased over time
 *   (0.30 → 0.48) as the gap between constitutional aspiration (UCC) and
 *   institutional reality (unenacted mandate) has widened. The suppression
 *   requirement peaked in 1985 (Shah Bano case, 0.65) when the Supreme Court
 *   briefly moved toward equality, then stabilized (0.62) as political
 *   backlash restored personal law primacy. Extractiveness has risen (0.45 →
 *   0.58) as legal gaps have accumulated: same-sex couples have no marriage
 *   recognition; transgender persons lack identity-coherent personal law
 *   frameworks; women's property rights remain subordinate; religious
 *   minorities within religious communities (e.g., Dalit Hindus) face
 *   compounded subordination.
 *
 * KEY AGENTS:
 *   - Women in personal law jurisdictions (powerless/trapped) — bear unequal property rights, restricted divorce, child custody subordination; no viable exit except conversion or relocation
 *   - Same-sex couples (powerless/identity-locked) — structurally mobile but identity-fused with religious communities whose personal law excludes them; denied marriage recognition, inheritance rights, joint custody
 *   - Religious authority institutions (institutional/arbitrage) — benefit from constitutionally guaranteed jurisdiction; arbitrage between religious law and secular courts; bear no extraction costs
 *   - State judiciary (institutional/constrained) — enforces personal law despite constitutional equality mandates; constrained by precedent and political backlash; active enforcement required
 *   - Secular citizens and religious minorities (moderate/constrained) — experience both coordination (predictable legal frameworks) and extraction (forced religious jurisdiction over non-religious choice)
 *   - Uniform Civil Code mandate (institutional/theatrical) — nominally directs secularization but remains unenacted; performative aspiration without institutional backing
 *   - Analytical observer (analytical/analytical) — risks naturalizing a contingent constitutional compromise as an immutable structural fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel_flat_control, 0.58).
domain_priors:suppression_score(marriage_authority_kernel_flat_control, 0.62).
domain_priors:theater_ratio(marriage_authority_kernel_flat_control, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel_flat_control, "Legitimacy Basis for Marriage Authority in Indian Constitutional Framework").
narrative_ontology:topic_domain(marriage_authority_kernel_flat_control, "constitutional_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(marriage_authority_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(marriage_authority_kernel_flat_control, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, religious_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, state_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, patriarchal_inheritance_structures).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, women).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, religious_minorities).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, same_sex_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, secular_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN BOUND BY PERSONAL LAW (SNARE) — Trapped within personal law jurisdiction determined by birth religion, with no exit to secular law for marriage dissolution, inheritance, or custody. The constraint extracts unpaid labor, restricted property rights, and custody vulnerability. Suppression is structural: religious law subordinates women's agency; state law respects religious jurisdiction and refuses to hear secular alternatives. No viable exit short of religious conversion or leaving the country.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAME-SEX COUPLE (SNARE WITH IDENTITY LOCK) — Structurally mobile (could hypothetically relocate) but identity-locked: their identity as a couple is constituted through their religious community, yet that community's personal law does not recognize their marriage. The constraint extracts recognition denial, property insecurity, and custody vulnerability. Suppression is both structural (legal bars) and internalized (identity fusion with the same community whose law excludes them). Maximum experienced extraction from trapped/legal dimension; additional binding through identity lock.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: SECULAR CITIZEN / RELIGIOUS MINORITY (TANGLED ROPE) — Constrained by personal law jurisdiction they may not endorse. Experiences both coordination (personal law provides stable marriage and inheritance frameworks within their community) and extraction (the constraint enforces religious authority over secular choice and minority rights). Suppression is high: exit to secular law is formally possible but carries social/community cost. Mixed experience: genuine coordination function (predictable marriage rules) embedded in extractive authority structure.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RELIGIOUS AUTHORITY INSTITUTIONS (ROPE) — Benefit from constitutionally guaranteed jurisdiction over personal law. Experience the constraint as coordination: personal law preserves religious community autonomy and provides institutional stability. Can arbitrage between religious law and secular courts (invoking personal law when it serves authority interests). No material extraction from this perspective — the authority benefits from coordination function without bearing costs.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE JUDICIARY (TANGLED ROPE) — Experiences genuine coordination: personal law reduces state enforcement burden by delegating family matters to religious institutions. Also experiences extraction: forced to apply personal laws that violate constitutional equality guarantees; judiciary is constrained by precedent (Sharma v. Sharma, Shah Bano) and cannot unilaterally secularize. Active enforcement required: courts must compel compliance with personal law provisions. Constrained by constitutional contradiction between Articles 25 (religious freedom) and 14-15 (equality).
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: UNIFORM CIVIL CODE MANDATE (PITON) — Article 44 directs the state to enact a Uniform Civil Code, but the mandate remains performative: no UCC has been enacted; instead, personal law persists through constitutional convention (Article 25). The constitutional aspiration is theatrical — invoked in policy documents, academic discourse, and political rhetoric but without institutional implementation. Theater ratio is moderate (0.48) because some performativity exists but the coordination function of personal law is real and functional. The UCC mandate is maintained through inertia despite structural conflict with Article 25.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational lens, the personal law framework appears as an unchangeable structural feature of post-colonial constitutional design: the Indian Constitution embedded religious autonomy as the foundational legitimacy compromise that enabled post-independence nation-building. The constraint appears as a natural settlement of irreconcilable demands (Hindu, Muslim, Christian, Sikh communities each requiring legal autonomy). However, the structural data reveals this as a false summit: the personal law system is maintained through active enforcement, benefits identifiable institutional actors, and suppresses alternatives (secular law, UCC). The 'immutability' is performative, not natural.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_authority_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(marriage_authority_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts from women, same-sex couples, and secular citizens through unequal legal status, restricted exit options, and forced religious jurisdiction over intimate decisions. The value is moderate-to-high, not maximal (0.70+), because coordination functions (stable marriage frameworks, community autonomy) provide real benefits that offset some extraction. The 0.13-point rise from 1947 (0.45) reflects accumulating legal gaps as constitutional evolution (equality jurisprudence) has outpaced personal law reform. Suppression (0.62): Structural barriers include legal bars to secular marriage/divorce, apostasy laws in some states, social penalties for conversion, and community enforcement (ostracism, honor-based violence). Suppression is not total because some agents can exit (convert, relocate, use legal ambiguities), so the trapped exit_options value (0.62) is justified but not maximal. Theater ratio (0.48): The Uniform Civil Code mandate is theatrical — 75 years of constitutional aspiration without implementation. The performativity is moderate, not dominant (0.70+), because the personal law system IS functional and not purely theatrical; the theater is in the gap between constitutional promise and institutional delivery, not in the constraint's daily operation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Religious authorities see Rope (pure coordination, no extraction, community autonomy preserved). Women in that same jurisdiction see Snare (trapped, extraction maximal). The state judiciary sees Tangled Rope (genuine coordination function embedded in extractive structure). The secular citizen sees Tangled Rope (coordination benefits offset by forced religious jurisdiction). The same-sex couple sees Snare with identity lock (trapped AND identity-fused with the excluding community). The UCC mandate, viewed from the state, appears as Piton (aspirational intent undermined by institutional inertia). The civilizational analytical observer risks Mountain (naturalizing the constitutional compromise as immutable), but the structural data reveals this as a false summit: the constraint is actively maintained, benefits identifiable actors, and suppresses alternatives. The perspectival gaps are not measurement errors — they are features of an asymmetric constraint that extracts from some while coordinating for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Religious authorities are beneficiaries with arbitrage exit (d ≈ 0.1-0.2, full subsidy). Women are victims with trapped exit (d ≈ 0.9, full target). The state is a mixed beneficiary (coordination function, reduced enforcement burden) and partial victim (constitutional conflict), producing moderate d ≈ 0.5. Same-sex couples are victims with identity_locked exit (d ≈ 0.85), higher than merely trapped because the lock prevents even escape to secular alternatives within their identity frame. Secular citizens are ambiguous — they may benefit from some personal law provisions (inheritance stability) while suffering extraction (forced religious jurisdiction), producing constrained exit (d ≈ 0.6-0.7). The engine computes effective extraction (χ) by applying the sigmoid f(d) to ε, scaled by scope. At national scope, the amplification is modest (0.95-1.05 range); at global scope, verification barriers push amplification higher. High-d agents (women, same-sex couples) experience χ approaching ε even before scope amplification. Low-d agents (religious authorities) experience negative χ (subsidy from coordination).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: the founding mandate was to preserve religious autonomy in a pluralist post-colonial nation (legitimate original purpose). But the mandate has outlived its coordination function for large swaths of the population. Women, same-sex couples, and secular citizens experience personal law as pure extraction without coordination benefit — the 'autonomy' it preserves is religious authority's autonomy, not theirs. The state's response (neither repealing Article 25 nor enacting Article 44) is theatrical rather than resolving: 75 years of constitutional limbo maintains theater. The Piton classification of the UCC mandate correctly captures this — the mandate is maintained as a constitutional aspiration without institutional backing. The mandatrophy is not resolved because doing so requires choosing between irreconcilable constitutional commitments (religious autonomy vs. individual equality), and the political cost of that choice is high. The constraint persists through institutional inertia (personal law is functional enough to avoid crisis) and through active enforcement by religious authorities (leveraging state power to enforce personal law). The falsity of the mountain classification (natural law immutability) becomes clear when one observes that maintaining the constraint requires active suppression of alternatives (UCC never enacted) and active enforcement (state courts applying religious law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_authority_grounding,
    'Does the legitimacy of personal law jurisdiction rest on religious authority (lineage to pre-constitutional religious traditions) or state legitimacy (constitutional grant through Article 25)?',
    'Analysis of Indian constitutional history: does the Constitution create personal law or merely recognize pre-existing religious authority? If created by Constitution, the authority is state-derived and can be withdrawn. If merely recognized, the authority is independent and the Constitution is a constraint on the state.',
    'If state-created: personal law is contingent on constitutional structure and could be repealed via UCC. If pre-constitutional: personal law has independent legitimacy and UCC would require explicit religious consent or constitutional amendment. Classification from the analytical observer perspective shifts from false summit to genuine mountain if pre-constitutional authority is established.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_authority_grounding, conceptual, 'Whether personal law authority derives from Constitution or pre-constitutional religious tradition').

omega_variable(
    equality_override_threshold,
    'At what threshold of rights violation does the state''s constitutional duty (Articles 14-15 equality) override its duty to respect religious autonomy (Article 25)?',
    'Comparative case law: Shah Bano (1985, equality favored), Sabarimala (2018, autonomy favored), recent transgender marriage cases. Identify the threshold where courts have shifted from deference to religious authority to imposing constitutional equality.',
    'If equality override threshold is low: personal law system is unstable, classification shifts toward snare across more perspectives. If threshold is high: personal law system has strong constitutional backing, classification remains tangled_rope with stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equality_override_threshold, empirical, 'Threshold for equality rights overriding religious autonomy in court decisions').

omega_variable(
    exit_sufficiency_of_conversion,
    'Does religious conversion constitute a genuine exit option from personal law, or is conversion itself constrained (social cost, formal barriers, apostasy risk)?',
    'Empirical analysis: rates of conversion to exit personal law; documentation of apostasy laws in states (criminal penalties for conversion from Islam, Christianity, Sikhism); social cost data from post-conversion households.',
    'If conversion is a low-cost exit: exit_options should be upgraded from trapped to constrained for more agents, reducing effective extraction. If conversion is socially expensive or legally barred: trapped classification is justified and extraction values increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_sufficiency_of_conversion, empirical, 'Whether religious conversion constitutes a genuine low-cost exit from personal law').

omega_variable(
    mandate_mortality,
    'Is the Uniform Civil Code mandate (Article 44) dead (no realistic path to implementation), alive (political momentum for enactment), or contested (active dispute among political parties and religious communities)?',
    'Political timeline: state-level UCC enactment attempts (Goa, Gujarat, Uttarakhand); national political commitments; court rulings on UCC feasibility. Presence/absence of bill drafting, legislative action, and political party alignment.',
    'If mandate is dead: scaffold perspective is weakened, UCC is purely theatrical (piton classification confirmed). If mandate is alive: sunset clause logic strengthens for scaffold perspectives. If contested: the constraint remains unstable, classification bifurcates across political coalitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_mortality, empirical, 'Viability and political momentum for Uniform Civil Code implementation').

omega_variable(
    women_agency_within_personal_law,
    'Can women''s agency within personal law (through reform movements, reformed interpretations, women''s courts) substantially reduce the extractiveness of personal law, or is the subordination built into the institutional structure?',
    'Comparative analysis: reformed personal law statutes (Goa Maintenance of Dependents Act vs. Hindu Succession Act); women''s reform movements within religious communities; documented outcomes of women''s courts and legal literacy programs.',
    'If agency-within-system reduces extraction substantially: extractiveness value should be lower (0.35-0.45 range). If subordination is structural: extractiveness remains high (0.55-0.65). Classification of women''s perspective shifts from snare toward tangled_rope if agency proves effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_agency_within_personal_law, empirical, 'Whether women''s agency within personal law can substantially reduce its extractiveness').

omega_variable(
    religious_community_heterogeneity,
    'Do all members of a religious community experience the same extraction from personal law, or do internal hierarchies (caste, sect, gender, class) create divergent extractiveness values within the same religious group?',
    'Ethnographic and case-law analysis: outcomes for Dalit women under Hindu law, marginalized sects under Muslim law, lower-caste Sikhs under Sikh law. Documentation of internal power hierarchies and how personal law entrenches them.',
    'If high heterogeneity: separate constraint stories needed for each sub-group (ε-invariance decomposition). If homogeneous: single story per religious community is justified. Current story aggregates across heterogeneity; omega surfaces the buried differentiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_community_heterogeneity, empirical, 'Degree of extractiveness heterogeneity within religious communities under personal law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel_flat_control, 1947, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_theater_1947, marriage_authority_kernel_flat_control, theater_ratio, 1947, 0.3).
narrative_ontology:measurement(marr_theater_1985, marriage_authority_kernel_flat_control, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(marr_theater_2005, marriage_authority_kernel_flat_control, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(marr_theater_2023, marriage_authority_kernel_flat_control, theater_ratio, 2023, 0.48).

% Extraction over time
narrative_ontology:measurement(marr_extractiveness_1947, marriage_authority_kernel_flat_control, base_extractiveness, 1947, 0.45).
narrative_ontology:measurement(marr_extractiveness_1985, marriage_authority_kernel_flat_control, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(marr_extractiveness_2005, marriage_authority_kernel_flat_control, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(marr_extractiveness_2023, marriage_authority_kernel_flat_control, base_extractiveness, 2023, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_suppression_1947, marriage_authority_kernel_flat_control, suppression_requirement, 1947, 0.55).
narrative_ontology:measurement(marr_suppression_1985, marriage_authority_kernel_flat_control, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(marr_suppression_2005, marriage_authority_kernel_flat_control, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(marr_suppression_2023, marriage_authority_kernel_flat_control, suppression_requirement, 2023, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel_flat_control, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, hindu_succession_gender_asymmetry).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, muslim_marriage_unilateral_divorce).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, transgender_identity_recognition).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, goa_uniform_civil_code_implementation).

% DUAL FORMULATION NOTE:
% This story captures the legitimacy basis (constitutional kernel) for personal law adjudication. Downstream stories capture specific personal law regimes (Hindu, Muslim, Christian, Sikh) and their particular extraction mechanisms. This upstream story establishes the constraint that permits those downstream extractions to persist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel_flat_control, powerless, 0.88).
constraint_indexing:directionality_override(marriage_authority_kernel_flat_control, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
