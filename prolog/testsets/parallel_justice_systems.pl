% ============================================================================
% CONSTRAINT STORY: parallel_justice_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parallel_justice_systems, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: parallel_justice_systems
 *   human_readable: Parallel Justice Systems and Legal Pluralism
 *   domain: law/political/social
 *
 * SUMMARY:
 *   Parallel justice systems arise when formal state law and
 *   customary/religious/community-based dispute resolution coexist with
 *   overlapping jurisdiction. This constraint structures legal access,
 *   authority legitimacy, and extraction mechanisms across multiple agent
 *   classes: marginalized populations trapped in informal systems, mobile
 *   populations navigating both, state apparatuses benefiting from outsourced
 *   dispute resolution, customary authorities maintaining social cohesion
 *   while exercising asymmetric power, reform coalitions seeking integration,
 *   and international frameworks performing opposition while tolerating
 *   pluralism. The constraint exhibits tangled rope characteristics at the
 *   core — genuine coordination benefits (customary systems provide local
 *   legitimacy and fill formal court capacity gaps) coexist with asymmetric
 *   extraction (marginalized populations, particularly women, lack exit
 *   options and are subject to authority decisions without appeal). The
 *   extractiveness trajectory shows degradation over the measurement interval
 *   as international human rights pressure increases without producing
 *   institutional change, leading to performative theater (formal opposition
 *   + practical tolerance) and proliferation of unfulfilled reform
 *   frameworks. This is a key structural example of how legal pluralism
 *   naturalizes marginalization through legitimacy claims.
 *
 * KEY AGENTS:
 *   - Marginalized Populations: Primary victims (powerless/trapped) — lack literacy, resources, or geographic proximity to formal courts; trapped in informal systems with no appeal mechanism
 *   - Mobile Disputants: Secondary victims (moderate/constrained) — can navigate both systems but face high switching costs (social ostracization risk); forced choice between legitimacy and legal protection
 *   - State Legal Apparatus: Primary beneficiary (institutional/arbitrage) — outsources dispute resolution to customary authorities, avoiding cost of universal formal court access while maintaining sovereignty claims
 *   - Customary Authority (Elders/Chiefs/Religious Leaders): Mixed actor (powerful/constrained) — gains legitimacy and enforcement power from parallel system but constrained by state law's residual threat; often benefits extractively from power to judge within community
 *   - Legal Pluralism Reform Coalition: Organized agents (organized/constrained) — human rights groups, development organizations, bar associations pushing for hybrid tribunals and customary law codification; see sunset in formalization
 *   - International Legal Framework: Institutional performer (institutional/arbitrage) — formally opposes parallel systems as human rights violations but practically tolerates them; maintains arbitrage between declaring commitment and accepting pragmatic reality
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing dual systems as inevitable state-formation feature rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parallel_justice_systems, 0.58).
domain_priors:suppression_score(parallel_justice_systems, 0.65).
domain_priors:theater_ratio(parallel_justice_systems, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parallel_justice_systems, extractiveness, 0.58).
narrative_ontology:constraint_metric(parallel_justice_systems, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(parallel_justice_systems, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parallel_justice_systems, tangled_rope).
narrative_ontology:human_readable(parallel_justice_systems, "Parallel Justice Systems and Legal Pluralism").
narrative_ontology:topic_domain(parallel_justice_systems, "law/political/social").

domain_priors:requires_active_enforcement(parallel_justice_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parallel_justice_systems, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(parallel_justice_systems, customary_authority_elders).
narrative_ontology:constraint_beneficiary(parallel_justice_systems, informal_dispute_settlers).
narrative_ontology:constraint_victim(parallel_justice_systems, marginalized_populations).
narrative_ontology:constraint_victim(parallel_justice_systems, dispute_subjects).
narrative_ontology:constraint_victim(parallel_justice_systems, legal_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPUTE SUBJECT (SNARE) — Powerless agent trapped in local jurisdiction. Cannot exit to formal courts due to geographic isolation, illiteracy, cost barriers, or cultural identity. Forced to accept outcomes of customary dispute resolution with no appeal mechanism. Maximum extraction through lack of alternatives and enforced reliance on coercive authority.
constraint_indexing:constraint_classification(parallel_justice_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MOBILE DISPUTANT (TANGLED ROPE) — Agent with some mobility (urban dweller, educated, resource access) can in principle navigate either system, but faces high switching costs. Customary system provides local legitimacy and social integration (coordination benefit); formal system provides legal recourse but risks social ostracization. Genuine coordination function alongside asymmetric extraction through forced choice between social belonging and legal protection.
constraint_indexing:constraint_classification(parallel_justice_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE LEGAL APPARATUS (ROPE) — Benefits from dual legitimacy: formal courts establish state sovereignty while tolerating customary systems to avoid resource burden of universal legal access. Experiences the constraint as coordination — outsourcing dispute resolution to informal authorities reduces demand on formal courts. Net beneficiary with minimal external pressure; can arbitrage between assertion of sovereignty and practical delegation.
constraint_indexing:constraint_classification(parallel_justice_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CUSTOMARY AUTHORITY (TANGLED ROPE) — Elder/chief/religious authority gains legitimacy and enforcement power from parallel system, but is structurally constrained by state legal system. Genuine coordination function: maintains social cohesion and dispute resolution in absence of formal courts. Asymmetric extraction: authority's judgment often favors male, senior, or wealthy community members. Must navigate tension between customary legitimacy and state law's residual threat.
constraint_indexing:constraint_classification(parallel_justice_systems, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEGAL PLURALISM REFORM COALITION (SCAFFOLD) — Organized agents (human rights groups, development organizations, progressive bar associations) view parallel systems as temporary, improvable through integration: standardized customary law codes, appeals to formal courts, hybrid tribunal structures, and women's protections in informal proceedings. See sunset in formalization of customary law and strengthened state capacity. Theater ratio is moderate because reform involves creating transparency within customary proceedings (codification, documentation, public hearings) rather than eliminating informal authority entirely.
constraint_indexing:constraint_classification(parallel_justice_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL FRAMEWORK (PITON) — Global human rights institutions, international courts, and foreign aid conditionality formally oppose parallel justice systems as inconsistent with rule of law and human rights. Yet this framework persists in tolerating dual systems as pragmatic reality: integration costs are high, enforcement is weak, and developed nations tolerate parallel systems in allies. Theater_ratio high because international opposition is performative — declarations of commitment to unified legal systems coexist with acceptance of parallel systems in practice.
constraint_indexing:constraint_classification(parallel_justice_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a state-formation perspective, some degree of legal pluralism is structurally inevitable during state consolidation: universal formal law enforcement requires infrastructure and capacity that developing states cannot provide immediately. Parallel systems emerge naturally as rational gap-filling. However, this mountain classification risks naturalizing what is actually contingent institutional choice — states could invest in universal formal courts if politically committed.
constraint_indexing:constraint_classification(parallel_justice_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parallel_justice_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parallel_justice_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parallel_justice_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parallel_justice_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parallel_justice_systems, TR),
    TR >= 0.70.

:- end_tests(parallel_justice_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The core extraction mechanism is the denial of exit options for powerless agents. Marginalized populations trapped in informal systems cannot appeal to formal courts due to cost, distance, literacy, or cultural barriers. This generates significant captured surplus for authorities and state apparatus. Customary authorities extract through biased adjudication (favoring male, senior, or wealthy disputants); state apparatus extracts through delegated governance without service delivery responsibility. The extractiveness increased over time (0.32→0.58) because international pressure has formalized the legitimacy claims surrounding parallelism, creating more sophisticated theater while material access disparities persist. Suppression (0.65): High. Multiple mechanisms maintain the constraint: geographic isolation and infrastructure failure (no courts available locally), economic barriers (formal litigation is expensive), linguistic barriers (formal law operates in state language often not spoken by marginalized groups), cultural conditioning (internalized belief that informal authority is legitimate), legal complexity (formal procedures require knowledge most disputants lack), and identity lock (community membership understood as requiring acceptance of customary authority). Theater ratio (0.68): High. The international human rights framework, formal legal codes that nominally protect dispute subjects, and national court systems that formally supersede customary law are all performative — they exist in law but not in practice for marginalized populations. Reform initiatives (legal aid, community courts, customary law codification) often become theater: announced but underfunded, creating appearance of reform without material improvement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates substantial perspectival divergence across the same base properties. The trapped disputant sees snare (no exit, full extraction). The customary authority sees tangled rope (coordination benefits alongside power extraction). The state sees rope (pure coordination, outsourced dispute resolution). The reform coalition sees scaffold (temporary problem with sunset through integration). The international framework sees piton (performative opposition masking pragmatic tolerance). The mountain classification from the analytical observer is a false summit — it naturalizes what is actually a contingent institutional arrangement (state capacity allocation, political will, and elite interests in maintaining informal control) as an immutable feature of state consolidation. This perspectival range is the signature diagnostic: tangled rope constraints generate wide perspectival gaps because they contain both genuine coordination (which some agents experience) and genuine extraction (which other agents bear).
 *
 * DIRECTIONALITY LOGIC:
 *   The primary directionality driver is exit options. Trapped agents (powerless/trapped) cannot exit to formal law regardless of preferences, generating maximum d and maximum experienced extraction. Constrained agents (moderate/constrained, customary authority/constrained, reform coalition/constrained) face high but surmountable costs to exit or reform, generating moderate d. Arbitrage agents (institutional/arbitrage state, international framework/arbitrage) have lowest d: they benefit from the dual system and can arbitrage between formal assertion and practical tolerance. The state apparatus specifically benefits from reduced demand on formal courts without losing sovereignty claims — this is a net-positive directionality outcome (low d), making rope classification from its perspective accurate. Customary authorities face an asymmetric constraint: they derive legitimacy and enforcement power from parallelism but are structurally vulnerable to state legal override — this creates constrained exit options and moderate d. The reform coalition sees this as a solvable coordination problem (sunset logic) despite their organizational power (organized/constrained), indicating their structural position is one of pushing against entrenched interests rather than wielding control over the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution in parallel justice systems requires distinguishing genuine coordination benefits from cover stories for extraction. The state and customary authorities each claim coordination: the state claims dual systems fill formal court capacity gaps; customary authorities claim they maintain social cohesion. Both claims are partially true. The mistake would be classifying the entire system as rope (pure coordination) or snare (pure extraction) without acknowledging that different agents experience different combinations. The tangled rope classification resolves mandatrophy by asserting that BOTH coordination and extraction are structurally present: formal law provides genuine dispute resolution capacity but is inaccessible to marginalized populations; customary systems genuinely organize community life but enforce asymmetric power. The perspectival gap (snare for trapped agents, rope for state) reveals that mandatrophy is resolved through indexicality: there is no single truth, only context-dependent structural readings. A single agent (say, a woman in a rural area) may simultaneously experience rope coordination (her family disputes are adjudicated through customary law that maintains social order) and snare extraction (she has no exit if the customary judgment is unjust, and appeal to formal law is inaccessible). The constraint is tangled rope from the systemic perspective precisely because it has this dual character: it coordinates some disputes and extracts from some agents, and these are often different groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    access_equity_tradeoff,
    'Does tolerance of parallel justice systems improve effective legal access for marginalized populations or merely legitimize their exclusion from formal law?',
    'Longitudinal empirical comparison: dispute resolution outcomes (fairness, enforceability, appeal rates) in communities with accessible parallel systems vs communities transitioning to unified formal systems. Measure dispute subject satisfaction, outcome enforceability, and repeat victimization rates.',
    'If parallel systems improve access: constraint may classify closer to Rope (genuine coordination). If they entrench exclusion: constraint is Snare from powerless perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_equity_tradeoff, empirical, 'Whether parallel systems improve legal access or entrench marginalization').

omega_variable(
    customary_authority_legitimacy_base,
    'Is customary authority''s legitimacy genuinely rooted in social consensus or enforced through latent coercion and cultural gatekeeping?',
    'Study exit dynamics: measure rates of community members rejecting customary authority jurisdiction when formal courts become accessible; analyze whether formal court acceptance correlates with education, mobility, or demographic advantage.',
    'If exit rates are low: customary legitimacy is genuine and coordination benefits are real. If high: legitimacy is performative and suppression is the primary binding mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_authority_legitimacy_base, empirical, 'Source of customary authority legitimacy').

omega_variable(
    gender_asymmetry_amplification,
    'Do parallel justice systems amplify gender-based extraction, or does suppression of women''s exit options through cultural constraints mask pre-existing patriarchal extraction?',
    'Comparative analysis of justice outcomes for women across case types in parallel vs unified systems; measure gender gap in appeal rates, outcome satisfaction, repeat victimization. Track whether women''s legal literacy and formal court usage increase when formal systems become accessible.',
    'If amplification: parallel systems add a layer of extraction specifically targeting women. If masking: women bear existing patriarchal extraction but the parallel system structure is not the primary driver.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_asymmetry_amplification, empirical, 'Whether parallel systems amplify gender-based extraction').

omega_variable(
    state_capacity_vs_ideology,
    'Are parallel systems tolerated primarily because state capacity is insufficient to deliver universal formal justice, or because state elites benefit from maintaining informal control mechanisms?',
    'Examine state resource allocation: do states invest in formal court capacity in high-development regions but tolerate parallel systems in peripheral regions? Do state elites'' own disputes go to formal courts? Compare tolerance of parallel systems across different state types (democratic vs authoritarian).',
    'If capacity-driven: parallel systems may be transitional and improvable through state investment. If ideology-driven: state benefits from maintaining dual legitimacy and may actively prevent unification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_vs_ideology, empirical, 'Whether state toleration is capacity-based or ideologically motivated').

omega_variable(
    informal_system_reform_ceiling,
    'Can codification, standardization, and oversight of customary law eliminate the extractive dimension while preserving coordination benefits, or does formalization inevitably eliminate the legitimacy source?',
    'Natural experiment analysis: compare outcomes in jurisdictions that have implemented hybrid tribunals, codified customary law with appeal mechanisms, or created parallel formal tracks for customary disputes vs pure informal systems. Measure whether reforms actually reduce extraction or merely add complexity.',
    'If reformable: scaffold perspective is structurally sound and sunset is achievable. If not reformable: hybrid systems become theaters masking continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_system_reform_ceiling, empirical, 'Whether customary law can be reformed without losing legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parallel_justice_systems, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pjs_tr_t0, parallel_justice_systems, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pjs_tr_t10, parallel_justice_systems, theater_ratio, 10, 0.56).
narrative_ontology:measurement(pjs_tr_t20, parallel_justice_systems, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(pjs_be_t0, parallel_justice_systems, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pjs_be_t10, parallel_justice_systems, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(pjs_be_t20, parallel_justice_systems, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parallel_justice_systems, enforcement_mechanism).
narrative_ontology:affects_constraint(parallel_justice_systems, legal_pluralism_gender_bias).
narrative_ontology:affects_constraint(parallel_justice_systems, informal_dispute_resolution_access).
narrative_ontology:affects_constraint(parallel_justice_systems, state_capacity_externality).

% DUAL FORMULATION NOTE:
% Parallel justice systems decompose into three structurally distinct constraints: (1) the core pluralism constraint (this story, ε=0.58, tangled_rope) governing how dual systems coexist and allocate dispute resolution authority; (2) gender-specific extraction through customary law biases (ε=0.68, snare), where women's restricted exit options amplify extraction; (3) the state capacity externality (ε=0.42, scaffold), where formal court absence forces reliance on informal systems, which could be solved through state investment. Each has different ε values and different resolution pathways. This story addresses the hybrid coordination-extraction of the core pluralism mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parallel_justice_systems, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
