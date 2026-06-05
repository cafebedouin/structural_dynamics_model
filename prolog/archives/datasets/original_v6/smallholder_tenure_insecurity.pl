% ============================================================================
% CONSTRAINT STORY: smallholder_tenure_insecurity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_smallholder_tenure_insecurity, []).

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
 *   constraint_id: smallholder_tenure_insecurity
 *   human_readable: Smallholder Tenure Insecurity and Extractive Land Governance
 *   domain: economic/agricultural/political
 *
 * SUMMARY:
 *   Smallholder tenure insecurity describes the structural vulnerability of
 *   farming communities whose land rights are not formally recognized or
 *   legally protected by the state. This constraint operates across
 *   Sub-Saharan Africa, South Asia, Latin America, and Southeast Asia,
 *   affecting approximately 2 billion people. The constraint exhibits
 *   tangled_rope properties: it contains genuine coordination functions
 *   (consolidating fragmented holdings for large-scale investment, reducing
 *   administrative burden on states with limited capacity, enabling flexible
 *   resource allocation) while simultaneously extracting from smallholders
 *   through land seizure, displacement, and coercive terms. The
 *   classification demonstrates how the same structural arrangement produces
 *   different perceived types across observer positions: government sees rope
 *   (coordination), smallholders see snare (pure extraction), developers see
 *   tangled_rope (mixed gains), and land rights reformers see scaffold
 *   (temporary problem with sunset). The theater ratio (0.48) reflects that
 *   this is primarily a functional extraction mechanism rather than a
 *   performative ritual — tenure insecurity does real work for beneficiaries,
 *   which is why it persists despite known harmful effects. The
 *   extractiveness has increased over the 50-year interval as commercial
 *   pressures intensified and state capacity for adjudication improved for
 *   some groups while systematically excluding others.
 *
 * KEY AGENTS:
 *   - Smallholder Farmers: Primary victims (powerless/trapped) — bear full extraction through land loss and coercive terms; no structural exit options available
 *   - Pastoral Communities: Secondary victims (moderate/constrained) — face extraction through land loss to agriculture/conservation; customary tenure systems unrecognized; constrained exit due to economic and cultural dependence
 *   - Indigenous Groups: Victims with identity lock (powerless/identity_locked) — face extraction compounded by identity fusion with ancestral territory; cannot exercise mobility options without abandoning cultural identity
 *   - Government Agencies: Primary beneficiary (institutional/arbitrage) — controls land allocation, captures licensing revenue, reduces administrative burden; has policy discretion and exit options
 *   - Commercial Developers: Secondary beneficiary (powerful/mobile) — acquire land at low cost due to unclear tenure; benefit from state enforcement; globally mobile exit options
 *   - Local Elites: Beneficiary (powerful/constrained) — accumulate land through insider connections; constrained by need to maintain state relationships
 *   - Land Rights Reform Coalition: Organized agents (organized/constrained) — NGOs, international agencies, civil society; see constraint as soluble through policy reform; constrained by need to work within state legitimacy frameworks
 *   - Colonial Land Registry Institutions: Institutional inertia actor (institutional/arbitrage) — formal titling systems persist through donor funding and institutional path dependence despite limited actual function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(smallholder_tenure_insecurity, 0.58).
domain_priors:suppression_score(smallholder_tenure_insecurity, 0.72).
domain_priors:theater_ratio(smallholder_tenure_insecurity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(smallholder_tenure_insecurity, extractiveness, 0.58).
narrative_ontology:constraint_metric(smallholder_tenure_insecurity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(smallholder_tenure_insecurity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(smallholder_tenure_insecurity, tangled_rope).
narrative_ontology:human_readable(smallholder_tenure_insecurity, "Smallholder Tenure Insecurity and Extractive Land Governance").
narrative_ontology:topic_domain(smallholder_tenure_insecurity, "economic/agricultural/political").

domain_priors:requires_active_enforcement(smallholder_tenure_insecurity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(smallholder_tenure_insecurity, government_agencies).
narrative_ontology:constraint_beneficiary(smallholder_tenure_insecurity, commercial_developers).
narrative_ontology:constraint_beneficiary(smallholder_tenure_insecurity, foreign_investors).
narrative_ontology:constraint_beneficiary(smallholder_tenure_insecurity, local_elites).
narrative_ontology:constraint_victim(smallholder_tenure_insecurity, smallholder_farmers).
narrative_ontology:constraint_victim(smallholder_tenure_insecurity, pastoral_communities).
narrative_ontology:constraint_victim(smallholder_tenure_insecurity, indigenous_groups).
narrative_ontology:constraint_victim(smallholder_tenure_insecurity, agricultural_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped by insecure land tenure. Cannot exit without losing livelihood and ancestral land. Faces extraction through predatory land acquisition, forced displacement, uncompensated seizure, and coercive terms on remaining land. High suppression: legal systems favor documented ownership, credit systems exclude those without titles, enforcement mechanisms protect developer interests over cultivator rights. Maximum experienced extraction for this agent.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNMENT AGENCY (ROPE) — Experiences tenure insecurity as a coordination mechanism: unclear property rights reduce administrative burden (no titles to adjudicate in remote areas), enable flexible land allocation, and generate revenue through licensing developers. Beneficiary with exit options — can redefine policy, issue new titles, or maintain status quo. Sees the constraint as solving a real coordination problem: land allocation in contexts of limited state capacity and competing claims.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMERCIAL DEVELOPER (TANGLED ROPE) — Benefits from tenure insecurity (low acquisition costs, weak legal obstacles to displacement). Also experiences genuine coordination function: unclear tenure creates opportunity for large-scale investment that smallholders cannot finance individually. Mobile exit options (can invest elsewhere) but structurally benefits from this specific constraint. Mixed relationship: gains from extraction but also from solving coordination problem of consolidating fragmented holdings.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PASTORAL COMMUNITY (TANGLED ROPE) — Bears extraction through land loss to commercial agriculture and conservation reserves, but also genuinely coordinates access to pasture across seasonal and climatic cycles through customary tenure systems. Modern nation-state frameworks don't recognize customary title, creating asymmetric extraction. Constrained exit: they can abandon pastoralism but this requires economic restructuring and loss of cultural identity. High suppression: customary law unrecognized, state law imposed, enforcement favors modern titling over traditional rights.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INDIGENOUS COMMUNITIES (SNARE WITH IDENTITY_LOCK) — Trapped by dual suppression: state law denies land rights, and identity is constituted through ancestral territory. Exit would require abandoning indigenous identity and relationship to land. Maximum suppression: legal systems exclude communal title, enforcement mechanisms criminalize traditional resource use, alternative economic models are deliberately foreclosed. Identity-locked: cannot exercise what few mobility options exist because land is inseparable from cultural survival. Structural d high, cognitive lock adds extra immobility layer.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: LAND RIGHTS REFORM COALITION (SCAFFOLD) — Organized actors (NGOs, international development agencies, civil society) see tenure insecurity as a soluble problem with sunset logic: formalization programs, community land titling, and rights-based approaches are creating alternative tenure arrangements. Experiences constraint as temporary coordination failure resolvable through policy reform. Theater low: reform movement builds on measured research and documented harm, not on ritualized compliance. Sunset clause: as formal titling expands and customary rights frameworks are legally recognized, the tenure insecurity mechanism should degrade. Constrained exit: reformers must work within state legitimacy frameworks and secure funding to implement alternatives.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLONIAL LAND REGISTRY INSTITUTIONS (PITON) — Formal titling systems installed during colonialism persist through institutional inertia despite poor fit to local contexts. Theater high: land registries maintain elaborate bureaucratic processes (surveys, documentation, adjudication) that appear functional but exclude smallholders who lack literacy, capital, or documentation. The registry sees its own function as degraded (many smallholders never participate, customary systems continue in parallel) but persists because donor funding supports it and no competing institution has fully replaced it. Piton signature: high theater (bureaucratic ritual), low actual function (most land remains untitled or semi-titled), institutional inertia (persists despite known limitations).
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN — FALSE SUMMIT) — From a civilizational/universal view, some degree of tenure insecurity appears inherent: land is scarce, claims overlap, and adjudicating competing uses requires governance structures. This perspective naturalizes tenure insecurity as an immutable feature of human-land relationships. However, structural data reveals this is a false summit: tenure insecurity is not physics-level constraint. The base extractiveness (0.58) and high suppression (0.72) indicate active enforcement of inequality, not natural scarcity. Comparable societies with strong customary tenure or comprehensive titling systems show much lower extractiveness. The 'natural law' framing obscures policy choices and institutional design.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(smallholder_tenure_insecurity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(smallholder_tenure_insecurity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(smallholder_tenure_insecurity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(smallholder_tenure_insecurity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(smallholder_tenure_insecurity, TR),
    TR >= 0.70.

:- end_tests(smallholder_tenure_insecurity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantial value from smallholders through land loss and coercive terms, but is not maximum extraction — some smallholders retain land and some negotiate improved conditions, preventing full deprivation. The trajectory shows increasing extractiveness over the 50-year interval (from 0.42 to 0.58) as commercial pressures intensified and state capacity for selective enforcement improved. Suppression (0.72): High. Legal systems favor documented ownership over customary claims, credit systems exclude untitled holders, enforcement mechanisms systematically privilege developers over smallholders, and alternative governance structures are delegitimized. Suppression includes both structural barriers (capital requirements for titling, legal literacy) and ideological suppression (naturalizing formal titling as inevitable, portraying customary systems as backward). Theater ratio (0.48): Moderate-low. This is primarily functional extraction rather than performative theater — tenure insecurity does real work for beneficiaries, generating concrete benefits (reduced administrative costs for states, low acquisition costs for developers, access to consolidation opportunities). The theater component appears in formal titling systems that maintain elaborate bureaucratic processes with limited actual coverage, and in development narratives that frame tenure formalization as inevitable progress. The fact that theater ratio is lower than extractiveness indicates this is a real, functional constraint rather than one maintained primarily through ritual.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the smallholder's snare classification and the government's rope classification reflects opposite experiences of the same constraint. The smallholder experiences tenure insecurity as pure coercion with no coordination benefit — they are pushed out of land, face insecure conditions, and have no exit. The government experiences it as coordination — it solves the problem of allocating land when administrative capacity is limited and claims overlap. This is not a dispute about facts but about structural position. From the smallholder's perspective, the 'coordination' (consolidating holdings for commercial investment) is not a coordination problem they asked to have solved. It's a mechanism to extract their land. The snare/rope divergence is the classification system detecting that this agent pair experiences radically different effective extractiveness from the same structural arrangement. The analytical observer at civilizational scope risks naturalizing this as immutable — 'land scarcity is inherent' — but the structural data contradicts this. Comparable societies with strong property rights systems (either customary or formal, but applied equally) show much lower extractiveness and suppression. The tenure insecurity is not immutable; it's a policy choice about whose claims the state recognizes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the agent's structural position and experience of extraction flow. Smallholder farmers as trapped powerless agents derive d ≈ 0.95 (full target of extraction), yielding high f(d) ≈ 1.42 and high experienced extractiveness. Government agencies as institutional beneficiaries with arbitrage exit derive d ≈ 0.05 (full beneficiary), yielding f(d) ≈ -0.12 and negative/neutral experienced extractiveness (they do not experience themselves as extracted from). Developers as powerful beneficiaries with mobile exit derive d ≈ 0.25, yielding f(d) ≈ 0.02 (near-neutral). Pastoral communities as moderate constrained agents who both contribute to coordination and bear extraction derive d ≈ 0.55 (mixed), yielding f(d) ≈ 0.75 (moderate experienced extraction). Indigenous groups as identity-locked agents derive d ≈ 0.89 (high target despite some structural mobility), yielding f(d) ≈ 1.28 (high experienced extraction). The identity lock adds perceptual immobility on top of structural mobility. Reform coalition as organized constrained beneficiary-victim mix derives d ≈ 0.45 (moderately distributed), yielding f(d) ≈ 0.45 (moderate experienced extraction as they work against their own beneficiary status). Spatial scope σ(S) is national/regional for most perspectives, with some global scope for developers and reform movements, yielding σ ≈ 1.0–1.1. The effective extraction χ = ε × f(d) × σ(S) thus varies from negative/near-zero for government (beneficiary perspective) to 0.82+ for trapped smallholders at national scope.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY SCENARIO: The constraint demonstrates how false natural-law framing conceals policy choices. The civilizational analytical observer's mountain classification falsely summarizes tenure insecurity as inherent scarcity. The base extractiveness (0.58) and suppression (0.72) metrics directly contradict this — if tenure insecurity were a law of nature (like gravity), different societies with identical resource constraints would show similar levels. They don't. Kenya, Uganda, and Rwanda with similar ecology show vastly different tenure security depending on policy choices. This reveals the mandatrophy: the mountain classification is a false summit, a naturalization of contingent institutional arrangements. The tangled_rope classification at the institutional/beneficiary level resolves the mandatrophy by acknowledging that the constraint contains BOTH genuine coordination (solving the problem of allocating land among competing claims) AND asymmetric extraction (systematically privileging some claimants over others). The reformist scaffold classification offers a sunset: policy reforms (titling programs, customary rights recognition, rights-based approaches) can degrade the constraint by making tenure security less dependent on state discretion and more formalized, reducing the extraction mechanism while maintaining coordination. However, the identity_lock omega variable introduces a secondary mandatrophy: even if material tenure becomes secure, do indigenous communities remain bound by identity lock to particular territories? This mandatrophy is deeper — it asks whether resolving structural tenure insecurity resolves the cognitive/identity dimensions of the constraint, or whether those persist independently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    titling_effectiveness_paradox,
    'Does formal land titling actually reduce tenure insecurity, or does it substitute one form of insecurity (lack of legal recognition) for another (vulnerability to legal dispossession)?',
    'Longitudinal comparison: tenure security indices before and after titling programs in target countries; tracking of post-titling land loss rates; farmer welfare assessments across multiple time points',
    'If titling effective: scaffold perspective valid, reform coalition strategy sound, constraint should degrade over time. If titling creates new insecurity: extraction mechanism persists, merely changes form, scaffold sunset is illusory. Classification may shift from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(titling_effectiveness_paradox, empirical, 'Whether formal titling reduces or transforms tenure insecurity').

omega_variable(
    customary_tenure_recognition_feasibility,
    'Can state legal systems recognize and enforce customary land tenure without collapsing customary governance institutions or creating competitive dual-system extraction?',
    'Case studies of countries with recognized customary title (Botswana, some Latin American countries); analysis of dual legal system stability; measurement of extraction rates in hybrid systems',
    'If feasible: customary tenure pathways reduce suppression, shift classification toward rope for pastoral/indigenous communities. If unfeasible: recognition in law while enforcement remains state-biased maintains asymmetric extraction, classification remains snare/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_tenure_recognition_feasibility, empirical, 'Whether state recognition of customary tenure can reduce extraction').

omega_variable(
    developer_incentive_substitution,
    'If smallholder tenure becomes secure through titling, will commercial developers seek alternative extraction mechanisms (financial debt traps, input dependency, price manipulation) rather than direct land seizure?',
    'Observation of developer strategies in countries with strong smallholder titling (Kenya post-Land Bill, parts of India); tracking of indirect extraction mechanisms (contract farming coercion, input credit traps, output price manipulation)',
    'If substitution occurs: constraint transforms rather than resolves, extractiveness may remain high but mechanism changes from snare (direct dispossession) to tangled_rope (financial coordination coercion). If extraction declines: reform coalition strategy valid, constraint genuinely resolves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developer_incentive_substitution, empirical, 'Whether securing tenure shifts rather than removes extraction mechanisms').

omega_variable(
    identity_lock_persistence_trajectory,
    'For indigenous communities classified as identity_locked on tenure insecurity, does the suppression persist and bind after material barriers (loss of land) are removed, or is identity lock purely dependent on the structural suppression?',
    'Post-land-restitution assessment: communities regaining territory — do cultural identity and tenure security correlate, or do identity constraints persist even when material barriers dissolve?',
    'If identity lock is independent: suppression mechanism includes cognitive/cultural dimensions requiring different policy responses than land reform alone. If lock is dependent on material barriers: resolving tenure insecurity directly resolves identity constraint. Classification refinement: distinguishes whether identity_lock is primary or secondary mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence_trajectory, empirical, 'Whether identity lock persists after material tenure barriers are removed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(smallholder_tenure_insecurity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sti_tr_t0, smallholder_tenure_insecurity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sti_tr_t25, smallholder_tenure_insecurity, theater_ratio, 25, 0.48).
narrative_ontology:measurement(sti_tr_t50, smallholder_tenure_insecurity, theater_ratio, 50, 0.48).
narrative_ontology:measurement(sti_tr_t10, smallholder_tenure_insecurity, theater_ratio, 10, 0.51).

% Extraction over time
narrative_ontology:measurement(sti_be_t0, smallholder_tenure_insecurity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sti_be_t25, smallholder_tenure_insecurity, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(sti_be_t50, smallholder_tenure_insecurity, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(sti_be_t10, smallholder_tenure_insecurity, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(smallholder_tenure_insecurity, resource_allocation).
narrative_ontology:affects_constraint(smallholder_tenure_insecurity, agricultural_productivity_extraction).
narrative_ontology:affects_constraint(smallholder_tenure_insecurity, migration_lock_through_land_dependence).
narrative_ontology:affects_constraint(smallholder_tenure_insecurity, customary_law_vs_state_law_conflict).

% DUAL FORMULATION NOTE:
% Smallholder tenure insecurity is a parent constraint that affects multiple downstream constraints in agricultural and rural development systems. The core constraint (who has enforceable land rights) structures access to productive resources, which affects agricultural productivity extraction. The land dependence also structures migration decisions (people trapped in declining agricultural zones). Customary/state law conflict is a structurally distinct constraint with different ε values, related to tenure insecurity but analytically separable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(smallholder_tenure_insecurity, powerful, 0.25).
constraint_indexing:directionality_override(smallholder_tenure_insecurity, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
