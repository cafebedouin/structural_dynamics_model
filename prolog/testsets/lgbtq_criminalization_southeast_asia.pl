% ============================================================================
% CONSTRAINT STORY: lgbtq_criminalization_southeast_asia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lgbtq_criminalization_southeast_asia, []).

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
 *   constraint_id: lgbtq_criminalization_southeast_asia
 *   human_readable: LGBTQ Criminalization in Southeast Asia
 *   domain: legal/political/human_rights
 *
 * SUMMARY:
 *   LGBTQ criminalization in Southeast Asia represents a constraint that
 *   combines colonial legal inheritance, state power consolidation, religious
 *   institutional boundary maintenance, and systematic suppression of sexual
 *   minorities. The constraint exhibits multiple structurally distinct
 *   dimensions: criminal law enforcement, family-level social control,
 *   extrajudicial violence, and identity-denial psychological mechanisms.
 *   Extractiveness has increased from 0.55 to 0.68 over the measurement
 *   interval as enforcement has become more selective and politically
 *   instrumentalized, while theater ratio has increased from 0.42 to 0.55 as
 *   performative enforcement (periodic prosecutions for political messaging)
 *   supplements direct suppression. The constraint operates simultaneously as
 *   pure extraction (snare from the victim perspective), as institutional
 *   coordination (rope from state apparatus), as mixed
 *   coordination-extraction with religious boundary maintenance (tangled rope
 *   from conservative institutions), as colonial legal inertia (piton), as a
 *   targetable institutional barrier (scaffold from rights movements), and as
 *   a naturalized cultural law (false summit mountain from conservative
 *   analytical perspectives). The constraint's structural heterogeneity makes
 *   it a diagnostic exemplar: the same laws produce six structurally distinct
 *   types depending on observational position.
 *
 * KEY AGENTS:
 *   - LGBTQ Persons: Primary victims (powerless/trapped) — face criminal liability, imprisonment, family separation, employment exclusion, and psychological trauma with no legal exit option
 *   - State Apparatus and Law Enforcement: Primary beneficiaries (institutional/arbitrage) — expand authority, justify budgets, consolidate political control; full exit options (decriminalization is feasible)
 *   - Conservative Religious Institutions: Secondary beneficiaries (powerful/mobile but identity_locked) — maintain doctrinal coherence and institutional boundaries; mixed coordination and extraction
 *   - Closeted LGBTQ Persons: Secondary victims (moderate/identity_locked) — face family-level coordination demands with embedded extraction; structurally mobile but identity-fused with family belonging
 *   - LGBTQ Rights Movements: Organized agents (organized/constrained) — building alternative legal pathways and transnational advocacy with regional sunset logic
 *   - Colonial Legal System: Institutional actor (institutional/arbitrage) — sodomy laws inherited from British, Dutch, French colonial codes; persist through inertia (piton)
 *   - Healthcare and Mental Health Workers: Tertiary victims (moderate/constrained) — pressured to pathologize or treat LGBTQ identities; face licensing/employment penalties for ethical autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lgbtq_criminalization_southeast_asia, 0.68).
domain_priors:suppression_score(lgbtq_criminalization_southeast_asia, 0.78).
domain_priors:theater_ratio(lgbtq_criminalization_southeast_asia, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lgbtq_criminalization_southeast_asia, extractiveness, 0.68).
narrative_ontology:constraint_metric(lgbtq_criminalization_southeast_asia, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(lgbtq_criminalization_southeast_asia, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lgbtq_criminalization_southeast_asia, snare).
narrative_ontology:human_readable(lgbtq_criminalization_southeast_asia, "LGBTQ Criminalization in Southeast Asia").
narrative_ontology:topic_domain(lgbtq_criminalization_southeast_asia, "legal/political/human_rights").

domain_priors:requires_active_enforcement(lgbtq_criminalization_southeast_asia).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lgbtq_criminalization_southeast_asia, state_apparatus).
narrative_ontology:constraint_beneficiary(lgbtq_criminalization_southeast_asia, conservative_religious_institutions).
narrative_ontology:constraint_beneficiary(lgbtq_criminalization_southeast_asia, political_gatekeepers).
narrative_ontology:constraint_victim(lgbtq_criminalization_southeast_asia, lgbtq_populations).
narrative_ontology:constraint_victim(lgbtq_criminalization_southeast_asia, gender_nonconforming_persons).
narrative_ontology:constraint_victim(lgbtq_criminalization_southeast_asia, sexual_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LGBTQ PERSONS (SNARE) — Face criminal liability, imprisonment, and social death for identity and relationships. Exit options are severely constrained: physical exit (migration) requires resources and legal status most lack; identity exit (closeting) requires psychological self-denial with persistent extraction costs. The constraint extracts continuously through criminalization, surveillance, family separation, employment exclusion, and psychological trauma. No coordination benefit — the law serves only to suppress alternatives.
constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE APPARATUS (ROPE) — Benefits from criminalization through expansion of police authority, budget justification, and social control leverage. Experiences the constraint as a coordination mechanism: the criminal code coordinates state power and social control. Enforcement activity justifies institutional existence and resource allocation. Net beneficiary with full exit options (repeal is feasible). The constraint appears as functional coordination from this position.
constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSERVATIVE RELIGIOUS INSTITUTIONS (TANGLED ROPE) — Genuine coordination function: the constraint stabilizes institutional boundaries and membership criteria, enabling internal doctrinal coherence. Also asymmetric extraction: enforcement extends institutional authority into state power, creating political influence and cultural legitimacy. Mobile exit options (institutions could adapt teachings) but identity-locked in doctrinal commitment to specific sexual norms. Experience mixed coordination and extraction.
constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: COLONIAL LEGAL INHERITANCE (PITON) — Sodomy laws in Southeast Asia originated in British, Dutch, French colonial legal codes. Post-independence, these laws persist through institutional inertia despite changing cultural values in urban and younger populations. Theater ratio is high: enforcement is selective and performative (periodically invoked for political purposes), but many prosecutions are abandoned, laws are unevenly applied, and the actual functional suppression is supplemented by extrajudicial violence and family pressure. The law persists because alternatives haven't fully displaced it, not because enforcement is effective or consistent.
constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LGBTQ RIGHTS MOVEMENTS (SCAFFOLD) — Organized agents (NGOs, underground networks, regional coalitions) are building alternative legal pathways, documentation networks, and transnational advocacy infrastructure. Regional movements (ASEAN civil society, international human rights bodies) see the criminalization as a temporary institutional barrier with a sunset clause: growing democratic movements, economic integration, and intergenerational value shifts are incrementally creating exit pathways. High suppression due to constrained resources and political repression, but organized agency enables coalition formation and strategic exit.
constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: CLOSETED LGBTQ PERSONS IN CONSERVATIVE FAMILIES (TANGLED ROPE) — Face genuine coordination demands: family stability, inheritance rights, and social safety depend on conformity to heteronormative structures. Also asymmetric extraction: constrained from pursuing authentic relationships while bearing costs of psychological separation. Structurally mobile (economic independence, emigration are possible) but identity-locked: exit would require abandoning family identity, relational bonds, and community belonging. The constraint appears as mixed coordination (family stability) and extraction (self-denial) from this position. Theater ratio moderate: family honor rhetoric is partly genuine boundary maintenance and partly a cover story for control.
constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: HEALTHCARE/MENTAL HEALTH WORKERS (SNARE) — Many face legal and professional pressure to pathologize or 'treat' LGBTQ identities. Constrained exit options: professional licensing depends on state regulation, and many clinician associations lack explicit norms protecting LGBTQ-affirming care. Also bear extraction through cognitive conflict (professional ethics vs legal/professional pressure). The constraint extracts through forced collusion in harm — workers must either violate professional ethics or risk licensing/employment penalties.
constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a conservative civilizational perspective, some cultures naturally prohibit same-sex conduct as an immutable cultural or religious law. This framing treats criminalization as emerging naturally from deep cultural values. However, the structural data contradicts this: (a) criminalization rates vary widely within SE Asia despite similar cultural/religious contexts; (b) colonial origin of specific laws demonstrates contingency; (c) laws are selectively enforced, indicating political instrumentality rather than cultural immutability; (d) younger generations and urban populations show rapid value shifts. The mountain classification is a false summit — it naturalizes what is a contingent institutional arrangement maintained by active enforcement.
constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lgbtq_criminalization_southeast_asia_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lgbtq_criminalization_southeast_asia, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lgbtq_criminalization_southeast_asia, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lgbtq_criminalization_southeast_asia, TR),
    TR >= 0.70.

:- end_tests(lgbtq_criminalization_southeast_asia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The constraint extracts substantially from LGBTQ populations through criminalization, surveillance, family separation, employment exclusion, and psychological harm. The extraction is not maximal (0.85+) because: (a) enforcement is selective rather than uniform across all jurisdictions and populations; (b) underground communities and diaspora networks create partial exit options; (c) legal exposure varies widely by visibility level (closeted individuals face lower direct legal extraction). The increase from 0.55 to 0.68 reflects growing political instrumentalization of criminalization for electoral messaging and regime legitimation, increasing the selectivity and unpredictability of enforcement, which paradoxically increases effective extraction (victims cannot predict safety). Suppression (0.78): Very high. Barriers to exit include: criminal law with imprisonment penalties; family violence and economic exclusion for those attempting to leave; extrajudicial violence from state and non-state actors; psychological internalization (identity-lock) that persists across jurisdictions; lack of institutional protection or asylum mechanisms within SE Asia; documentation barriers for undocumented persons and those in rural areas. Theater ratio (0.55): Moderate-high. Enforcement is visibly selective and politically timed: laws are invoked for high-profile prosecutions during electoral cycles, against activist groups and visible populations, while many prosecutions are abandoned and invisible suppression (extrajudicial violence, family control) dominates. The performative character increases as enforcement becomes more politicized, but some enforcement is direct and functional (imprisonment, harassment).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The powerless victim sees pure extraction (snare) — the law exists only to suppress them. The state apparatus sees pure coordination (rope) — the law coordinates state power and social control. Religious institutions see mixed coordination and extraction (tangled_rope) — genuine boundary maintenance with institutional extraction benefits. Closeted persons see tangled rope — family coordination with embedded extraction. Rights movements see a targetable institutional problem with a sunset (scaffold) — alternative legal pathways are building. The colonial legal system sees institutional inertia (piton) — outdated laws persist through momentum despite changing values. The conservative analytical observer risks seeing natural law (mountain) — treating criminalization as immutable cultural tradition — but the structural data reveals this as false summit: laws are selective, colonial in origin, politically instrumentalized, and unevenly enforced. The perspectival gaps are not measurement ambiguities but genuine structural differences in who benefits and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the agent's structural position relative to the constraint. Victims (powerless/trapped LGBTQ persons) derive high d through their exit-less victimhood; the sigmoid f(d) applies maximum extraction multiplicand. Beneficiaries (state/institutions with arbitrage exit options) derive low d through their beneficiary status and full exit availability; f(d) applies subsidy to their experience (they can repeal at will but choose not to, indicating the extraction is valuable to them). Moderate victims (closeted LGBTQ persons with identity-lock) occupy intermediate d through constrained exit and identity-fusion: they could theoretically leave families but cannot psychologically exit their family identity. Religious institutions with powerful status but identity-locked exit derive moderate-high d: they benefit from enforcement but cannot easily change doctrinal commitments that justify the criminalization. Rights movements with organized power and constrained (not arbitrage) exit derive higher d than their nominal power would suggest, reflecting real structural subordination relative to state power. The heterogeneity of d values across perspectives produces the classification spread: powerless/trapped → snare, institutional/arbitrage beneficiary → rope, powerful/mobile but identity-locked → tangled_rope, moderate/identity_locked victims → tangled_rope with low chi despite victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that classification depends critically on whether the observer is measuring from a beneficiary or victim position. From the victim's perspective (powerless/trapped), this is unambiguously a snare: extraction is maximal, suppression is total, coordination benefit is zero. From the state apparatus perspective (institutional/arbitrage), this is rope: genuine coordination of state power and social control. The mandatrophy resolution is NOT 'which type is objectively correct' but rather: the constraint IS a snare for victims AND a rope for beneficiaries. The system correctly classifies both. The false summit (mountain) appears only when conservative analytical observers naturalize the constraint as immutable cultural law — the naturalization occurs when the observer adopts a position that makes the contingent institutional origins invisible. The engine's false summit detector flags this: if a constraint classifications as mountain but structural data shows selective enforcement, colonial origin, and regime-dependent variation, the mountain classification is a false summit. The true classification presheaf has depth: snare from victim perspective, rope from beneficiary perspective, scaffold from rights movement perspective, piton from colonial inheritance perspective, tangled_rope from religious institution and closeted-person perspectives, and false summit mountain from naturalized conservative perspective. No single type is 'the' answer — the set of six perspectives IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_mechanism,
    'Is selective enforcement a feature that reduces effective suppression, or a mechanism that increases extraction through unpredictability and discretionary targeting?',
    'Analysis of enforcement patterns: correlation between prosecution rates and political cycles, targeting of visible vs closeted populations, use of laws against activist groups vs random enforcement',
    'If enforcement selectivity reduces suppression: snare classification may downgrade to tangled_rope. If it increases extraction through unpredictability: suppression should be scored higher (0.85+) and snare classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Whether selective enforcement reduces or increases effective extraction').

omega_variable(
    family_versus_legal_suppression_distinction,
    'What proportion of suppression is legal (criminal law and police enforcement) vs extrajudicial (family violence, community ostracism, economic exclusion)? Which is dominant in maintaining the constraint?',
    'Comparative analysis of suppression mechanisms: surveys of LGBTQ populations on sources of constraint; institutional review of prosecution rates vs documented harassment; analysis of how decriminalization affects safety outcomes in countries that have reformed',
    'If legal suppression is secondary: decriminalization may have limited effect and alternative constraints (family pressure, social norms) require separate story treatment. If legal suppression is primary: decriminalization would directly reduce extraction for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_versus_legal_suppression_distinction, empirical, 'Relative weight of legal vs extrajudicial suppression mechanisms').

omega_variable(
    religious_coordination_versus_cover_story,
    'Is the conservative religious institutional coordination function (maintaining doctrinal coherence and boundary maintenance) genuine, or is religious framing primarily a legitimation cover story for state power consolidation?',
    'Historical analysis of law timing relative to religious institution consolidation; comparative study of criminalization in religiously heterogeneous countries; analysis of whether institutions actively enforce laws or use them primarily for political legitimation',
    'If genuine coordination: religious institutions see real tangled_rope classification. If cover story: the constraint is more purely extractive for beneficiaries, reducing coordination rationale and strengthening snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_coordination_versus_cover_story, conceptual, 'Whether religious coordination is genuine or instrumentalized').

omega_variable(
    regional_divergence_trajectory,
    'Will SE Asian countries follow decriminalization trajectories (Taiwan, Thailand movements) or entrench criminalization (Philippines, Malaysia resurgence)?',
    'Tracking of legislative changes, judicial decisions, and transnational advocacy outcomes; correlation with regime type, democratic transitions, and regional integration; longitudinal measurement of public opinion shifts',
    'Trajectories toward decriminalization strengthen scaffold classification and reduce snare effectiveness. Trajectories toward entrenchment strengthen snare and reduce sunset viability of scaffold perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_divergence_trajectory, empirical, 'Regional divergence in decriminalization vs entrenchment trajectories').

omega_variable(
    identity_lock_internalization_depth,
    'For closeted LGBTQ persons in conservative families, how much of the suppression is internalized identity-lock vs external family enforcement? Does exit (leaving the family) immediately enable identity freedom or does internalized suppression persist?',
    'Longitudinal psychological studies of LGBTQ persons post-family-separation; analysis of internalized homophobia persistence in diaspora populations; comparison of psychological outcomes for those with vs without family reintegration',
    'If suppression is primarily internalized: the constraint''s effective suppression extends beyond the family relationship and persists through identity-lock mechanisms. If primarily external: exit from the family substantially reduces suppression costs for that perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_depth, empirical, 'Degree of internalization in family-based identity-lock suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lgbtq_criminalization_southeast_asia, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lgbtq_se_asia_tr_t0, lgbtq_criminalization_southeast_asia, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lgbtq_se_asia_tr_t10, lgbtq_criminalization_southeast_asia, theater_ratio, 10, 0.48).
narrative_ontology:measurement(lgbtq_se_asia_tr_t20, lgbtq_criminalization_southeast_asia, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(lgbtq_se_asia_be_t0, lgbtq_criminalization_southeast_asia, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(lgbtq_se_asia_be_t10, lgbtq_criminalization_southeast_asia, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(lgbtq_se_asia_be_t20, lgbtq_criminalization_southeast_asia, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lgbtq_criminalization_southeast_asia, enforcement_mechanism).
narrative_ontology:affects_constraint(lgbtq_criminalization_southeast_asia, family_obligation_southeast_asia).
narrative_ontology:affects_constraint(lgbtq_criminalization_southeast_asia, religious_institution_boundary_maintenance).
narrative_ontology:affects_constraint(lgbtq_criminalization_southeast_asia, state_apparatus_legitimation_southeast_asia).

% DUAL FORMULATION NOTE:
% LGBTQ criminalization in SE Asia operates at multiple structural levels: (1) state-level legal enforcement (this story); (2) family-level social control and obligation (family_obligation_southeast_asia); (3) religious institution doctrinal boundary maintenance (religious_institution_boundary_maintenance). These are not the same constraint viewed from different angles — they have different ε values reflecting the relative extractiveness of legal vs family vs religious enforcement. Legal enforcement alone (this story, ε=0.68) shows high extraction but selective application. Family-level control shows different extraction mechanisms (psychological, economic) with high identity-lock. Religious institutions show low ε for their internal coordination function but higher extraction when linked to state enforcement. Decomposition enables measurement of which enforcement level is dominant and whether decriminalization would substantially reduce suppression (if legal enforcement is primary) or leave family/religious suppression intact (if internalized).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lgbtq_criminalization_southeast_asia, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
