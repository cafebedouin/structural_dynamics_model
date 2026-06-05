% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Legitimacy Equilibrium: Domain-Partitioned Authority
 *   domain: political_history/institutional_change/modernization_studies
 *
 * SUMMARY:
 *   The dual practice legitimacy equilibrium describes a persistent
 *   institutional arrangement in which state authority governs
 *   public/administrative domains (taxation, law, education, census) through
 *   rationalized modern codes (Gregorian calendar, standardized
 *   weights/measures, written law), while traditional authority governs
 *   private/ritual domains (marriage, inheritance, religious observance,
 *   agricultural calendars) through customary codes. This is NOT a
 *   transitional state between tradition and modernity, but a stable
 *   equilibrium in which both legitimacy frames coexist with minimal
 *   expectation of convergence. Practitioners maintain dual identities:
 *   Gregorian calendar for tax deadlines, lunar calendar for planting;
 *   Western suit for state employment, traditional dress for home and ritual;
 *   state law for contract disputes, customary law for inheritance and
 *   marriage. The constraint extracts value from this bifurcation through
 *   code-switching costs, identity fragmentation, and suppression of
 *   alternatives — practitioners cannot simply choose one frame; compliance
 *   requires managing both. The state benefits from having rationalized
 *   public administration without needing to homogenize all social life.
 *   Traditional authorities benefit from retaining autonomy in their domains
 *   without needing to resist state encroachment directly. The practitioners
 *   bearing the constraint experience it as a mixed coordination-extraction
 *   hybrid: the bifurcation does enable both state efficiency and community
 *   autonomy, but at the cost of cognitive and social labor required to
 *   maintain dual identities and comply with dual legitimacy standards.
 *
 * KEY AGENTS:
 *   - State Bureaucratic Apparatus: Institutional beneficiary (institutional/arbitrage) — coordinates taxation, census, law enforcement through unified public-domain codes; extracts administrative efficiency from domain partition
 *   - Traditional Authority Holders: Institutional beneficiary (institutional/constrained) — retain autonomy in private/ritual domains; constrained by state overlay that gradually encroaches on their domain scope
 *   - Embedded Practitioners: Primary victim (powerless/trapped) — forced to maintain dual identities and code-switch between legitimacy frames; bear cognitive and social costs of bifurcation
 *   - Community Governance Networks: Secondary actor (organized/constrained) — benefit from protection of ritual/private domains; constrained by state domain encroachment and younger generation's adoption of state legitimacy frames
 *   - Colonial/Postcolonial Administration: Institutional framework (institutional/arbitrage) — institutionalizes dual legitimacy through 'indirect rule' structures; maintains arrangement performatively
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent bifurcation as inevitable structural feature of scaled societies; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.38).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.48).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Legitimacy Equilibrium: Domain-Partitioned Authority").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/institutional_change/modernization_studies").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '1da8af19-2350-478a-8024-438c5218bc34').
narrative_ontology:cs_kernel_codification('1da8af19-2350-478a-8024-438c5218bc34', distributed).
narrative_ontology:cs_authority_grounding('1da8af19-2350-478a-8024-438c5218bc34', lineage).
narrative_ontology:cs_interpretation_layer_present('1da8af19-2350-478a-8024-438c5218bc34').
narrative_ontology:cs_reading_relation('1da8af19-2350-478a-8024-438c5218bc34', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('1da8af19-2350-478a-8024-438c5218bc34', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('1da8af19-2350-478a-8024-438c5218bc34', foundational, legitimacy_derives_from_domain_partition).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_domain_partition, holdable).
narrative_ontology:cs_axiom_grounding('1da8af19-2350-478a-8024-438c5218bc34', legitimacy_derives_from_domain_partition, conventional).
narrative_ontology:cs_axiom('1da8af19-2350-478a-8024-438c5218bc34', foundational, state_and_traditional_domains_mutually_compatible).
narrative_ontology:cs_axiom_status(state_and_traditional_domains_mutually_compatible, holdable).
narrative_ontology:cs_axiom_grounding('1da8af19-2350-478a-8024-438c5218bc34', state_and_traditional_domains_mutually_compatible, conventional).
narrative_ontology:cs_reference_frame('1da8af19-2350-478a-8024-438c5218bc34', stable_dual_legitimacy_equilibrium).
narrative_ontology:cs_drift_state('1da8af19-2350-478a-8024-438c5218bc34', contemporary_erosion_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1da8af19-2350-478a-8024-438c5218bc34', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucratic_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_structures).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, practitioners_navigating_domains).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cultural_coherence_of_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMBEDDED PRACTITIONER (SNARE) — Cannot exit the dual legitimacy frame without severe social/economic penalty. Trapped in code-switching: Gregorian calendar for tax compliance, lunar calendar for harvest timing; Western suit for state employment, traditional dress for ritual obligation. Suppression is structural (legal enforcement of state codes) and internalized (social expectation of code-switching). Zero degrees of freedom.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY GOVERNANCE NETWORKS (TANGLED ROPE) — Organized actors at the community level (lineage councils, craft guilds, agricultural cooperatives) experience genuine coordination benefit from domain-partitioned legitimacy. They maintain ritual calendars, dress codes, and dispute resolution — all autonomous from state interference. But they are also constrained by the state's overlay: tax deadlines must be met in Gregorian time, property disputes must follow state law, children must attend state schools. They benefit from the bifurcation because it protects their domain; they are victimized by it because the state's domain encroaches.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE BUREAUCRATIC APPARATUS (ROPE) — Experiences domain partitioning as pure coordination: state authority establishes uniform calendar, weights, measures, legal procedures — enabling taxation, census-taking, contract enforcement. The state-public domain is rationalized; private/ritual domains operate under traditional authority. This is coordination for administrative purposes. The state extracts resources but also provides public goods (security, infrastructure, legal adjudication). Net beneficiary with low experienced extraction because the state sees the constraint as enabling its core function.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRADITIONAL AUTHORITY HOLDERS (TANGLED ROPE) — Religious leaders, elders, lineage heads benefit from the state's recognition of private/ritual domain autonomy — they retain authority over marriage, inheritance, ritual observance. But they are constrained by the state's overlay: state law supersedes traditional law in many domains, state-mandated education erodes knowledge transmission, younger generations adopt state legitimacy frames. Genuine coordination benefit (ritual domain protected) plus asymmetric extraction (authority scope shrinking across generations).
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: COLONIAL/POSTCOLONIAL LEGACY (PITON) — The dual legitimacy frame is often institutionalized through colonial administrative structures ('indirect rule': state governs public sphere, customary authorities govern private sphere under state oversight). This institutional arrangement persists through inertia long after its functional rationale has eroded. Theater ratio is high because the arrangement is maintained performatively: states claim to 'respect traditional authority' while systematically constraining it; traditional authorities perform legitimacy while losing substantive autonomy. Piton classification reflects that the structure is mostly maintained theatrically.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN / FALSE SUMMIT CANDIDATE) — From a civilizational/universal perspective, the dual legitimacy frame appears as a structural inevitability: all complex societies must coordinate at scale (state level) while maintaining identity at local level (community level). The bifurcation of legitimacy appears as an immutable feature of social organization — you cannot have both state-level coordination and community-level autonomy without some domain partitioning. However, the analytical observer must attend to the beneficiary/victim structure: this inevitability naturalizes what is historically contingent arrangement that extracts value from practitioners forced to maintain dual identities. The engine's false summit detector will flag this perspective as a candidate for reclassification via FSM signature.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, TR),
    TR >= 0.70.

:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does solve genuine coordination problems (state needs unified public codes; communities need ritual autonomy), but extraction runs through the practitioner's requirement to maintain dual identities. The value reflects that some benefit is genuinely coordination (enabling both state and community governance) while some is extraction (practitioners bear code-switching and identity-fragmentation costs). Measurement trajectory (0.28→0.33→0.38) shows extractiveness rising over the interval, as state domain gradually encroaches on traditional domains — theater ratio rises correspondingly, suggesting the arrangement becomes more performative as the coordination function weakens. Suppression (0.48): Moderate-high. Structural barriers include legal enforcement of state codes, state-mandated education that privileges state legitimacy, social pressure to adopt state identity markers (dress, language, time), and lack of alternatives (practitioners cannot exit either domain). But suppression is not total — some practitioners resist (maintaining lunar calendars despite state discouragement), some communities negotiate (preserving ritual practices in nominally 'private' spaces), and some domains maintain functional autonomy despite state claims to authority. Requires active enforcement because domain partition is not self-equilibrating — the state must continuously reassert authority over public domains while appearing to respect private domains. Theater ratio (0.62): Moderate-high. The arrangement is partly functional (state administration does require unified codes; communities do benefit from domain autonomy) and partly performative (states claim to 'respect traditional authority' while constraining it; traditional authorities perform autonomy while losing substantive scope; practitioners perform dual compliance while experiencing it as constraint). Rising trajectory reflects increasing performative content as the functional coordination benefits erode.
 *
 * PERSPECTIVAL GAP:
 *   The structural phenomenon — domain-partitioned legitimacy — appears as pure extraction (Snare) to embedded practitioners, as mixed benefit-and-constraint (Tangled Rope) to community governance networks and traditional authorities, as pure coordination (Rope) to the state bureaucratic apparatus, as degraded institutional inertia (Piton) to the colonial/postcolonial administrative system, and as natural structural inevitability (Mountain/False Summit) to the analytical observer. The perspectival gap is maximal between the powerless practitioner (who experiences only suppression and code-switching cost) and the institutional state (which experiences only coordination benefit). The organized community networks and traditional authorities occupy the middle ground — they perceive genuine benefit from their protected domains but are constrained by state encroachment. The piton perspective reveals that the arrangement is increasingly maintained theatrically as its functional coordination rationale weakens. The analytical observer's mountain perspective is a false summit candidate — it naturalizes what is revealed as a politically structured equilibrium when beneficiary/victim structure is made visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base_extractiveness (0.38), the observer's directionality value (d) derived from their structural position, and scope modifier σ(S). The embedded practitioner experiences high d (1.0 approaching trapped victim) and negative coordination benefit — very high χ. The community governance networks and traditional authorities experience moderate d (beneficiary + constrained exit) — moderate χ reflecting both benefit and constraint. The state apparatus experiences low d (beneficiary + arbitrage) — low or negative χ, experiencing the constraint as enabling their function. The colonial/postcolonial legacy perspective experiences institutional d but from a degraded/piton position. The analytical observer operates at civilian scope but risks mis-classifying the arrangement as mountain through naturalization of beneficiary interests as structural inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   The dual equilibrium reading resolves mandatrophy by asserting that domain partition is not a failed coordination mechanism requiring resolution toward unified legitimacy, but a genuine equilibrium solution to the coordination problem: how to enable state-scale administration AND community-scale ritual/identity autonomy. The mandatrophy in the sibling readings is different: the endogenous reading struggles with the question 'why do some practitioners adopt state practices voluntarily while others resist?' (mandatrophy surfaces in the boundary conditions); the exogenous reading struggles with 'when does state decree produce compliance versus resistance?' (mandatrophy surfaces in enforcement mechanisms). The equilibrium reading sidesteps both mandatrophies by treating the bifurcation itself as the equilibrium outcome — suppression and extraction are real, but they are the cost of the coordination solution, not failure of coordination. This reading's mandatrophy is instead: 'how stable is this equilibrium over generations?' — the measurement trajectory suggesting extractiveness and theater rising over time indicates the equilibrium may be eroding, with the constraint transitioning toward exogenous override (state domain expanding) or endogenous displacement (traditional practices losing perceived utility). The analytical observer's mountain perspective represents a mandatrophy resolution attempt through naturalization — treating the bifurcation as inevitable rather than as a historically contingent political choice. The false summit detection mechanism interrogates this naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducibility_of_domain_partition,
    'Is the domain partition (state-public vs traditional-private) an irreducible structural feature of scaled societies, or a contingent institutional arrangement that could be dissolved through unified legitimacy?',
    'Historical analysis of societies that attempted unified legitimacy (French Revolutionary homogenization of time/law, Turkish Kemalist standardization, Maoist ritual erasure) and their long-term outcomes; examination of whether practitioners perceived unified legitimacy as liberating or dominating',
    'If irreducible: dual legitimacy is a mountain, bifurcation is natural law. If contingent: dual legitimacy is a snare or tangled rope, bifurcation is enforced choice. This reading asserts contingency with managed equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreducibility_of_domain_partition, conceptual, 'Whether domain partition is irreducible structural feature or contingent arrangement').

omega_variable(
    strategic_vs_internalized_compliance,
    'Do practitioners maintain dual identities because they are forced to (strategic compliance under suppression) or because they have internalized the legitimacy of dual domains (voluntary adoption)?',
    'Ethnographic measurement of compliance frame: do practitioners articulate domain partition as ''natural and good'' or as ''necessary but constraining''? Generational shift in framing (do children maintain same frame as parents)? Behavior in contexts where enforcement is absent (do practitioners maintain dual codes voluntarily)?',
    'If strategic: suppression remains high, constraint is extraction/snare. If internalized: suppression falls, constraint becomes coordination/rope. This reading assumes partial internalization (theater_ratio 0.62 reflects mixed strategic and internalized compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_internalized_compliance, empirical, 'Measurement of whether compliance is strategic or internalized').

omega_variable(
    kernel_reading_disambiguation,
    'This constraint is one reading of the contested kernel ''legitimacy_of_practice_standardization.'' The dual equilibrium reading asserts domain partition is legitimate when both state and traditional authorities maintain autonomy in their respective domains. The sibling readings disagree: endogenous displacement argues legitimacy emerges from voluntary adoption; exogenous override argues legitimacy derives from state decree. Which reading describes the actual structure in a given empirical case?',
    'Case study decomposition: identify whether practitioners experience domain partition as (a) self-equilibrating (dual_practice_equilibrium), (b) emerging from perceived utility shifts (endogenous_displacement), or (c) imposed by state authority (exogenous_override). Multiple readings may apply to the same case at different historical periods or across different social strata.',
    'Empirical classification of specific historical cases (e.g., Qing bureaucratic-examination system with local ritual autonomy = equilibrium; Meiji calendar adoption with elite voluntary embrace = endogenous; British indirect rule = exogenous override). Different reading applies different ε, beneficiary/victim structure, and classification type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, empirical, 'Kernel reading disambiguation: which sibling reading applies to empirical case').

omega_variable(
    boundary_drift_over_generations,
    'Does domain partition maintain stable boundaries across generations, or does the state-public domain gradually expand at the expense of traditional-private domains?',
    'Historical tracking of domain boundaries: which practices counted as ''private/ritual'' in generation t versus generation t+1? Measurement of state encroachment (state education mandates, state marriage law supplanting traditional marriage, state calendar superseding traditional calendars). Examination of whether practitioners perceive drift as erosion.',
    'If boundaries stable: equilibrium is genuine and sustained. If boundaries drift: constraint is unstable, transitioning toward exogenous override (state domain expanding) or endogenous displacement (traditional practices losing perceived utility). This reading assumes current equilibrium; measurements will reveal drift direction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_drift_over_generations, empirical, 'Historical tracking of domain boundary drift').

omega_variable(
    false_summit_interrogation_dual_legitimacy,
    'Is the analytical observer''s view of dual legitimacy as a natural law (''all scaled societies need this bifurcation'') actually a naturalization of a contingent institutional arrangement that benefits state actors and traditional authority holders at the cost of practitioner identity coherence?',
    'False summit signature detection: engine flags beneficiary presence (state_bureaucratic_apparatus, traditional_authority_structures declared in base_properties) on mountain classification. If triggered: reclassify via FSM to tangled_rope, revealing that the mountain framing is a cover story for managed asymmetric extraction.',
    'If FSM triggers: the constraint naturalizes a political choice (domain partition) as inevitable structure. The ''inevitability'' is the extraction mechanism — practitioners internalize the dual-domain frame as necessary, not recognizing it as a constructed equilibrium that serves beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_interrogation_dual_legitimacy, conceptual, 'FSM interrogation of false summit in dual legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpse_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(lpse_tr_t3, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(lpse_tr_t6, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(lpse_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lpse_be_t3, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(lpse_be_t6, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the contested kernel 'legitimacy_of_practice_standardization.' The dual equilibrium reading asserts domain-partitioned legitimacy is a stable equilibrium where state authority governs public/administrative domains and traditional authority governs private/ritual domains. The sibling readings (endogenous_displacement, exogenous_override) represent alternative structural hypotheses about practice legitimacy: voluntary adoption driven by perceived utility versus state decree for collective benefit. These are not measurements of the same constraint from different angles — they are genuinely different constraints with different beneficiary/victim structures, different measured ε values, and different classification types. Each reading is instantiated as a separate constraint story. The network links indicate theoretical kinship (all three are readings of the same kernel) and empirical coupling (which reading applies to a given historical case depends on context; multiple readings may apply simultaneously to different domains or generations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
