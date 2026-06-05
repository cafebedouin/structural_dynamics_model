% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Sovereign Legitimacy: Constitutional Hybrid Reading (Ceremonial Inheritance + Delegated Power)
 *   domain: political_philosophy/constitutional_theory/comparative_government
 *
 * SUMMARY:
 *   The constitutional hybrid reading presents sovereign legitimacy as
 *   arising from a functional division: inherited ceremonial authority (the
 *   monarch reigns, embodies continuity, grants democratic legitimacy through
 *   historical symbol) combined with delegated power (the elected government
 *   rules, exercises executive function, subject to electoral
 *   accountability). This reading is one of three incommensurable ways of
 *   understanding the kernel of 'sovereign authority' — competing with
 *   monarchical (inherited authority as both reign AND rule) and republican
 *   (delegated authority alone, with no ceremonial inheritance). The
 *   constitutional hybrid uniquely bridges these by splitting them:
 *   legitimacy flows from heredity (symbolic continuity, non-partisan
 *   stability), power flows from delegation (electoral accountability,
 *   programmatic change). This creates a tangled rope structure: genuine
 *   coordination function (the monarch provides neutral succession,
 *   continuity across partisan transitions, historical grounding for
 *   democratic institutions) intertwined with asymmetric extraction (the
 *   hereditary principle constrains reform possibilities, vetos structural
 *   change, subordinates citizens to an inherited authority they cannot
 *   alter). The constraint exhibits rising theater_ratio over its interval
 *   (0.42 → 0.58), indicating that the ceremonial role is becoming
 *   increasingly performative — the legitimacy work it does is declining
 *   relative to the ritual elaboration required to maintain it.
 *   Extractiveness is stable (0.28 → 0.32), reflecting that the structural
 *   extraction from the hereditary veto remains constant despite declining
 *   public acceptance of the legitimacy claim.
 *
 * KEY AGENTS:
 *   - Citizens: Trapped within the legitimacy claim (powerless/trapped) — bear the cost of non-consensual inherited authority
 *   - Reform Coalition (parliamentary + civil society): Organized but constrained (organized/constrained) — can pressure change but face structural entrenchment of hereditary role
 *   - Elected Government: Institutional beneficiary (institutional/arbitrage) — coordinates on the monarch as neutral arbiter; maintains system because it stabilizes executive power
 *   - Hereditary Institution (the Monarchy): Institutional beneficiary (institutional/arbitrage) — derives legitimacy and survival from the hybrid reading; perpetuates itself through constitutional entrenchment
 *   - Transnational Democracy Movement: Organized advocates for pure democracy (organized/mobile) — see the hybrid as transitional scaffold, exerting generational pressure toward republicanism
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the hybrid as inevitable rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.32).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.28).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Sovereign Legitimacy: Constitutional Hybrid Reading (Ceremonial Inheritance + Delegated Power)").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory/comparative_government").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '8f14c8f2-95a9-4a20-b691-1b9815e53495').
narrative_ontology:cs_kernel_codification('8f14c8f2-95a9-4a20-b691-1b9815e53495', formalized).
narrative_ontology:cs_authority_grounding('8f14c8f2-95a9-4a20-b691-1b9815e53495', lineage).
narrative_ontology:cs_interpretation_layer_present('8f14c8f2-95a9-4a20-b691-1b9815e53495').
narrative_ontology:cs_reading_relation('8f14c8f2-95a9-4a20-b691-1b9815e53495', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f14c8f2-95a9-4a20-b691-1b9815e53495', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('8f14c8f2-95a9-4a20-b691-1b9815e53495', foundational, authority_divisible_legitimacy_from_heredity_power_from_delegation).
narrative_ontology:cs_axiom_status(authority_divisible_legitimacy_from_heredity_power_from_delegation, holdable).
narrative_ontology:cs_axiom_grounding('8f14c8f2-95a9-4a20-b691-1b9815e53495', authority_divisible_legitimacy_from_heredity_power_from_delegation, conventional).
narrative_ontology:cs_axiom('8f14c8f2-95a9-4a20-b691-1b9815e53495', secondary, ceremonial_monarch_provides_non_partisan_stability).
narrative_ontology:cs_axiom_status(ceremonial_monarch_provides_non_partisan_stability, holdable).
narrative_ontology:cs_axiom_grounding('8f14c8f2-95a9-4a20-b691-1b9815e53495', ceremonial_monarch_provides_non_partisan_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('8f14c8f2-95a9-4a20-b691-1b9815e53495', institutional_continuity_through_divided_authority).
narrative_ontology:cs_drift_state('8f14c8f2-95a9-4a20-b691-1b9815e53495', contemporary_democratic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8f14c8f2-95a9-4a20-b691-1b9815e53495', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, institutional_continuity).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, political_stability).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, logical_coherence_of_authority).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, radical_democratic_reform).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN (SNARE) — Citizens cannot opt out of the legitimacy claim embedded in the hereditary monarchy. They are trapped within a system where ceremonial authority (inherited, unchangeable) co-constitutes the state's legal personality. Exit requires renouncing citizenship. The extraction is the forced acknowledgment of inherited legitimacy as binding — citizens bear the cost of a sovereignty structure they did not choose and cannot revise.
constraint_indexing:constraint_classification(sovereign_legitimacy__constitutional_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM COALITION (TANGLED ROPE) — Parliamentary and civil society groups benefit from the procedural clarity that constitutional monarchy provides (stable framework for contested powers) but face extraction via the veto-power of inherited authority. Can organize, can pressure change, but change is constrained by the ceremonial role's constitutional entrenchment. Mixed experience: genuine coordination function (stable succession, continuity across governments) alongside structural extraction (symbolic veto on radical reform).
constraint_indexing:constraint_classification(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELECTED GOVERNMENT (ROPE) — Governments coordinate on the ceremonial monarch as a neutral arbiter and continuity symbol. They benefit from the monarch's legitimacy (confers stability on executive action) and the monarch's neutrality (keeps the ceremonial role above partisan dispute). Can exit through republican reform but has strong arbitrage incentive to maintain the hybrid structure. Net coordination with minor extraction (ceremonial prerogatives, constitutional precedent).
constraint_indexing:constraint_classification(sovereign_legitimacy__constitutional_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEREDITARY INSTITUTION (ROPE) — The monarchy as an institution coordinates on its own perpetuation through constitutional entrenchment. Experiences the constraint as pure coordination: the hybrid reading provides legitimacy and survival by splitting authority (ceremonial inheritance handles legitimacy, elected government handles power). High arbitrage value — can maintain institutional existence indefinitely under this reading.
constraint_indexing:constraint_classification(sovereign_legitimacy__constitutional_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSNATIONAL DEMOCRACY MOVEMENT (SCAFFOLD) — International actors promoting republican governance see constitutional monarchy as a temporary transitional form — inherited authority persisting through institutional momentum but increasingly difficult to justify in democratic contexts. The constraint is scaffold-like: it coordinates stability in the short term but contains an intrinsic sunset clause (generational pressure toward pure democracy, declining public acceptance of hereditary legitimacy). Mobile exit: republics can be formed through constitutional reform without institutional collapse.
constraint_indexing:constraint_classification(sovereign_legitimacy__constitutional_hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL AUTHORITY VIEW (MOUNTAIN) — From a civilizational frame, some authority structure is inherent to collective action; the split between ceremonial legitimacy and functional power mirrors a deep structural necessity (every human institution requires both symbolic continuity and practical adaptation). This perspective sees the constitutional hybrid as an inevitable natural form, not a contingent arrangement. However, the extractive mechanisms visible from other perspectives suggest this is a false summit — the 'inevitability' naturalizes a specific institutional choice, not a law of human nature.
constraint_indexing:constraint_classification(sovereign_legitimacy__constitutional_hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereign_legitimacy__constitutional_hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereign_legitimacy__constitutional_hybrid_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The constitutional hybrid extracts from citizens through the non-consensual imposition of inherited authority as a component of state legitimacy. However, the extraction is tempered by genuine coordination benefits: the monarch does provide non-partisan stability, generational continuity, and a neutral symbol that elected governments can appeal to. This is not pure snare (which would extract with minimal coordination) nor pure rope (which would coordinate with minimal extraction). The moderate value reflects the authentic hybrid: real functional benefits alongside real structural constraints on reform. Suppression (0.28): Moderate-low. Citizens cannot exit the state's legitimacy claim (national boundary, territorial sovereignty), but suppression is not severe because: (1) the hereditary authority is formally ceremonial, not dictatorial; (2) elected government provides procedural voice; (3) republican alternatives exist in international law and constitutional theory, making the choice visible (though difficult to exercise). Theater ratio (0.58): Moderate-high. The ceremonial role involves substantial theatrical performance (coronation rituals, constitutional prerogatives, formal state functions) relative to functional power (the monarch actually governs very little in constitutional monarchies). The rising trajectory reflects increasing difficulty in justifying the performance as anything other than tradition — legitimacy is being maintained through ritual elaboration even as public acceptance of the underlying claim (inherited authority is legitimate) declines. Claimed type (tangled rope): The constraint requires active enforcement (constitutional entrenchment of hereditary succession, legal prohibitions on abolishing the institution without supermajority amendment); it has genuine beneficiaries (institutional continuity, political stability, neutral arbitration across partisan transitions); it has genuine victims (those who object to inherited authority, those constrained by the structure's resistance to radical reform).
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals radically different classifications across observer positions. The citizen trapped within the nation-state sees pure extraction and coercion (snare) — they are subordinated to an inherited authority they did not choose. The reform coalition sees mixed coordination and extraction (tangled rope) — the system both provides stable procedure and constrains their ambitions. The elected government sees coordination (rope) — the monarch neutralizes partisan conflict and provides executive stability. The monarchy sees coordination (rope) — the hybrid reading ensures institutional survival. The transnational democracy movement sees a temporary form with an intrinsic sunset (scaffold) — constitutional monarchies are bridging toward pure democracy, not permanent fixtures. The civilizational analyst sees a natural necessity (mountain) — all states need both symbolic continuity and practical power, making the split inevitable. Yet the analytical view is a false summit: the split is a specific institutional choice, not a law of human nature. Republics also manage continuity (through constitutional courts, ceremonial presidencies, historical narrative) and power (through elected executives). The 'inevitability' of the hybrid naturalizes a particular solution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from beneficiary/victim status plus exit options. Citizens classified as powerless/trapped experience high d (near 1.0) — they bear costs, have no structural exit, maximum experienced extraction. The reform coalition as organized/constrained experiences moderate d (around 0.55–0.60) — they are victims of structural constraints but have organizational capacity and some exit paths (constitutional amendment, electoral pressure). The elected government as institutional/arbitrage experiences low d (around 0.20–0.30) — they are beneficiaries with high exit capacity (can reform or abolish through constitutional processes). The hereditary institution as institutional/arbitrage experiences near-zero d (0.05–0.15) — full beneficiary with maximum exit capacity (can voluntarily accept ceremonial limitation or resist reform). The democracy movement as organized/mobile experiences moderate d (around 0.50–0.60) — they are advocates for reform with significant organizational capacity and historical momentum on their side. The analytical observer at 0.72–0.75 experiences structural ambiguity — they see both the genuine benefits (coordination, stability) and genuine harms (exclusion, constraint), placing them in the middle of the directionality spectrum. The canonical derivation f(d) maps these to experienced extractiveness chi; the perspectives are indexed by (P,T,E,S) tuples that produce the classification types listed.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constitutional hybrid is a READING of the legitimacy kernel, not 'the truth' about authority. The mandate to avoid mislabeling extraction as coordination (or vice versa) is met by indexing to observer position: from the government's perspective, it is genuinely coordination (the monarch stabilizes). From the citizen's perspective, it is genuinely extraction (the monarch constrains). The tangled rope classification captures this: the constraint has both coordination and extraction, and they are not illusory — they are real features of the divided structure. The mandate to avoid calling something a mountain when it is merely naturalizing a contingent choice is met by the analytical perspective showing the false summit: civilizational observers risk naturalizing the hybrid as inevitable, but comparative evidence shows it is one choice among logically coherent alternatives (monarchical, republican). The theta-fix is correct classification relative to standpoint: rope from the beneficiary's position (coordination, arbitrage exit), snare from the victim's position (extraction, trapped exit), scaffold from the reformer's position (temporary with sunset clause), piton from the institutional perspective (performative ritual of declining functional value), mountain from the analytical perspective that mistakes design choice for natural law (false summit). The gap across these is not an error — it is a feature. It reveals that 'what is the constraint?' has different answers for different reasons that are all valid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_vs_pure_forms_logical_status,
    'Is the constitutional hybrid (ceremonial + delegated) a logically coherent third form or an unstable interpolation between monarchical and republican poles?',
    'Formal analysis of legitimacy claims: can a single state body simultaneously ground authority in two incommensurable sources (inherited heredity AND popular delegation)? Historical analysis of constitutional hybrids that collapsed toward one pole vs those that stabilized.',
    'If logically coherent: tangled rope classification stands; the constraint coordinates genuine mixed function. If unstable interpolation: reclassify toward snare (extraction via logical incoherence) or piton (theatrical maintenance of contradiction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_pure_forms_logical_status, conceptual, 'Whether the constitutional hybrid is logically coherent or unstable interpolation').

omega_variable(
    which_reading_has_institutional_momentum,
    'Does the constitutional hybrid reading have greater institutional staying power than monarchical or republican readings in contemporary democracies?',
    'Comparative institutional analysis: examine the trajectory of 15+ constitutional monarchies over 50 years; measure reform pressure, constitutional amendment frequency, and public opinion on hereditary authority. Compare to republican and purely ceremonial trends.',
    'If hybrid has greater momentum: confirms scaffold perspective (real institutional form with generational stability). If momentum favors pure republican or ceremonial: hybrid is degrading toward piton (performance masking structural decline).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(which_reading_has_institutional_momentum, empirical, 'Institutional momentum of constitutional hybrid vs pure readings').

omega_variable(
    kernel_reading_incommensurability,
    'Are the monarchical, republican, and constitutional hybrid readings genuinely incommensurable (unable to coexist in one framework) or do they represent different emphasis within a shared commitment to legitimate authority?',
    'Formal comparison of axioms across readings: identify contradictions vs complementarities. Historical analysis of hybrid constitutions that transitioned toward pure forms — did the readings foreclose each other or merely lose political support?',
    'If incommensurable: the hybrid reading forecloses monarchical and republican readings (they cannot coexist). If commensurable: the readings coexist_with each other (different parties hold them simultaneously). This determines the reading_relations topology in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Logical incommensurability of kernel readings').

omega_variable(
    extractive_asymmetry_source,
    'Does the extraction in this reading flow from the constitutional entrenchment of inherited authority (structural) or from the specific occupant of the hereditary role (contingent)?',
    'Counterfactual: compare extractiveness under weak vs strong hereditary occupants. If weak occupants can be ceremonial-only (low extraction), the source is contingent; if the structure itself extracts regardless of occupant, the source is structural.',
    'If structural: suppression and extraction are intrinsic to the hybrid form. If contingent: the constraint could be rope-like under different leadership, suggesting the current snare/tangled rope experience is institution-specific, not reading-inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_asymmetry_source, empirical, 'Whether extraction is structural or contingent on occupant').

omega_variable(
    ceremonial_neutrality_assumption,
    'Can a hereditary institution actually maintain political neutrality, or does inherited authority inevitably bias the state toward conservatism?',
    'Historical analysis: measure the policy direction of constitutional monarchies vs pure democracies on reform speed, redistribution, and institutional innovation. Identify whether hereditary authority correlates with conservative bias even when formally neutral.',
    'If neutrality is genuine: the coordination benefit (stable arbiter) is real, tangled rope classification stands. If neutrality is illusory: the constraint extracts through hidden conservative bias, reclassify toward snare with hidden mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_neutrality_assumption, empirical, 'Whether ceremonial authority can maintain political neutrality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_const_hybrid_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sov_const_hybrid_tr_t2, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2, 0.5).
narrative_ontology:measurement(sov_const_hybrid_tr_t4, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(sov_const_hybrid_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sov_const_hybrid_be_t2, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(sov_const_hybrid_be_t4, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 4, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(sov_const_hybrid_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(sov_const_hybrid_su_t2, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2, 0.25).
narrative_ontology:measurement(sov_const_hybrid_su_t4, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 4, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__constitutional_hybrid_reading, 0.18).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_amendability_ceiling).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, executive_power_delegation_structure).

% DUAL FORMULATION NOTE:
% The constitutional hybrid reading is ONE of THREE readings of the 'sovereign_legitimacy' kernel. The monarchical and republican readings are separate constraint stories with different epsilon values and different beneficiary/victim structures. This story (constitutional_hybrid_reading) has epsilon=0.32 and splits authority into ceremonial+delegated. The monarchical reading has authority unified in inherited role (different epsilon, different authority_grounding). The republican reading has authority unified in delegation (third epsilon, third authority_grounding). All three are linked via network.affects_constraints. Do not conflate them — each is a clean constraint story conforming to epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
