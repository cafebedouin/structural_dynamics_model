% ============================================================================
% CONSTRAINT STORY: dharmasastra_competing_interpretive_frames
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_competing_interpretive_frames, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dharmasastra_competing_interpretive_frames
 *   human_readable: Dharmasastra Schools as Competing Interpretive Authority
 *   domain: religion/legal_traditions
 *
 * SUMMARY:
 *   The dharmasastra schools (Mitakshara, Dayabhaga, and others) represent a
 *   remarkable institutional solution to the problem of scaling interpretive
 *   authority across heterogeneous populations. Rather than enforcing
 *   interpretive monopoly — which would require either suppressing all
 *   competing claims (politically expensive) or uniform application of alien
 *   rules (socially destabilizing) — the brahminical system permitted
 *   multiple incompatible interpretations to coexist as long as each claimed
 *   fidelity to a shared Vedic kernel. This configuration enabled both
 *   genuine coordination (each region achieved stable, predictable law
 *   through its dominant school) and systemic extraction (brahminical elites
 *   captured interpretive authority, women and non-elite populations
 *   navigated incompatible rules they could not escape, sophisticated actors
 *   exploited cross-jurisdictional variation). The constraint's
 *   extractiveness has risen over time (0.22 to 0.38 across the interval) as
 *   imperial administration and later colonial systems forced confrontation
 *   with incompatible rules, generating the need for explicit rule
 *   hierarchies and conflict-of-laws principles. Theater ratio has similarly
 *   risen (0.35 to 0.58) as brahminical authorities increasingly performed
 *   kernel fidelity while openly acknowledging that schools taught
 *   incompatible interpretations. The configuration demonstrates how
 *   interpretive accretion can scale across large, heterogeneous populations
 *   without requiring interpretive monopoly — but at the cost of embedding
 *   extraction into the pluralism mechanism itself.
 *
 * KEY AGENTS:
 *   - Mitakshara School: Institutional authority holder (institutional/arbitrage) — northern regions' dominant school; genuinely coordinates inheritance and marriage law; captures authority to interpret Vedic dharma
 *   - Dayabhaga School: Institutional authority holder (institutional/arbitrage) — southern/eastern regions' dominant school; parallel to Mitakshara; incompatible interpretations coexist
 *   - Regional Brahminical Elites: Institutional beneficiaries (institutional/arbitrage) — profit from school monopoly on interpretation; secure elite status through brahminical authority
 *   - Women Subject to Inheritance Rules: Powerless victims (powerless/trapped) — face incompatible inheritance and marriage rules; cannot exit regional jurisdiction; bear costs of variation asymmetrically
 *   - Cross-Regional Merchants: Powerful arbitrageurs (powerful/arbitrage) — navigate rule variation with legal expertise; exploit gaps between schools' interpretations
 *   - Local Non-Elite Populations: Moderate victims (moderate/constrained) — navigate incompatible rules with limited legal expertise; experience extraction without arbitrage capacity
 *   - Imperial/Colonial Administrators: External agents (powerful/constrained) — forced to make explicit rule hierarchies when schools' incompatibilities blocked unified administration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the pluralism mechanism as immutable feature of law-at-scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_competing_interpretive_frames, 0.38).
domain_priors:suppression_score(dharmasastra_competing_interpretive_frames, 0.42).
domain_priors:theater_ratio(dharmasastra_competing_interpretive_frames, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_competing_interpretive_frames, extractiveness, 0.38).
narrative_ontology:constraint_metric(dharmasastra_competing_interpretive_frames, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dharmasastra_competing_interpretive_frames, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_competing_interpretive_frames, tangled_rope).
narrative_ontology:human_readable(dharmasastra_competing_interpretive_frames, "Dharmasastra Schools as Competing Interpretive Authority").
narrative_ontology:topic_domain(dharmasastra_competing_interpretive_frames, "religion/legal_traditions").

domain_priors:requires_active_enforcement(dharmasastra_competing_interpretive_frames).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_competing_interpretive_frames, regional_brahminical_elite).
narrative_ontology:constraint_beneficiary(dharmasastra_competing_interpretive_frames, interpretive_school_authority_holders).
narrative_ontology:constraint_victim(dharmasastra_competing_interpretive_frames, cross_regional_legal_coherence).
narrative_ontology:constraint_victim(dharmasastra_competing_interpretive_frames, women_subject_to_incompatible_inheritance_rules).
narrative_ontology:constraint_victim(dharmasastra_competing_interpretive_frames, local_populations_navigating_conflicting_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MITAKSHARA SCHOOL (ROPE) — Operates as genuine coordination mechanism within northern regions. Provides interpretive stability, enables legal predictability for inheritance and marriage disputes, coordinates brahminical authority around shared hermeneutic principles. The school experiences itself as solving a real coordination problem: how to apply eternal Vedic law to changing social conditions. Experiences minimal extraction — the coordination benefit (predictable law, shared interpretive framework) flows to the community that adopts the school. No significant suppression of alternatives at regional scale because the school has achieved dominant adoption through what it perceives as interpretive superiority.
constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 2: DAYABHAGA SCHOOL (ROPE) — Parallel rope classification for southern/eastern regional adoption. Provides equally genuine coordination within its regional sphere. The school is not experienced as extraction by adherents — it solves the legitimate coordination problem of applying dharmasastra to local inheritance and social conditions. Operates with equal perceived kernel fidelity to Mitakshara. Like Mitakshara, experiences minimal extraction because the interpretive coordination mechanism appears to serve the community's needs. Institutional actors with arbitrage options: schools can migrate interpretations in response to changing conditions while maintaining kernel fidelity claims.
constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: WOMEN SUBJECT TO INHERITANCE VARIATION (SNARE) — Within a single region, Mitakshara vs Dayabhaga inheritance rules produce incompatible rights. A woman in a Mitakshara jurisdiction has different property rights, succession standing, and marital authority than her counterpart in a Dayabhaga jurisdiction. If a woman marries across the jurisdictional boundary or relocates, her legal status becomes incoherent. No exit option: the rule applies based on regional domicile and caste, neither of which is freely mobile. Suppression is high: alternative interpretations are precluded by regional brahminical enforcement. The constraint extracts asymmetry: a brahminical authority (the school) captures the power to define her legal status; she bears the cost of incompatible rules she cannot escape.
constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: CROSS-REGIONAL MERCHANT AND ADMINISTRATIVE CLASSES (TANGLED ROPE) — Actors operating across multiple jurisdictions experience the variation as mixed coordination and extraction. Legitimate coordination: the system enables regional autonomy and prevents any single school from imposing universal rules, which would trigger violent resistance. Each region retains interpretive authority — a genuine coordination achievement. Extraction: merchants operating across regions must navigate incompatible inheritance, marriage, and obligation rules. A merchant's inheritance claim recognized in Mitakshara territory is invalid in Dayabhaga territory. Administrative officials (sultanates, later colonial bureaucrats) must either maintain parallel legal systems or impose one school's rules on alien populations, generating resentment. The powerful merchant class can afford legal expertise to navigate the variation; local populations cannot. Arbitrage exists: sophisticated actors exploit the gaps between interpretations for financial or political advantage.
constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: UNIFIED VEDIC KERNEL CLAIM (PITON) — The brahminical establishment maintains that all competing schools interpret a single Vedic dharma. This unifying claim is largely performative: it permits the schools to coexist without openly admitting that dharma is fragmented. The kernel claim functions as inertial institutional theater — schools must continuously perform kernel fidelity even though their interpretations contradict each other. The performance persists because admitting interpretive fragmentation would undermine brahminical authority claims. The constraint is degraded: the original coordination function (unified law) has atrophied into ritual consensus-performance. Yet the performance persists because the alternative — open acknowledgment of interpretive pluralism — would require legitimating mechanisms the brahminical system does not possess.
constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: REGIONAL BRAHMINICAL COALITIONS (TANGLED ROPE) — Regional brahminical networks maintain schools as mechanisms for both genuine coordination (enabling stable law within the region) and extraction (preventing alternative schools, suppressing non-brahminical legal authorities, securing brahminical monopoly on interpretation). The constraint is hybrid: regions genuinely coordinate around a school, but the schools together suppress alternative legal frameworks and non-brahminical authority systems. Enforcement is required: dissent is handled through ritual exclusion, status degradation, or administrative pressure. The schools have constrained exit options — moving between schools or abandoning the brahminical framework carries significant social cost, but some institutional flexibility exists (imperial courts sometimes overrode school preference, heterodox movements occasionally challenged brahminical authority).
constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, the variation among schools appears as an immutable feature of how legal authority scales across heterogeneous populations: any system claiming to unify large regions with incompatible social practices must either impose uniform rules (generating resistance) or permit local variation under unifying meta-claims (kernel fidelity). This perspective risks naturalizing what is actually a contingent institutional achievement — the brahminical system's specific solution to the pluralism problem. However, the structural data indicates this is a false summit: identifiable beneficiaries (brahminical authority holders, regional elites) profit from the configuration. If the beneficiary/victim asymmetry is present, the constraint is not a natural law but a constructed coordination mechanism with built-in extraction.
constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_competing_interpretive_frames_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dharmasastra_competing_interpretive_frames, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(dharmasastra_competing_interpretive_frames, TR),
    TR >= 0.70.

:- end_tests(dharmasastra_competing_interpretive_frames_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The system delivers genuine coordination within regions (each school provides stable, interpretable law) but enables extraction through two mechanisms: (1) brahminical monopoly on interpretation authority, preventing non-brahminical legal frameworks and heterodox challenges; (2) cross-jurisdictional arbitrage, where sophisticated actors profit from rule variation that harms non-elite populations. Extractiveness has risen over time because imperial administration exposed the incompatibilities — the system worked as long as regions remained isolated; interconnection revealed the extraction mechanism. Suppression (0.42): Moderate. Significant suppression of alternative legal frameworks and non-brahminical authorities, but not total. Some regions developed quasi-parallel systems, heterodox movements occasionally challenged brahminical authority, and colonial rule eventually overrode brahminical law entirely. Theater ratio (0.58): Moderate-high. Brahminical authorities increasingly perform kernel fidelity (claiming unified dharma) while publicly acknowledging that schools teach incompatible rules. The performance persists because admitting interpretive fragmentation would undermine brahminical authority legitimacy. The theater has increased over the interval as the gap between kernel claims and operational pluralism widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival heterogeneity. The Mitakshara school experiences genuine coordination — the system enables predictable law and stable authority within northern regions. The Dayabhaga school has parallel genuine experience in its region. But women subject to incompatible inheritance rules across different schools experience pure extraction: their legal status depends on regional domicile (not freely mobile) and is determined by rules they cannot change. Cross-regional merchants experience mixed extraction and coordination: the system constrains them (incompatible rules, legal uncertainty) but enables sophisticated arbitrage (those with expertise profit). Regional brahminical authorities experience their own kernel claims as performative (piton): they must continuously assert kernel unity while acknowledging operational pluralism. The analytical observer at civilizational scale risks seeing an immutable natural law (mountain) — pluralism-at-scale must use kernel claims — but the structural data suggests this is a false summit: the brahminical system's specific solution to pluralism is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent position. Institutional schools (Mitakshara, Dayabhaga) experience low d → low χ because they are net beneficiaries (arbitrage exit: they can reinterpret to adapt to changing conditions). Regional brahminical elites are institutional beneficiaries with arbitrage options: d ≈ 0.10-0.15, f(d) ≈ -0.05, χ is small or negative (they experience the system as coordination). Women trapped in incompatible rules: d ≈ 0.95, f(d) ≈ 1.42, χ scales to high extraction. Cross-regional merchants with legal expertise: d ≈ 0.35-0.40 (neither full victim nor full beneficiary; they profit from variation but also bear some cost of navigating it). Local non-elite populations: d ≈ 0.70-0.80, f(d) ≈ 1.15-1.25, experiencing significant extraction. The perspectival gaps are large: institutional actors see rope (coordination), powerless women see snare (extraction), merchant class sees tangled_rope (mixed), regional brahmins maintaining performance see piton (degraded theater).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that coordination and extraction are not binary categories but structural dimensions that combine. The system achieves genuine coordination within regions (schools provide stable, interpretable law that enables social coordination) AND enables systematic extraction (through brahminical monopoly on interpretation and cross-jurisdictional arbitrage that harms non-elite populations). These are not contradictory classifications — they are simultaneously true at different perspectival positions. From the perspective of a regional population unified by school adherence, the constraint is rope (pure coordination). From the perspective of a woman navigating incompatible rules across regions, the constraint is snare (pure extraction). From the perspective of a cross-regional actor, the constraint is tangled_rope (mixed). The mandatrophy resolves by recognizing that the same structural configuration is experienced differently by agents in different structural positions relative to the constraint. The mountain perspective (naturalizing pluralism-at-scale as requiring kernel claims) is a false summit: the system's specific solution is contingent, not necessary. The constraint's extractiveness and theater metrics (rising over time) indicate that the system becomes increasingly theatrical (performing kernel unity while operationalizing pluralism) as imperial administration exposes the incompatibilities. The piton classification reflects this degradation: the kernel claim persists through institutional inertia even as its functional coordination value atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_vs_accretion,
    'Is there a singular Vedic kernel, or does ''the kernel'' become whatever each school claims to interpret? At what point does interpretive accretion consume the unifying function?',
    'Textual analysis: identify contradictions between school interpretations that cannot both be faithful to the same Vedic passages. If contradictions are endemic, the kernel is not a constraint — it is theater. If contradictions are resolvable through hermeneutic techniques (sambandha, context, hierarchical authority), the kernel remains constraining.',
    'If kernel is singular: system is rope (genuine coordination around shared text). If kernel is consumed by accretion: system is piton (kernel claims are performative, actual coordination is regional precedent). If kernel is strategic fiction: system is snare/tangled_rope (kernel fidelity claims suppress admission of pluralism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_singularity_vs_accretion, empirical, 'Whether the Vedic kernel is genuinely singular or has been fragmented by interpretive accretion').

omega_variable(
    brahminical_monopoly_mechanism,
    'How much of the constraint''s persistence depends on brahminical institutional monopoly vs. genuine coordination value of the school system?',
    'Historical cases: moments when brahminical monopoly weakens (heterodox movements, imperial courts reducing brahminical authority, colonial legal replacement). Do regions adopt alternative legal systems when monopoly pressure lifts? Or do they retain school-based coordination because it delivers genuine value?',
    'If monopoly-dependent: constraint is primarily snare/tangled_rope (suppression is central mechanism). If coordination-dependent: constraint is primarily rope (schools are genuinely useful coordination devices). If mixed: constraint structure depends on regional context — some regions rely more on monopoly, others more on coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_monopoly_mechanism, empirical, 'Relative importance of brahminical monopoly vs. coordination value').

omega_variable(
    cross_jurisdictional_cost_incidence,
    'Who bears the costs of navigating cross-jurisdictional variation — sophisticated merchant classes with legal expertise, or non-elite populations?',
    'Dispute resolution records, merchant account books, administrative documents: track who petitions for rule clarification, who hires brahminical legal advisors, who loses disputes across jurisdictional boundaries. High elite success rate in cross-jurisdictional disputes suggests effective arbitrage; high non-elite loss rate suggests extraction.',
    'If costs are borne by elites: variation is a neutral coordination device (rope-like for all). If costs are borne by non-elites: variation is extraction mechanism enabling arbitrage by sophisticated actors (tangled_rope/snare for non-elites).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_jurisdictional_cost_incidence, empirical, 'Incidence of cross-jurisdictional rule variation costs by social class').

omega_variable(
    interpretive_flexibility_vs_rigidity,
    'Do schools permit interpretive flexibility to respond to changing conditions, or do they rigidify interpretations once established?',
    'Longitudinal analysis of school commentarial tradition: do later commentaries expand or constrain interpretive scope? Are schools responsive to imperial pressure or resistant? Do they reinterpret to accommodate new social conditions or cling to ancient precedent?',
    'If flexible: schools function as genuine coordination mechanisms that can adapt (rope-like). If rigid: schools become constraint on adaptation, forcing regions to either abandon schools or violate social realities (piton or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_vs_rigidity, empirical, 'Whether schools permit interpretive flexibility or enforce rigidity').

omega_variable(
    false_summit_kernel_naturalization,
    'Is the mountain classification (natural law immutability of the pluralism problem) a genuine recognition of structural necessity, or a naturalization of a contingent institutional solution?',
    'Comparative analysis: do non-brahminical legal systems in contemporary and historical contexts solve the pluralism problem (allowing local variation under unifying meta-claims) without schools/kernel claims? If yes, the brahminical solution is contingent, not necessary.',
    'If contingent: mountain is false summit; system is actually tangled_rope with beneficiaries (brahminical authority, regional elites) who profit from naturalizing the configuration. If necessary: mountain holds; pluralism-at-scale may require kernel claims or equivalent mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_kernel_naturalization, conceptual, 'Whether the pluralism-at-scale problem requires kernel claims or represents naturalization of contingent brahminical solution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_competing_interpretive_frames, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dharma_tr_t0, dharmasastra_competing_interpretive_frames, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dharma_tr_t500, dharmasastra_competing_interpretive_frames, theater_ratio, 500, 0.45).
narrative_ontology:measurement(dharma_tr_t1000, dharmasastra_competing_interpretive_frames, theater_ratio, 1000, 0.55).
narrative_ontology:measurement(dharma_tr_t1500, dharmasastra_competing_interpretive_frames, theater_ratio, 1500, 0.58).

% Extraction over time
narrative_ontology:measurement(dharma_be_t0, dharmasastra_competing_interpretive_frames, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dharma_be_t500, dharmasastra_competing_interpretive_frames, base_extractiveness, 500, 0.32).
narrative_ontology:measurement(dharma_be_t1000, dharmasastra_competing_interpretive_frames, base_extractiveness, 1000, 0.38).
narrative_ontology:measurement(dharma_be_t1500, dharmasastra_competing_interpretive_frames, base_extractiveness, 1500, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_competing_interpretive_frames, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_competing_interpretive_frames, vedic_authority_establishment).
narrative_ontology:affects_constraint(dharmasastra_competing_interpretive_frames, brahminical_monopoly_on_interpretation).
narrative_ontology:affects_constraint(dharmasastra_competing_interpretive_frames, cross_regional_legal_coherence).

% DUAL FORMULATION NOTE:
% This constraint story decomposes 'dharmasastra as interpretive authority system' into three structurally distinct constraints: (1) vedic_authority_establishment (ε ≈ 0.15, Rope) — how the brahminical system established the Vedas as authoritative source, genuine coordination mechanism for legal legitimation across regions; (2) brahminical_monopoly_on_interpretation (ε ≈ 0.65, Snare) — suppression of non-brahminical legal frameworks and heterodox authorities; (3) dharmasastra_competing_interpretive_frames (ε ≈ 0.38, Tangled Rope) — how interpretive pluralism coexists with monopoly authority, enabling both coordination and extraction. The three constraints are linked: establishment creates the authority structure; monopoly defends it; pluralism operates within monopoly. Each has different ε values because they have different structural functions and are measured by different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
