% ============================================================================
% CONSTRAINT STORY: hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_scaffolding_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_scaffolding_reading
 *   human_readable: Hybrid Scaffolding: Mandated Practice Adoption via Elite Modeling and Ideological Framing
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint models the structural mechanism by which a state-mandated
 *   cultural practice achieves partial displacement of indigenous practices
 *   through a hybrid method: top-down decree (pure coercion) is insufficient
 *   and generates resistance; organic cultural evolution from below
 *   (endogenous climb) is too slow; but scaffolded imposition—combining elite
 *   infrastructure modeling, ideological messaging framing, and material
 *   incentives—achieves stable partial adoption within 1-3 generations. The
 *   exemplar case is dress codes and associated status markers in colonial
 *   and early-national state formation: pure decree to 'wear the official
 *   style' fails because enforcement cannot reach everywhere and provides no
 *   mechanism for ordinary people to learn the new practices. Organic
 *   adoption is slow because there is no initial incentive. But when combined
 *   with (1) elite modeling visible in courts and cities, (2) ideological
 *   framing that equates the practice with civilization/progress/proper
 *   identity, and (3) material rewards (land, jobs, commercial access) for
 *   adopters, the practice achieves partial penetration into the population.
 *   Rural elites and merchants adopt to signal status and gain access; some
 *   farmers and craftspeople adopt as they migrate to cities or engage in
 *   commerce; younger generations raised in mixed-practice households accept
 *   the imposed practice as normal. After 50-100 years, the practice appears
 *   'natural' to participants and observers alike. The constraint's
 *   extractiveness (0.52) reflects that this is genuine extraction—subaltern
 *   populations lose cultural autonomy and bear psychological cost of
 *   displacement—but also partial coordination (the state genuinely solves a
 *   consolidation problem, and some adopters genuinely benefit from the
 *   networks and opportunities the scaffolding creates). The theater_ratio
 *   trajectory (0.30 → 0.56 → 0.72) shows how the functional coercive
 *   mechanisms (material incentives, practical training, network access) are
 *   gradually replaced by performative ideological maintenance (endless
 *   reiteration that the practice is 'natural,' 'evolved from within,'
 *   'inevitable progression') as the practice becomes demographically
 *   dominant. This reading is one of three competing interpretations of how
 *   imposed practices achieve cultural displacement; it differs from the
 *   exogenous_override_reading (pure state decree) and the
 *   endogenous_climb_reading (purely organic adoption) by asserting that the
 *   intermediate scaffolding mechanism is the empirically robust pattern in
 *   successful state-directed cultural change.
 *
 * KEY AGENTS:
 *   - Urban Political Elites: Institutional actors (arbitrage/mobile) — design and maintain the scaffolding infrastructure (elite modeling, resource allocation, enforcement coordination); benefits from state consolidation and cultural legitimacy; experiences the constraint as coordination (rope) despite asymmetric extraction toward them
 *   - Rural Populations: Powerless victims (trapped) — primary targets of cultural imposition; bear full cost of displacement without meaningful participation in scaffolding design; face coercion and suppression of subaltern practices; experience maximal extraction (snare)
 *   - Secondary Adopters: Moderate-power transitional agents (constrained) — merchants, minor administrators, educated rural persons with upward mobility incentive; adopt the imposed practice to gain elite access; experience genuine coordination benefits (network access, economic opportunity) alongside extraction (cultural loss, legitimacy labor); tangled_rope experience reflects both dimensions
 *   - Religious/Cultural Authority Institutions: Institutional actors (arbitrage) — schools, temples, media, clerical hierarchies; maintain ideological messaging framing; shift over time from genuine persuasion toward performative maintenance; piton classification emerges from their degradation from functional coordination to theater
 *   - Organized Resistance Networks: Organized agents (mobile) — cultural preservation societies, dissidents, underground schools; maintain alternative practices; have genuine exit options (geographic, institutional, organizational); experience constraint as temporary scaffold with declining force; perspective reflects that organized agents can resist effectively
 *   - Analytical Observer (Civilizational): Sees the outcome as 'natural cultural evolution' without noticing ongoing enforcement; risks false summit (mountain) classification by naturalizing what remains a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_scaffolding_reading, 0.52).
domain_priors:suppression_score(hybrid_scaffolding_reading, 0.48).
domain_priors:theater_ratio(hybrid_scaffolding_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_scaffolding_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hybrid_scaffolding_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hybrid_scaffolding_reading, theater_ratio, 0.56).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_scaffolding_reading, "Hybrid Scaffolding: Mandated Practice Adoption via Elite Modeling and Ideological Framing").
narrative_ontology:topic_domain(hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hybrid_scaffolding_reading, distributed).
narrative_ontology:cs_authority_grounding(hybrid_scaffolding_reading, practice).
narrative_ontology:cs_interpretation_layer_present(hybrid_scaffolding_reading).
narrative_ontology:cs_kernel_id(hybrid_scaffolding_reading, legitimacy_of_imposed_practice).
narrative_ontology:cs_reading_relation(hybrid_scaffolding_reading, exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation(hybrid_scaffolding_reading, endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom(hybrid_scaffolding_reading, foundational, scaffolded_infrastructure_enables_adoption).
narrative_ontology:cs_axiom_status(scaffolded_infrastructure_enables_adoption, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_scaffolding_reading, scaffolded_infrastructure_enables_adoption, empirically_contingent).
narrative_ontology:cs_axiom(hybrid_scaffolding_reading, foundational, imposed_practice_achieves_stable_partial_displacement).
narrative_ontology:cs_axiom_status(imposed_practice_achieves_stable_partial_displacement, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_scaffolding_reading, imposed_practice_achieves_stable_partial_displacement, empirically_contingent).
narrative_ontology:cs_reference_frame(hybrid_scaffolding_reading, pre_imposition_baseline).
narrative_ontology:cs_drift_state(hybrid_scaffolding_reading, post_scaffolding_naturalization, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_scaffolding_reading, urban_political_elites).
narrative_ontology:constraint_beneficiary(hybrid_scaffolding_reading, state_legitimacy_apparatus).
narrative_ontology:constraint_victim(hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_victim(hybrid_scaffolding_reading, subaltern_cultural_practices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL POPULATION (SNARE) — No structural exit from the imposed practice regime. Coercion operates through resource dependency (economic incentives tied to adoption), social surveillance (elite approval for adopters), and cultural delegitimization of alternatives. Maximal extraction: the subject population bears the full cost of cultural displacement without meaningful participation in the scaffolding infrastructure that makes adoption navigable for urban elites.
constraint_indexing:constraint_classification(hybrid_scaffolding_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECONDARY ADOPTER / CONSTRAINED UPWARD MOBILITY (TANGLED ROPE) — The moderate-power agent (merchant, minor administrator, educated rural person) faces both coordination benefit and asymmetric extraction. Adoption of the imposed practice creates access to elite networks, commercial opportunities, and administrative advancement. But adoption also requires abandoning prior cultural identity and bearing the psychological/social cost of that abandonment. The state coordinates this upward mobility; simultaneously, it extracts legitimacy capital (the modest success story becomes propaganda for 'natural' adoption) and cultural labor (the adopter must perform authentic enthusiasm to maintain access). Suppression is real but surmountable — constrained exit reflects that the path upward exists and some navigate it, but at significant cost.
constraint_indexing:constraint_classification(hybrid_scaffolding_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN POLITICAL ELITES (ROPE) — The elites experience the constraint as pure coordination: managing the infrastructure of elite modeling (dress codes, language, behavioral norms), distributing ideological messaging (schools, media, clerical authority), and providing material incentives (jobs, land grants, status markers) for adoption. The elite perspective sees this as solving a collective action problem: 'How do we establish a unified state identity that overcomes regional/cultural fragmentation?' From within this perspective, extraction is invisible — the whole apparatus is framed as coordination for public benefit. Arbitrage exit means elites can exit if they choose (they are not trapped), but they benefit from maintaining the system.
constraint_indexing:constraint_classification(hybrid_scaffolding_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IDEOLOGICAL APPARATUS / RELIGIOUS-CULTURAL AUTHORITY (PITON) — Schools, temples, clerical hierarchies, and media institutions maintain the propagandistic claim that the imposed practice is 'natural,' 'inevitable,' or 'divinely sanctioned.' Over time, the performative content of this messaging increases relative to its functional content. The apparatus becomes self-perpetuating ritual: endless repetition of legitimacy claims without genuine persuasive force. The theater_ratio reflects that much institutional activity is spent maintaining the appearance of willing adoption rather than managing actual coercive mechanisms. The piton classification emerges not from maximal extraction but from degradation of the original coordination function (genuine persuasion) into pure performative maintenance.
constraint_indexing:constraint_classification(hybrid_scaffolding_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED RESISTANCE / CULTURAL PRESERVATION (SCAFFOLD) — Organized groups (religious dissidents, cultural preservation societies, underground alternative schools) see the scaffolded imposition as a temporary constraint with a structural sunset. They maintain parallel institutions (hidden schools, clandestine rituals, oral transmission networks) that preserve subaltern practices and knowledge. Their effective extraction from the state apparatus is low because they have agency: they can exit (geographically or institutionally), they can sustain alternatives, and they can seed revival of suppressed practices when state enforcement weakens. The scaffold classification reflects that organized resistance creates an alternative coordination pathway with declining suppression as the organized agents accumulate resources and legitimacy.
constraint_indexing:constraint_classification(hybrid_scaffolding_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CIVILIZATIONAL ANALYTICAL OBSERVER (MOUNTAIN / FALSE SUMMIT CANDIDATE) — From a universal/civilizational time horizon, the imposed practice appears to become the 'natural' cultural baseline after 2-3 generations of adoption. The older subaltern practices fade from memory; younger cohorts take the imposed practice as their native identity. The naturalizing observer concludes: 'This is simply how this culture evolved; there was no imposition, merely displacement of less-viable practices by more adaptive ones.' This perspective risks misclassifying a contingent institutional arrangement (sustained by ongoing scaffolding infrastructure and state enforcement) as an immutable feature of the cultural landscape. The engine will detect this as a false summit: identifiable beneficiaries (elites) and ongoing enforcement mechanisms contradict the natural law classification.
constraint_indexing:constraint_classification(hybrid_scaffolding_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_scaffolding_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_scaffolding_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_scaffolding_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_scaffolding_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): This reading models the constraint as genuinely hybrid — not pure coercion (which would be snare, ε ≥ 0.66) and not pure coordination (which would be rope, ε ≤ 0.35). The measurement trajectory (0.35 → 0.52 → 0.48) captures the empirical pattern: early impositions rely mostly on decree and enforcement, yielding moderate extractiveness. As scaffolding infrastructure develops (elite modeling becomes visible, messaging becomes pervasive, material incentives become structured), extractiveness initially rises because the state is now effectively reaching more of the population. But as adoption becomes demographic norm and younger cohorts naturalize the practice, perceived extractiveness declines—the new baseline makes the practice feel less imposed. Suppression (0.48): Moderate. The rural population faces real barriers—enforcement of old-practice abandonment, economic dependency on elite-controlled resources, social surveillance, cultural delegitimization—but suppression is not total. Organized resistance networks can exist (though constrained); some rural populations maintain syncretic practices (hybrid old+new); knowledge of pre-imposition practices persists in some regions. Suppression_ratio reflects that this is coercive but not totalitarian. Theater_ratio (0.56): Moderate-high, trending upward. In the early phase (t0), the apparatus is pragmatic: resources go to infrastructure and enforcement because the state needs actual behavioral change. As the practice becomes demographically mainstream (t50), the apparatus increasingly employs messaging and propaganda because actual enforcement is less necessary. By t100, the apparatus is substantially theatrical (piton trajectory): endless reiteration that the practice is 'natural' and 'evolved from the population' even though observers can still see the scaffolding infrastructure (schools teaching the language, media celebrating the dress, administrative rewards for compliance). This trajectory (rising theater_ratio) is the diagnostic signature of tangled_rope → piton degradation.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the powerless victim's snare experience (maximal extraction, no agency, no choice, no benefit) and the institutional beneficiary's rope experience (pure coordination, genuine problem-solving, mutual benefit, agency in design). These are the same constraint, but the lived experiences are incommensurable. The secondary adopter's tangled_rope experience is structurally the bridge: they get partial benefit (networks, opportunity) alongside partial extraction (cultural cost, identity labor). Their perspective makes visible what the other two perspectives hide: that the constraint involves both genuine coordination and genuine extraction, neither pure. The organized resistance's scaffold perspective reveals that the constraint is temporary—the sunset clauses are real. The piton perspective reveals that the apparatus degrades over time. The false summit perspective reveals the temptation to naturalize the outcome and forget its contingency. No single perspective is 'wrong'—they are all accurate descriptions of the constraint from their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural relationship to the extraction flow, their exit options, and their status as beneficiary or victim. Rural populations are victims with no exit (trapped) → maximum d → maximum f(d) → maximum χ they experience. Secondary adopters are victims with high-cost exit (constrained) → high d but not maximal → moderate-high χ. Elites are beneficiaries with complete exit (arbitrage) → minimum d → negative or near-zero f(d) → they experience benefit (negative χ means subsidy, not extraction). The analytical observer has no structural position relative to the extraction flow (analytical exit option) → moderate canonical d → moderate f(d). The directionality derivation for the piton perspective (institutional/arbitrage) shows that institutions maintaining the apparatus do not experience extraction—they are beneficiaries (low d) despite their role in the coercive mechanism. This reflects that beneficiaries are structurally defined by the extraction flow, not by moral judgment. The institutions benefit materially and institutionally from the apparatus; their d-value reflects this benefit, even though their role involves maintenance of suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by asserting that the hybrid scaffolding mechanism is the empirically robust pattern for successful state-directed cultural change—it is neither pure exogenous override (snare-like, and ultimately fragile) nor purely endogenous climb (too slow for state consolidation timelines), but the structured middle path where both coordination and extraction are real, where both genuine benefit and genuine cost operate simultaneously, and where the constraint can stabilize at 40-70% adoption without collapsing. The mandatrophy between 'is this coordination or extraction?' is resolved by answering 'yes, structurally both, with different agents experiencing different ratios.' The powerless experience pure extraction; the elites experience pure coordination; the moderate agents experience the hybrid equilibrium. The constraint is both a solution to a real coordination problem (state consolidation, cultural standardization for governance) and a mechanism of extraction (displacement of subaltern autonomy, concentration of legitimacy power, creation of insider/outsider status). Denying either dimension misses the structural dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffolding_effectiveness_threshold,
    'What proportion of the subject population must adopt the imposed practice for the scaffolding to be classified as ''partial success'' vs. ''failed regime'' vs. ''stable equilibrium''?',
    'Historical analysis of adoption rates by generation, region, and social class; longitudinal tracking of practice persistence across generations after state enforcement weakens; comparison with pure-decree regimes that failed to achieve minimum adoption.',
    'If threshold < 30% adoption: scaffolding as theoretical construct is descriptive only; empirically, the constraint functions more as snare than tangled_rope. If threshold > 70% adoption: scaffold becomes stable enough to be reclassified as rope or even mountain (cultural norm emerges). Current estimate (40-60%) places the constraint in the tangled_rope zone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_effectiveness_threshold, empirical, 'Adoption threshold for distinguishing partial success from regime failure').

omega_variable(
    ideological_messaging_mechanism,
    'Does ideological messaging genuinely persuade adopters, or does it function purely as cover story for material coercion and carrots?',
    'Analysis of contemporaneous personal narratives (diaries, letters, oral histories) from secondary adopters; investigation of whether adopted practices persist when material incentives decline; correlation between messaging intensity and adoption speed controlling for coercive mechanisms.',
    'If messaging genuinely persuades: the constraint is less extractive (quasi-endogenous pull is real, not theater). If messaging is pure cover: extractiveness should be reclassified upward (0.52 → 0.65+) and the constraint becomes closer to snare. Theater_ratio also sensitive to this: high genuine persuasion → lower theater; messaging as cover → higher theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_messaging_mechanism, empirical, 'Whether ideological messaging persuades or merely covers coercion').

omega_variable(
    urban_elite_extraction_surplus,
    'How much of the elite benefit from scaffolding comes from genuine coordination gains vs. from pure rent-seeking (appropriation of subaltern cultural resources, concentration of power, extraction of legitimacy)?',
    'Comparative analysis of elite economic data pre- and post-scaffolding; investigation of whether elites adopt the imposed practice themselves (genuine coordination) or maintain separate private practices (extraction disguised as coordination); study of whether coordination benefits accrue equally to all elites or concentrate in a subset.',
    'If coordination gains dominate: the rope perspective is accurate, and beneficiary extraction is partial. If rent-seeking dominates: the constraint is more snare-like even for elites, and overall extractiveness should increase. Current assessment (tangled_rope beneficiary perspective) assumes mixed motives — genuine coordination for state consolidation plus surplus extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urban_elite_extraction_surplus, empirical, 'Proportion of elite benefit from genuine coordination vs. pure rent-seeking').

omega_variable(
    competing_readings_kernel_ambiguity,
    'What constitutes a ''successful'' cultural imposition — endogenous adoption by subjects, or sufficient elite coordination regardless of lower-level resistance?',
    'Specification of success metrics by different historical schools (nationalist historiography vs. subaltern studies). This is an omega because different readings of the kernel legitimacy_of_imposed_practice define success differently: the exogenous_override_reading measures success by elite decree compliance; the endogenous_climb_reading measures success by genuinely organic cultural evolution; the hybrid_scaffolding_reading (this constraint) measures success by stable partial adoption achieved through infrastructure + messaging.',
    'This omega routes the committer ambiguity: the three sibling readings are not empirically refutable differences but definitional differences about what ''successful imposition'' means. Resolving this omega requires explicit normative judgment about which success metric is legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_readings_kernel_ambiguity, conceptual, 'Kernel ambiguity: what constitutes successful cultural imposition').

omega_variable(
    rural_cultural_suppression_persistence,
    'After state enforcement weakens or is removed, do suppressed rural practices spontaneously revive, or do they remain absent because new generations have lost the cultural knowledge?',
    'Post-colonial studies of practice revival; comparison of rural regions with continuous suppression vs. those with enforcement lapses; investigation of whether revival attempts succeed (knowledge recoverable) or fail (knowledge lost).',
    'If revival succeeds: suppression is structural/external (high suppression_ratio consistent with snare). If revival fails: the state has achieved genuine cultural displacement (suppression may be reclassified as partially internalized, merging some cognitive capture into the structural suppression). Current model assumes structural suppression; evidence of knowledge loss would increase the identity_locked component of the rural population''s exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_cultural_suppression_persistence, empirical, 'Whether suppressed practices revive or are permanently lost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_scaffolding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_early_pragmatism, hybrid_scaffolding_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(theater_t20_messaging_expansion, hybrid_scaffolding_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(theater_t50_propagandistic_maintenance, hybrid_scaffolding_reading, theater_ratio, 50, 0.61).
narrative_ontology:measurement(theater_t100_piton_trajectory, hybrid_scaffolding_reading, theater_ratio, 100, 0.72).

% Extraction over time
narrative_ontology:measurement(extract_t0_early_decree, hybrid_scaffolding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_t20_scaffolding_active, hybrid_scaffolding_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(extract_t50_partial_stabilization, hybrid_scaffolding_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(extract_t100_natural_integration, hybrid_scaffolding_reading, base_extractiveness, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:affects_constraint(hybrid_scaffolding_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(hybrid_scaffolding_reading, endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% legitimacy_of_imposed_practice is a contested kernel with three empirically distinct constraint readings. Each reading models a different structural mechanism for how imposed practices achieve displacement or stabilization. The three readings are siblings within the kernel family: exogenous_override_reading (pure decree, snare-like), endogenous_climb_reading (organic adoption, rope-like), and hybrid_scaffolding_reading (this story: tangled_rope with piton degradation). Each story has its own ε value, its own beneficiary/victim structure, and its own measurement trajectory reflecting its mechanism. They are linked through network.affects_constraints to enable comparative analysis. The kernel itself (legitimacy_of_imposed_practice) is under-determined by the empirical evidence — all three mechanisms can operate in different historical contexts, and the same case may exhibit elements of multiple mechanisms. The omega variables document this kernel-level ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
