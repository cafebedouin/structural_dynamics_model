% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_democratic_participation, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Speech Protection Hierarchy: Political Expression as Necessary for Democratic Self-Governance (Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint instantiates the democratic-participation reading of the
 *   speech-protection kernel — a specific, contestable interpretation of how
 *   free speech rights relate to democratic self-governance. The reading
 *   holds that speech protection is strongest and most robust for political
 *   expression necessary for democratic participation, while non-political
 *   speech may be more readily restricted. This reading is NOT the only
 *   defensible interpretation of speech rights (other readings emphasize
 *   absolutism, harm thresholds, marketplace mechanisms, or dignity-based
 *   constraints); it is one reading among a contested family. The structural
 *   dynamic is tangled: the constraint genuinely coordinates political
 *   communication (protecting candidates, civic associations, electoral
 *   discourse), while simultaneously creating an asymmetric protection
 *   hierarchy that leaves non-political speakers with weaker constitutional
 *   shields. The beneficiaries (political speakers) experience the constraint
 *   as enabling and coordinate-positive (Rope). The victims (non-political
 *   speakers, disparaged groups in commercial contexts) experience
 *   suppression (Snare). The judiciary experiences gatekeeping power (Tangled
 *   Rope). The piton perspective reveals that the marketplace-of-ideas
 *   justification — the original functional rationale — is substantially
 *   degraded in polarized media environments. The scaffold perspective
 *   identifies a sunset pathway: as alternative anti-corruption mechanisms
 *   mature (public financing, disclosure, reduced money-in-politics
 *   incentives), the need for expansive political speech protection to offset
 *   corruption decreases.
 *
 * KEY AGENTS:
 *   - Political Speakers / Electoral Candidates: Primary beneficiaries (organized/mobile) — receive heightened constitutional protection; can frame speech as 'political' to access protection tier
 *   - Non-Political Speakers / Artists / Commercial Speakers: Primary victims (powerless/trapped in category assignment) — receive weaker protection; cannot easily reframe as political; bear suppression costs disproportionately
 *   - Judiciary / Constitutional Courts: Institutional gatekeeper (institutional/constrained) — enforces the political/non-political distinction; gains power through adjudication; both coordinates (legitimate function) and extracts (institutional expansion)
 *   - Corporate/Commercial Entities: Secondary actors (powerful/mobile) — can strategically reframe commercial content as political expression to access heightened protection; navigate boundary ambiguity
 *   - Electoral Protection Coalition: Organized reformers (organized/constrained) — view political speech protection as temporary scaffold during corruption-fighting period; anticipate sunset as alternative mechanisms mature
 *   - Marketplace-of-Ideas Doctrine: Degraded justification (institutional/constrained) — original functional rationale (truth discovery through speech competition) operates theatrically rather than functionally in polarized/echo-chamber environments
 *   - Analytical Observer: Universal/civilizational view (analytical/analytical) — risks naturalizing a contested institutional reading as a law of democratic legitimacy itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.38).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.52).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Speech Protection Hierarchy: Political Expression as Necessary for Democratic Self-Governance (Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '557a43cc-f058-4e19-bd77-22b2a1248a33').
narrative_ontology:cs_kernel_codification('557a43cc-f058-4e19-bd77-22b2a1248a33', formalized).
narrative_ontology:cs_authority_grounding('557a43cc-f058-4e19-bd77-22b2a1248a33', lineage).
narrative_ontology:cs_interpretation_layer_present('557a43cc-f058-4e19-bd77-22b2a1248a33').
narrative_ontology:cs_reading_relation('557a43cc-f058-4e19-bd77-22b2a1248a33', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('557a43cc-f058-4e19-bd77-22b2a1248a33', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('557a43cc-f058-4e19-bd77-22b2a1248a33', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('557a43cc-f058-4e19-bd77-22b2a1248a33', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('557a43cc-f058-4e19-bd77-22b2a1248a33', foundational, democratic_participation_is_speech_primary_function).
narrative_ontology:cs_axiom_status(democratic_participation_is_speech_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('557a43cc-f058-4e19-bd77-22b2a1248a33', democratic_participation_is_speech_primary_function, deontological).
narrative_ontology:cs_axiom('557a43cc-f058-4e19-bd77-22b2a1248a33', foundational, political_speech_category_is_defensible).
narrative_ontology:cs_axiom_status(political_speech_category_is_defensible, holdable).
narrative_ontology:cs_axiom_grounding('557a43cc-f058-4e19-bd77-22b2a1248a33', political_speech_category_is_defensible, conventional).
narrative_ontology:cs_reference_frame('557a43cc-f058-4e19-bd77-22b2a1248a33', robust_democratic_participation).
narrative_ontology:cs_drift_state('557a43cc-f058-4e19-bd77-22b2a1248a33', contemporary_polarized_media_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('557a43cc-f058-4e19-bd77-22b2a1248a33', '2026-02-26T14:30:00Z').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, electoral_candidates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, civic_associations).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_political_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, disparaged_groups_in_non_political_contexts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-POLITICAL SPEAKER (SNARE) — A private individual, artist, or commercial speaker whose expression lacks direct political content cannot invoke the heightened protection the reading grants political speech. They are trapped: their speech receives weaker protection because it falls outside the reading's core function (democratic participation). No exit from the category assignment. Maximum experienced extraction — their suppression by state or private actors lacks the shield extended to political speech.
constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL SPEAKER / CIVIC ASSOCIATION (ROPE) — A candidate, political party, or advocacy organization speaking on matters of public concern experiences heightened First Amendment protection under this reading. The constraint functions primarily as coordination: it solves the collective action problem of ensuring candidates and civic associations can communicate without state suppression. These speakers can exit (remain silent, avoid politics) but mobile exit is costly politically — they benefit from the protection while bearing modest coordination costs.
constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY / COURTS (TANGLED ROPE) — Courts enforce the political/non-political distinction, deciding which speech qualifies for heightened protection. This constraint coordinates protection pathways (genuine function: ensuring political speakers can communicate) while creating asymmetric extraction: courts gain institutional power through the gatekeeping role (deciding what counts as 'necessary for democratic participation'), and non-political speakers are left with weaker protection. Constrained exit: courts could decline to enforce the hierarchy, but doing so violates the institutional mission.
constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CORPORATE/COMMERCIAL SPEAKER (TANGLED ROPE) — Large corporations and media entities can frame some commercial expression (e.g., advocacy advertising) as political speech to access heightened protection, but must navigate boundary uncertainty. Mobile exit is available (avoid politics, accept weaker protection) but the reading incentivizes strategically reframing commercial content as political. Mixed function: some genuine coordination (protecting corporate political participation), some extraction (ability to reframe commercial content as political to avoid regulation).
constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MARKETPLACE-OF-IDEAS JUSTIFICATION (PITON) — The original functional justification for robust political speech protection — that more speech counters false speech and truth emerges — is substantially performative. Political discourse often exhibits polarization, echo chambers, and asymmetric epistemology rather than convergence on truth. The doctrine persists through institutional inertia (constitutional doctrine inherited from an era with different media conditions) even as its functional basis (actual truth-discovery) has atrophied. Theater ratio reflects that the invocation of marketplace logic continues but the mechanism no longer operates.
constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL PROTECTION COALITION (SCAFFOLD) — From a generational perspective organized around fixing electoral corruption and dark money, the political speech protection hierarchy is a temporary scaffold: heightened protection for core electoral speech (candidate advocacy, issue campaigns directly affecting elections) serves a genuine coordination function during a transitional period when campaign finance mechanisms are being reconstructed. As alternative anti-corruption infrastructure matures (public financing, disclosure requirements, reduced money-in-politics pathways), the need for expansive political speech protection to offset corruption incentives diminishes. Sunset logic: the constraint becomes less necessary as its underlying problem (corruption incentive to suppress political speech) is solved by other means.
constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, this reading claims to instantiate a natural law: democratic self-governance logically requires robust protection for political expression; any system that suppresses political speech cannot be self-governing. The constraint appears immutable: it follows from the structure of democratic legitimacy itself. However, the structural data reveals this as a false summit. The reading is a specific institutional reading of a contested kernel — other readings (harm threshold, dignity-based, absolutist) offer competing definitions of how democratic self-governance relates to speech protection. The naturalization obscures that the political/non-political hierarchy is a constructed interpretive choice, not a law of democracy.
constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_protection_kernel__democratic_participation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, TR),
    TR >= 0.70.

:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading creates real asymmetry between political and non-political speakers' protection levels, but the extraction is constrained by the genuine coordination function (protecting electoral discourse). Political speakers benefit significantly; non-political speakers bear costs. The measurement trajectory (0.28 → 0.38 over 50 years) reflects that modern media and commercial speech have increasingly attempted to reframe as 'political' to access heightened protection, raising the effective extraction rate. Suppression (0.52): Moderate-high. Non-political speakers face meaningful barriers to equal protection; the constraint creates suppression mechanisms (differential liability rules, weaker protection frameworks). However, suppression is not absolute — alternative legal frameworks (other rights, equality doctrine, dignity protections) provide some countervailing shields. Theater ratio (0.48): Moderate. The marketplace-of-ideas rhetoric (truth through more speech) persists in judicial opinions even as its actual operation has degraded. Approximately half the constraint's cultural and rhetorical operation is performative (invoking truth-discovery logic that no longer functions), half is structural (actual differential protection mechanisms). Theater has increased over time (0.35 → 0.48) as the empirical gap between the marketplace theory and polarized-media reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   This reading shows a classic tangled-rope perspectival gap. Political speakers see the constraint as Rope (pure coordination — enabling necessary democratic discourse). Non-political speakers see Snare (extraction without coordination benefit for them). The judiciary sees Tangled Rope (coordinating political protection while extracting institutional gatekeeping power). Corporate speakers see Tangled Rope (mixed coordination and opportunity for strategic reframing). The electoral-protection coalition sees Scaffold (temporary coordination mechanism with sunset as alternatives mature). The marketplace doctrine sees Piton (original rationale degraded, doctrine persists through inertia). The analytical observer risks seeing Mountain (naturalizing the reading as a law of democracy itself). The perspectival richness reveals that the constraint is neither pure coordination nor pure extraction — it is an institutional allocation mechanism that genuinely serves one group (political speakers) while imposing costs on others (non-political speakers), justified by a functional rationale (truth discovery) that has partially atrophied.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the constraint. Political speakers (beneficiaries with mobile exit) experience low d, experiencing the constraint as enabling. Non-political speakers (victims with trapped exit) experience high d, experiencing maximum extraction. The judiciary (gatekeeper beneficiaries with constrained institutional exit) occupies the middle ground — they benefit from the constraint but cannot easily exit their enforcing role. The corporate entity (powerful beneficiary with mobile exit to remain non-political, but incentive to reframe as political for heightened protection) experiences ambiguous d — they could exit into non-political speech but mobile exit is strategically costly. The analytical observer (analytical power, analytical exit) occupies the position of maximum distance from the constraint's extraction flow, able to see its structure clearly but unable to participate in its coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing that tangled-rope classification is the correct terminal type — the constraint genuinely has both a coordination function (protecting political speech necessary for democratic participation) and an extraction function (creating a hierarchy that disadvantages non-political speakers). The mandatrophy is not 'is this coordination or extraction?' but rather 'what is the right allocation of protection across speech types in a legitimate democracy?' Different readings answer this differently: the absolutist reading says no allocation (nearly all speech equally protected); the harm-threshold reading says allocation by demonstrable harm; the dignity reading says allocation by non-subordination; the marketplace reading says allocation by role in truth-discovery; the democratic-participation reading (this one) says allocation by necessity for democratic self-governance. No single reading is 'correct' — they represent genuinely contestable decisions embedded in the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_speech_definition_instability,
    'What constitutes ''speech necessary for democratic participation''? Is the boundary between political and non-political speech stable or context-dependent?',
    'Doctrinal analysis of Supreme Court categorization patterns over time; examination of cases at the boundary (labor speech, cultural expression, commercial advocacy, foreign policy criticism); cross-jurisdictional comparison of how different democracies categorize speech types',
    'If boundary is unstable: the reading''s core claim collapses into incoherence (no stable hierarchy). If boundary is stable but narrow: many speakers currently receiving political protection would lose it. If boundary is stable and broad: the reading approaches the absolutist reading (nearly all speech becomes ''political'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_speech_definition_instability, conceptual, 'Stability and definition of political-speech category boundary').

omega_variable(
    non_political_speaker_actual_suppression_rate,
    'Do non-political speakers actually experience higher suppression rates or face meaningful exclusion from public discourse compared to the counterfactual of absolute speech protection?',
    'Empirical analysis of suppression events (censorship, defamation liability, platform removal) for political vs non-political speech 1980-2026; comparison of effective speech access across categories; survey evidence on perceived barriers by speaker type',
    'If suppression rates are identical: the reading''s extraction function is theater (no actual harm to non-political speakers). If suppression rates differ substantially: the reading genuinely functions to protect political speakers at non-political speakers'' expense (confirms tangled-rope asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_political_speaker_actual_suppression_rate, empirical, 'Whether non-political speakers face measurably higher suppression').

omega_variable(
    democratic_legitimacy_dependency,
    'Is democratic self-governance structurally dependent on privileging political speech protection, or do democracies function adequately with content-neutral speech protection?',
    'Comparative study of democracies with different speech protection hierarchies (e.g., Canada, Germany with hate-speech restrictions; Nordic consensus democracies with different boundaries); analysis of electoral quality, civic participation, and democratic legitimacy outcomes',
    'If legitimate democracies exist with content-neutral protection: the reading''s core axiom (that democracies require a political-speech hierarchy) is falsified. If all stable democracies employ some form of political-speech priority: the axiom is empirically supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_dependency, empirical, 'Whether democracies require political-speech protection hierarchy').

omega_variable(
    kernel_reading_contest_structural,
    'Does this reading foreclose, coexist with, or influence the sibling readings (absolutist, harm-threshold, marketplace, dignity)?',
    'Logical analysis of axiom compatibility; doctrinal examination of whether a single legal framework could hold multiple readings simultaneously; historical precedent analysis',
    'If this reading forecloses others: the contest is a zero-sum battle. If readings coexist: constitutional pluralism applies (multiple readings can be held by different parties). If influences: doctrinal development in one reading constrains others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structural, conceptual, 'Structural relationship between this reading and sibling readings').

omega_variable(
    extracted_surplus_by_institutional_actor,
    'Do courts and executives gain institutional power through gatekeeping the political/non-political distinction, and does this gatekeeping function constitute a form of extraction from the constraint?',
    'Analysis of institutional expansion (budget, personnel, discretionary authority) in courts and executive agencies enforcing speech distinctions; examination of cases where gatekeeping authority was exercised; doctrinal precedent showing growth of executive discretion',
    'If courts/executives extracted significant institutional power: the constraint is a mechanism for institutional growth masked as democratic protection. If gatekeeping is minimal: the constraint functions as nearly pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extracted_surplus_by_institutional_actor, empirical, 'Institutional power gains from gatekeeping the political-speech boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_dem_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(speech_dem_tr_t25, speech_protection_kernel__democratic_participation_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(speech_dem_tr_t50, speech_protection_kernel__democratic_participation_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(speech_dem_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(speech_dem_be_t25, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 25, 0.33).
narrative_ontology:measurement(speech_dem_be_t50, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, campaign_finance_reform_constraint).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, electoral_speech_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech-protection kernel. The other readings (absolutist, harm-threshold, marketplace, dignity) are distinct constraint stories with different ε values, different beneficiary/victim structures, and different perspectival gaps. Each reading is a complete constraint; they are linked via network.affects_constraints to model the kernel contest structure. The ε-invariance principle requires separate stories because the readings differ structurally: what counts as 'protected speech' is defined differently in each reading, affecting base extractiveness. The democratic-participation reading's ε=0.38 reflects the asymmetry created by the political/non-political hierarchy. Other readings produce different ε values reflecting their different structural definitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
