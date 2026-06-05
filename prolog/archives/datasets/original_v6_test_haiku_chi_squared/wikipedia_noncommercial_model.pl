% ============================================================================
% CONSTRAINT STORY: wikipedia_noncommercial_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wikipedia_noncommercial_model, []).

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
 *   constraint_id: wikipedia_noncommercial_model
 *   human_readable: Wikipedia's Non-Commercial, Volunteer-Driven Model
 *   domain: technological/information_commons
 *
 * SUMMARY:
 *   Wikipedia's non-commercial, volunteer-driven model represents a hybrid
 *   coordination-extraction constraint that resolves a critical collective
 *   action problem — producing a comprehensive, freely accessible global
 *   knowledge commons — while simultaneously extracting unpaid epistemic
 *   labor from a globally distributed volunteer workforce. The model has
 *   persisted for 24+ years through social commitment to the ideal of
 *   'knowledge for all,' but exhibits structural tensions between
 *   coordination function (enabling knowledge access) and extraction
 *   mechanism (uncompensated labor, gatekeeping asymmetry, marginalization of
 *   non-dominant knowledge traditions). The constraint exhibits multiple
 *   classifications across different observer positions: pure coordination
 *   (Rope) from the beneficiary perspective of global knowledge users, mixed
 *   hybrid (Tangled Rope) from the moderate volunteer editor perspective,
 *   pure extraction (Snare) from the powerless uncompensated editor
 *   perspective, institutional benefit (Rope) from the Wikimedia Foundation
 *   perspective, competitive displacement (Tangled Rope) from academic
 *   publishing gatekeepers, degraded institutional theater (Piton) from the
 *   governance bureaucracy, transitional scaffolding (Scaffold) from the
 *   emerging decentralized knowledge network perspective, and false natural
 *   law (Mountain — rejected) from the civilizational analytical observer.
 *   The theater ratio (0.55) reflects substantial performative content in
 *   Wikipedia's governance: policy discussions, arbitration committee
 *   hearings, and deletion review boards create the appearance of democratic
 *   governance while maintaining decision-making authority concentrated in
 *   elite administrator networks. The constraint has intensified over the
 *   2001-2015 interval as editorial complexity increased, volunteer burnout
 *   became visible, and the epistemically extractive nature of the model
 *   became more apparent.
 *
 * KEY AGENTS:
 *   - Uncompensated Volunteer Editors: Primary victim (powerless/trapped) — contribute unpaid labor without formal governance voice; no mechanism to exit without reputational loss
 *   - Marginalized Knowledge Communities: Structural victim (powerless/trapped) — excluded from editing gatekeeping through language barriers and knowledge representation asymmetry; Wikipedia extracts from dominant communities while offering no mechanism for excluded groups to shape coverage
 *   - Mid-Career Subject Matter Experts: Secondary victim (moderate/constrained) — derive knowledge-access benefits but face career risk and opportunity cost when contributing to Wikipedia instead of institutional work
 *   - Global Knowledge Users: Primary beneficiary (powerful/arbitrage) — receive free, comprehensive knowledge commons; experience zero extraction; greatest beneficiary from the model
 *   - Wikimedia Foundation & Donor Base: Institutional beneficiary (institutional/arbitrage) — operates $170M+ budget on volunteer infrastructure; donors receive reputation benefits; net beneficiary
 *   - Wikipedia Editorial Bureaucracy: Institutional actor (organized/constrained) — administers performative governance; maintains legitimacy appearance while concentrating decision authority
 *   - Academic Publishing & Knowledge Gatekeepers: Organized competitor (organized/constrained) — face competitive displacement from free encyclopedia; extracted from in terms of reduced demand for institutional expertise
 *   - Emerging Decentralized Networks: Potential successor (organized/mobile) — represent alternative knowledge production mechanisms with token incentives or blockchain verification; see Wikipedia as temporary scaffold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_noncommercial_model, 0.35).
domain_priors:suppression_score(wikipedia_noncommercial_model, 0.42).
domain_priors:theater_ratio(wikipedia_noncommercial_model, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, extractiveness, 0.35).
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wikipedia_noncommercial_model, tangled_rope).
narrative_ontology:human_readable(wikipedia_noncommercial_model, "Wikipedia's Non-Commercial, Volunteer-Driven Model").
narrative_ontology:topic_domain(wikipedia_noncommercial_model, "technological/information_commons").

domain_priors:requires_active_enforcement(wikipedia_noncommercial_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wikipedia_noncommercial_model, global_knowledge_users).
narrative_ontology:constraint_beneficiary(wikipedia_noncommercial_model, wikimedia_foundation).
narrative_ontology:constraint_beneficiary(wikipedia_noncommercial_model, volunteer_editors).
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, underrepresented_communities).
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, volunteer_labor_asymmetry).
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, editorial_governance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCOMPENSATED VOLUNTEER EDITOR (SNARE) — Trapped by social obligation and expertise reputation; contributes unpaid labor to a global commons while having no formal voice in governance or decision-making. No exit mechanism available without losing investment in community standing. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.58.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED KNOWLEDGE COMMUNITIES (SNARE) — Structurally excluded from editing gatekeeping (language barriers, lack of internet access, cultural misrepresentation in content standards). Wikipedia's model extracts epistemic labor from dominant communities while offering no mechanism for excluded groups to shape coverage of their own knowledge. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.67.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-CAREER SUBJECT MATTER EXPERTS (TANGLED ROPE) — Derive coordination benefits (free encyclopedia, quality curation, global reach) but face institutional extraction: time spent on Wikipedia editing is uncompensated, unrewardable in academic career metrics, yet necessary for knowledge democratization. Constrained by professional expectations and career risk if they neglect institutional duties. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: WIKIMEDIA FOUNDATION & DONOR BASE (ROPE) — Benefits from volunteer-powered model: operates a $170M+ annual budget with minimal operational overhead. Donors experience positive reputation from supporting 'knowledge for all.' Foundation sees this as pure coordination: solving collective action problem of knowledge access. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Negative extraction = net beneficiary.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL KNOWLEDGE USERS (ROPE) — Primary beneficiaries: free, comprehensive, generally reliable knowledge commons. Experience zero friction to access. No coercion felt; the model appears as pure coordination enabling access. d≈0.02, f(d)≈-0.18, σ=1.2 → χ≈-0.06. Negative extraction = pure coordination from this view.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SYSTEMIC KNOWLEDGE GATEKEEPERS (TANGLED ROPE) — Academic publishing and institutional knowledge access face coordinated competition from Wikipedia (positive: knowledge democratization; extractive: Wikipedia reduces demand for institutional expertise and scholarly publishing revenue). Constrained by need to maintain legitimacy while competing with free alternative. d≈0.55, f(d)≈0.72, σ=1.0 → χ≈0.25. Mixed perception: coordination threat + competitive displacement.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: WIKIPEDIA EDITORIAL BUREAUCRACY (PITON) — Administrative systems (arbitration committees, deletion review boards, policy enforcement) are substantially performative. Theater serves two functions: (1) legitimacy signal ('we have governance'), (2) bottleneck that appears democratic while maintaining elite editor control. theater_ratio=0.55 reflects significant performative content (policy discussions, voting) masking de facto rule-setting. Degraded piton: the bureaucracy persists through inertia but has low functional verification of stated governance ideals.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: EMERGING DECENTRALIZED KNOWLEDGE NETWORKS (SCAFFOLD) — From the vantage point of blockchain-based, token-incentivized, or AI-augmented knowledge platforms, Wikipedia's non-commercial model appears as a temporary institutional form with a sunset clause. As alternative knowledge production mechanisms (DAO-managed wikis, AI-assisted curation, blockchain verification) mature, the volunteer-dependent model becomes optional rather than necessary. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.15. Low effective extraction because alternative exit pathways exist for knowledge producers.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, global knowledge commons inherently require volunteer coordination as a universal feature of human knowledge accumulation across all literate societies. The non-commercial model appears as a natural law of commons governance: you cannot commodify epistemic truth without destroying the commons. However, structural data (ε=0.35, suppression=0.42, theater=0.55) contradicts mountain classification. This is a false summit: the 'universal' framing naturalizes what is actually a contingent institutional choice specific to Wikimedia's founding principles.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_noncommercial_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wikipedia_noncommercial_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_noncommercial_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(wikipedia_noncommercial_model, TR),
    TR >= 0.70.

:- end_tests(wikipedia_noncommercial_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts unpaid labor from volunteers (~100,000 active editors globally) and concentrates epistemic authority in elite administrator networks. However, extraction is not severe (ε ≤ 0.45 rope threshold) because the social contract is transparent, the knowledge product is genuinely free, and most volunteer editors derive meaning and status benefits from participation. The extraction is real but justified by coordination benefits. The 2001-2015 trajectory shows increasing extractiveness as editorial requirements became more specialized and volunteer burnout became visible. Suppression (0.42): Moderate. Barriers to participation include language requirements, gatekeeping policies, technical complexity, and cultural misrepresentation. Non-English speakers and communities without institutional internet access face high barriers. However, suppression is not total — Wikipedia remains the most accessible knowledge commons; alternative paid encyclopedias have higher suppression. Theater ratio (0.55): Moderate-high. Editorial governance (arbitration committees, deletion reviews, policy discussions) creates an appearance of democratic decision-making while actual authority is concentrated in administrator networks. The theater serves legitimacy signaling and protects the volunteer model from external criticism. As the system matured, theater increased: more formal policies, more process-heavy dispute resolution, more performative consensus-building. Claimed type: Tangled Rope. The model provides genuine coordination function (solving knowledge access) AND exhibits asymmetric extraction (uncompensated labor, gatekeeping asymmetry). Both conditions are satisfied; active enforcement (governance structures) is present.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full spectrum of classifications depending on structural position. Global knowledge users and the Wikimedia Foundation see near-pure coordination (Rope, negative χ); they benefit maximally and face no coercion. Uncompensated volunteer editors and marginalized communities see near-pure extraction (Snare, high χ); they bear costs with no formal voice. Mid-career experts and academic publishers see mixed experience (Tangled Rope, moderate χ); they derive benefits (free knowledge access, institutional legitimacy) but also face displacement and opportunity costs. The analytical observer risks naturalizing the model as a universal law of commons (Mountain) — but this fails the structural test: ε=0.35 and suppression=0.42 contradict mountain thresholds. The perspectival gap reveals that the 'knowledge for all' framing masks structural inequality in labor distribution and epistemic authority. A volunteer in sub-Saharan Africa with unreliable internet access experiences the constraint as pure extraction (snare) because they are excluded from participation. A tenured academic in a well-resourced university experiences it as coordination (rope) because they benefit from free knowledge access with minimal suppression. The system is the same; their structural positions are incommensurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Global knowledge users: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.18. Maximum net beneficiary; no extraction felt. Wikimedia Foundation: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Strong beneficiary; volunteers provide unpaid infrastructure. Uncompensated editors: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; contribute unpaid labor with no exit mechanism and no formal voice in governance. Marginalized communities: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; structurally excluded from participation mechanisms and knowledge representation. Mid-career experts: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction; face opportunity cost and career risk but also benefit from knowledge access. Academic publishers: Victim + constrained → d≈0.55, f(d)≈0.72. Moderate extraction from competitive displacement; constrained by need to maintain legitimacy against free alternative. Emerging networks: Mobile → d≈0.35, f(d)≈0.35. Low extraction because alternative exit pathways exist. Editorial bureaucracy: Institutional + arbitrage → d≈0.05, f(d)≈-0.12; piton classification comes from theater gate, not directionality. Analytical observer: Analytical → d≈0.72, f(d)≈1.15; mountain classification is false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is subtle: is Wikipedia's model a 'pure coordination breakthrough' (Rope) that legitimately mobilizes volunteer labor, or is it 'extractive capitalism with volunteer labor' (Snare with philanthropic veneer)? The tangled_rope classification resolves this by recognizing that BOTH are true simultaneously — the model provides genuine coordination benefits (free global knowledge access) AND exhibits genuine extraction (uncompensated labor, gatekeeping asymmetry, marginalization of excluded communities). The resolution mechanism: declare beneficiaries (global knowledge users, Wikimedia Foundation) and victims (uncompensated editors, marginalized communities) explicitly. The perspectival gap (rope from beneficiary view, snare from victim view) is not a classification error — it reflects real structural inequality. The theater ratio (0.55) further confirms tangled_rope: if the model were pure coordination, governance would be genuinely democratic, not performative. If pure extraction, the model would collapse from volunteer departure. The hybrid nature is structural, not observational. Mandatrophy is fully resolved: this is a legitimate tangled_rope, not a classification error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    volunteer_sustainability_threshold,
    'What minimum volunteer participation rate is structurally necessary to maintain Wikipedia''s knowledge production quality and coverage?',
    'Longitudinal analysis of editor cohort retention, contribution patterns, and quality metrics; correlation between volunteer supply shocks and content decay; comparison to commercial alternatives',
    'If threshold is low (<10% current participation): non-commercial model is resilient, snare/tangled_rope classifications are stable. If threshold is high (>50% current participation): system is fragile, vulnerable to volunteer burnout cascade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volunteer_sustainability_threshold, empirical, 'Minimum volunteer participation required for quality maintenance').

omega_variable(
    gatekeeping_extraction_or_quality_control,
    'Does Wikipedia''s editorial gatekeeping (administrator authority, deletion review boards) constitute quality-control coordination or structural extraction masquerading as governance?',
    'Comparative analysis: error rates in Wikipedia (protected vs unprotected articles), reversion patterns, admin decision consistency; correlation between access barriers and editorial participation demographics',
    'If quality-control: snare classification from volunteer perspective is overestimated; tangled_rope better captures mixed benefit. If extraction masquerading as governance: piton classification is confirmed; theater_ratio properly reflects performative bureaucracy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeping_extraction_or_quality_control, conceptual, 'Whether editorial gatekeeping is quality control or extraction').

omega_variable(
    knowledge_asymmetry_by_language_region,
    'Does the volunteer model systematically concentrate epistemic power in high-English-proficiency regions while extracting from or marginalizing non-English knowledge communities?',
    'Content coverage analysis by language; editor demographic distribution; representation of non-Western knowledge traditions; barriers to participation by non-English speakers; funding distribution to language editions',
    'If asymmetric extraction confirmed: snare classification from marginalized communities'' perspective is stable; broader scope (global) justifies high χ. If roughly balanced: classification shifts toward rope/tangled_rope; suppression metric should be lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_asymmetry_by_language_region, empirical, 'Whether volunteer model concentrates power in high-English regions').

omega_variable(
    ai_augmentation_sunset_mechanism,
    'Will AI-assisted editing tools and bot-powered content curation make human volunteer labor partially redundant, triggering the scaffold''s sunset clause?',
    'Monitoring of bot edit volume, AI tool adoption rates, quality metrics, and volunteer retention trends; correlation with rollout of Wikimedia AI initiatives; timeline to human-AI editorial parity',
    'If sunset mechanism activates: scaffold perspective becomes predictive; volunteer extraction constraint weakens. If AI remains auxiliary: scaffold is aspirational; human extraction persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_augmentation_sunset_mechanism, empirical, 'Whether AI tools will trigger sunset of volunteer model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wikipedia_noncommercial_model, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wiki_tr_t0, wikipedia_noncommercial_model, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wiki_tr_t7, wikipedia_noncommercial_model, theater_ratio, 7, 0.48).
narrative_ontology:measurement(wiki_tr_t14, wikipedia_noncommercial_model, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(wiki_be_t0, wikipedia_noncommercial_model, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(wiki_be_t7, wikipedia_noncommercial_model, base_extractiveness, 7, 0.29).
narrative_ontology:measurement(wiki_be_t14, wikipedia_noncommercial_model, base_extractiveness, 14, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wikipedia_noncommercial_model, information_standard).
narrative_ontology:affects_constraint(wikipedia_noncommercial_model, knowledge_commons_fragmentation).
narrative_ontology:affects_constraint(wikipedia_noncommercial_model, volunteer_burnout_cascade).
narrative_ontology:affects_constraint(wikipedia_noncommercial_model, academic_publishing_displacement).

% DUAL FORMULATION NOTE:
% Wikipedia's non-commercial model can be decomposed into three structurally distinct constraints: (1) volunteer labor extraction (ε≈0.35, snare from volunteer perspective), (2) knowledge access coordination (ε≈0.15, rope from user perspective), (3) epistemic gatekeeping (ε≈0.30, snare from marginalized communities perspective). These are linked: volunteer extraction enables knowledge access, gatekeeping enables quality control. The unified story treats the model as a single tangled_rope; decomposition would separately analyze labor dynamics, access mechanisms, and epistemic authority distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wikipedia_noncommercial_model, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
