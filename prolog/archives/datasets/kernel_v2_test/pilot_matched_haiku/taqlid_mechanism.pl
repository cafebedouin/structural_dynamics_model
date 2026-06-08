% ============================================================================
% CONSTRAINT STORY: taqlid_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taqlid_mechanism, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: taqlid_mechanism
 *   human_readable: Taqlid Mechanism: Institutionalized Following in Islamic Jurisprudence
 *   domain: islamic_law/jurisprudential_methodology/comparative_legal_theory
 *
 * SUMMARY:
 *   Taqlid (institutionalized following of established school authority) is a
 *   structural mechanism within Islamic jurisprudence that preserves madhab
 *   plurality while preventing interpretive chaos. The constraint operates at
 *   the intersection of epistemology, institutional authority, and legal
 *   practice. A jurist claiming mujtahid status (independent reasoning) must
 *   still operate within the epistemological framework of their madhab; a
 *   muqallid (follower) receives stable, transmissible jurisprudence but
 *   surrenders interpretive autonomy. The constraint exhibits genuine
 *   coordination function — it solves the problem of how to maintain legal
 *   predictability across generations and geographies without requiring
 *   centralized authority — while also concentrating interpretive power in
 *   established schools. The observable is the proportion of jurists claiming
 *   mujtahid vs muqallid status across centuries: periods of high mujtahid
 *   claims indicate weak taqlid enforcement or institutional instability;
 *   periods of low mujtahid claims indicate strong institutional gatekeeping.
 *   The constraint is downstream of four competing madhab readings (Hanafi,
 *   Maliki, Shafi'i, Hanbali), each grounding interpretive legitimacy in
 *   incompatible epistemological frameworks. Taqlid enables these four
 *   schools to coexist without requiring a meta-framework that reconciles
 *   their contradictions.
 *
 * KEY AGENTS:
 *   - Madhab Institutional Authority (organized/constrained): Primary beneficiary — taqlid provides institutional continuity and legitimacy; benefits from follower base and predictable jurisprudence transmission
 *   - Practicing Jurist / Muqallid (moderate/constrained): Secondary beneficiary — receives stable methodology, institutional support, and established precedents; constrained by madhab epistemology but not trapped
 *   - Mujtahid / Independent Reasoner (powerful/arbitrage): Mixed position — benefits from taqlid's coordination framework but experiences restriction on scope of independent reasoning; has exit options but faces institutional pressure
 *   - Lay Believer / Powerless Follower (powerless/trapped): Primary victim — instructed to follow madhab without voice in interpretation; bears cost of legal uncertainty resolution without participation; trapped by social structure and epistemic dependency
 *   - Islamic Legal Tradition (institutional/mobile): Institutional observer — experiences taqlid as both coordination (preserves plurality, prevents chaos) and extraction (restricts evolution, concentrates authority)
 *   - Analytical Observer (analytical/analytical): Civilizational perspective — sees taqlid as functional coordination mechanism solving genuine problem of legal stability across time and space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taqlid_mechanism, 0.35).
domain_priors:suppression_score(taqlid_mechanism, 0.42).
domain_priors:theater_ratio(taqlid_mechanism, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taqlid_mechanism, extractiveness, 0.35).
narrative_ontology:constraint_metric(taqlid_mechanism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(taqlid_mechanism, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taqlid_mechanism, rope).
narrative_ontology:human_readable(taqlid_mechanism, "Taqlid Mechanism: Institutionalized Following in Islamic Jurisprudence").
narrative_ontology:topic_domain(taqlid_mechanism, "islamic_law/jurisprudential_methodology/comparative_legal_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(taqlid_mechanism, 'dbe78691-3793-4a10-9dff-ff0568416aaf').
narrative_ontology:cs_kernel_codification('dbe78691-3793-4a10-9dff-ff0568416aaf', distributed).
narrative_ontology:cs_authority_grounding('dbe78691-3793-4a10-9dff-ff0568416aaf', lineage).
narrative_ontology:cs_interpretation_layer_present('dbe78691-3793-4a10-9dff-ff0568416aaf').
narrative_ontology:cs_reading_relation('dbe78691-3793-4a10-9dff-ff0568416aaf', taqlid_mechanism__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbe78691-3793-4a10-9dff-ff0568416aaf', taqlid_mechanism__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbe78691-3793-4a10-9dff-ff0568416aaf', taqlid_mechanism__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbe78691-3793-4a10-9dff-ff0568416aaf', taqlid_mechanism__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('dbe78691-3793-4a10-9dff-ff0568416aaf', foundational, madhab_plurality_legitimate).
narrative_ontology:cs_axiom_status(madhab_plurality_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('dbe78691-3793-4a10-9dff-ff0568416aaf', madhab_plurality_legitimate, conventional).
narrative_ontology:cs_axiom('dbe78691-3793-4a10-9dff-ff0568416aaf', foundational, institutional_authority_necessary).
narrative_ontology:cs_axiom_status(institutional_authority_necessary, holdable).
narrative_ontology:cs_axiom_grounding('dbe78691-3793-4a10-9dff-ff0568416aaf', institutional_authority_necessary, instrumental).
narrative_ontology:cs_axiom('dbe78691-3793-4a10-9dff-ff0568416aaf', secondary, mujtahid_status_gatekeepable).
narrative_ontology:cs_axiom_status(mujtahid_status_gatekeepable, holdable).
narrative_ontology:cs_axiom_grounding('dbe78691-3793-4a10-9dff-ff0568416aaf', mujtahid_status_gatekeepable, conventional).
narrative_ontology:cs_reference_frame('dbe78691-3793-4a10-9dff-ff0568416aaf', madhab_plurality_with_institutional_coordination).
narrative_ontology:cs_drift_state('dbe78691-3793-4a10-9dff-ff0568416aaf', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('dbe78691-3793-4a10-9dff-ff0568416aaf', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taqlid_mechanism, madhab_institutional_authority).
narrative_ontology:constraint_beneficiary(taqlid_mechanism, legal_predictability_collective).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(taqlid_mechanism, practicing_jurist_muqallid).
narrative_ontology:constraint_victim(taqlid_mechanism, mujtahid_independent_reasoner).
narrative_ontology:constraint_victim(taqlid_mechanism, lay_believer_powerless_follower).
narrative_ontology:constraint_vindicates(taqlid_mechanism, madhab_plurality_doctrine).
narrative_ontology:constraint_vindicates(taqlid_mechanism, interpretive_stability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each madhab (Hanafi, Maliki, Shafi'i, Hanbali) maintains institutional authority over interpretation within its framework. The madhab sets the agenda for what counts as valid reasoning, certifies mujtahid status, and enforces adherence to school methodology. The madhab benefits from taqlid through follower legitimacy and institutional continuity. Exit is constrained by institutional investment in scholarly lineages and established precedents, but a madhab can theoretically reform its methodology or merge with another school.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, madhab_institutional_authority, agenda_setter,
    institutional, generational, constrained, global).

% The practicing jurist who follows established school authority receives stable methodology, institutional support, and established precedents for issuing fatwas. The jurist benefits from taqlid through access to coherent jurisprudential framework and institutional backing. Exit is constrained by decades of training in madhab methodology and loss of institutional support if switching schools, but not impossible. The jurist experiences taqlid as enabling rather than constraining.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, practicing_jurist_muqallid, beneficiary,
    moderate, biographical, constrained, regional).

% The jurist claiming mujtahid status (independent reasoning) operates within madhab epistemological framework but claims authority to derive new rulings from foundational sources. The mujtahid benefits from taqlid's coordination framework (which provides legitimacy and institutional recognition) but experiences restriction on scope of independent reasoning. The mujtahid has arbitrage options (can found new school, appeal to alternative sources, claim universal mujtahid status) but faces institutional pressure to conform to madhab boundaries. The mujtahid both benefits from and pays into the constraint.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, mujtahid_independent_reasoner, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(taqlid_mechanism, mujtahid_independent_reasoner, payer).

% The ordinary believer seeking guidance is instructed to follow a madhab but has no voice in its interpretation and no meaningful exit option. Switching madhabs is socially costly and epistemically confusing. The believer bears the cost of legal uncertainty resolution (accepting whatever the madhab determines) without participating in determination. The believer is trapped by social structure and epistemic dependency — they cannot become a mujtahid (requires decades of specialized training) and cannot meaningfully exit the madhab system.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, lay_believer_powerless_follower, payer,
    powerless, biographical, trapped, local).

% The Islamic legal tradition as a whole maintains the taqlid mechanism to preserve madhab plurality and prevent interpretive chaos. The tradition benefits from taqlid through institutional stability and coherent jurisprudential framework. The tradition experiences restriction on evolution — new methodologies and reform movements face difficulty gaining legitimacy within established madhab boundaries. The tradition has mobile options (can adopt new schools, can reform methodology) but faces institutional inertia.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, islamic_legal_tradition, agenda_setter,
    institutional, civilizational, mobile, global).

% Legal predictability across time and space is a collective good that benefits from taqlid. The constraint enables stable, transmissible jurisprudence that can be applied consistently across generations and geographies. Legal predictability is not an agent but a vindicated proposition — the constraint's operation vindicates the principle that legal stability requires institutional coordination.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, legal_predictability_collective, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(taqlid_mechanism, legal_predictability_collective).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Taqlid solves the coordination problem of how to maintain legal predictability and institutional stability across generations and geographies without requiring centralized authority. The constraint enables the Islamic legal tradition to function as a coherent system despite the four madhabs' epistemologically incompatible frameworks. Without taqlid, each jurist would need to re-derive principles from foundational sources, creating interpretive chaos and legal unpredictability.
% TRANSFER_FUNCTION: Taqlid transfers interpretive authority from individual jurists to established madhab institutions. The constraint moves authority upward (from jurist to madhab) and backward (from contemporary reasoning to established precedent). In exchange, jurists receive stable methodology and institutional support. Lay believers transfer epistemic autonomy to madhab authorities in exchange for legal guidance.
% ABSENT_VOICES: Reform movements and modernist jurists who seek to transcend madhab boundaries are structurally excluded from the taqlid mechanism. These voices would object to the constraint's gatekeeping of mujtahid status and its restriction of independent reasoning. They are absent because taqlid's institutional structure does not provide them with legitimate standing — they are dismissed as lacking proper training or as threatening legal stability. The exclusion is structural, not accidental.
% DISAPPEARANCE_RATIONALE: If taqlid disappeared overnight, the Islamic legal tradition would experience significant rearrangement. The four madhabs would likely fragment into competing schools and sub-schools. Legal predictability would decrease. However, alternative coordination mechanisms might emerge (consensus-based authority, hierarchical governance, market competition between jurists). Some argue the tradition would rearrange toward greater flexibility and responsiveness; others argue it would collapse into chaos. The verdict is contested because the counterfactual depends on what alternative mechanisms would emerge.
% FOUNDING_PROBLEM: Taqlid emerged in response to the problem of how to maintain jurisprudential coherence and legal predictability as the Islamic tradition expanded geographically and temporally beyond the Prophet's lifetime and the early community. The founding problem was: how can jurists in different regions and centuries apply Islamic law consistently without direct access to the Prophet's guidance or the early community's consensus?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live and is attested by contemporary Islamic legal scholars across all madhabs. Even modernist reformers who critique taqlid acknowledge that the problem of legal predictability and institutional stability is real. The problem is corroborated by the fact that contemporary Islamic jurisprudence continues to invoke madhab authority and taqlid principles, suggesting that the founding problem has not been resolved by alternative mechanisms.
narrative_ontology:disappearance_verdict(taqlid_mechanism, contested).
narrative_ontology:founding_problem_status(taqlid_mechanism, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MADHAB INSTITUTION (ROPE) — Taqlid solves the genuine coordination problem of legal predictability across generations and geographies. A madhab without taqlid would fragment into competing interpretations. The institution benefits from the constraint (followers provide legitimacy and continuity) but also genuinely coordinates: followers receive stable, transmissible jurisprudence. Exit is constrained by institutional investment but not impossible — a madhab can reform its methodology. Net beneficiary experiencing the constraint as coordination.
constraint_indexing:constraint_classification(taqlid_mechanism, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: PRACTICING JURIST / MUQALLID (ROPE) — The jurist who follows established school authority experiences taqlid as enabling rather than constraining. Taqlid provides a coherent methodology, established precedents, and institutional support for issuing fatwas. The jurist could theoretically become a mujtahid (independent reasoner) but faces high costs: decades of study, loss of institutional backing, risk of isolation. The constraint is coordination — it solves the problem of how to apply law consistently without requiring every jurist to re-derive principles from first sources.
constraint_indexing:constraint_classification(taqlid_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MUJTAHID / INDEPENDENT REASONER (TANGLED ROPE) — The jurist claiming mujtahid status experiences taqlid as both coordination and extraction. Taqlid creates a stable framework within which mujtahids can operate and be recognized (coordination function). But taqlid also restricts the scope of independent reasoning — a mujtahid must still work within the madhab's epistemological framework or risk rejection. The mujtahid has arbitrage options (can found a new school, can appeal to alternative sources) but faces institutional pressure to conform. Mixed experience: genuine coordination benefit alongside asymmetric restriction.
constraint_indexing:constraint_classification(taqlid_mechanism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LAY BELIEVER / POWERLESS FOLLOWER (SNARE) — The ordinary believer seeking guidance experiences taqlid as constraint without meaningful benefit. They are instructed to follow a madhab but have no voice in its interpretation, no ability to question its authority, and no exit option (switching madhabs is socially costly and epistemically confusing). The believer bears the cost of legal uncertainty resolution (accepting whatever the madhab determines) without participating in the determination. Trapped by social structure and epistemic dependency.
constraint_indexing:constraint_classification(taqlid_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: ISLAMIC LEGAL TRADITION / INSTITUTIONAL OBSERVER (TANGLED ROPE) — The tradition as a whole experiences taqlid as both coordination and extraction. Taqlid preserves madhab plurality and prevents interpretive chaos (genuine coordination). But taqlid also concentrates interpretive authority in established schools, making it difficult for new methodologies or reform movements to gain legitimacy. The tradition has mobile options (can adopt new schools, can reform methodology) but faces institutional inertia. The constraint enables the tradition's survival while restricting its evolution.
constraint_indexing:constraint_classification(taqlid_mechanism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPARATIVE LEGAL THEORY (ROPE) — From a civilizational perspective, taqlid is a solution to a genuine coordination problem: how to maintain legal stability and predictability across time and space while preserving interpretive plurality. The constraint enables the Islamic legal tradition to function as a coherent system without requiring centralized authority. Extraction is minimal — the mechanism serves the tradition's own stated goals. The analytical observer sees taqlid as a functional coordination mechanism, not as a false summit or hidden extraction.
constraint_indexing:constraint_classification(taqlid_mechanism, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taqlid_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taqlid_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taqlid_mechanism, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(taqlid_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Taqlid extracts interpretive authority from individual jurists and concentrates it in established schools, but this extraction serves a genuine coordination function. The constraint is not primarily extractive — it is primarily coordinative. The extractiveness value reflects that some jurists experience restriction (mujtahids constrained by madhab epistemology) and some believers experience powerlessness (lay followers with no voice), but the overall system benefits most participants. The trajectory shows slight increase over the interval (0.25 → 0.35) reflecting historical institutionalization of madhab boundaries and increasing gatekeeping of mujtahid status. Suppression (0.42): Moderate. Taqlid requires enforcement through institutional mechanisms (madhab authority, scholarly consensus, social pressure) but suppression is not total. Jurists can and do claim mujtahid status; believers can and do switch madhabs; new methodologies can and do emerge (though with difficulty). The suppression is real but permeable. Theater ratio (0.38): Moderate-low. Taqlid involves some performative elements (ritual invocation of madhab authority, theatrical displays of scholarly credentials) but the core function is substantive — it genuinely coordinates legal interpretation. The theater is lower than in constraints where function has atrophied; taqlid's theater ratio reflects that the mechanism still serves its stated purpose.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is substantial and reveals the constraint's hybrid nature. The madhab institution and practicing jurist see Rope — genuine coordination enabling stable jurisprudence. The mujtahid sees Tangled Rope — coordination benefit alongside restriction on independent reasoning. The lay believer sees Snare — powerlessness and epistemic dependency without meaningful benefit. The institutional observer sees Tangled Rope — coordination enabling tradition's survival while restricting evolution. The analytical observer sees Rope — functional coordination mechanism. The gap between Rope and Snare perspectives is the diagnostic signal: the constraint genuinely coordinates for institutional and professional actors but genuinely constrains for powerless believers. This is not a false summit (the coordination function is real) but a genuine asymmetry in how the constraint distributes its benefits and costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the constraint. Madhab institutions and practicing jurists are beneficiaries with constrained exit — they experience low d (0.2-0.3 range), producing low effective extraction. Mujtahids are powerful actors with arbitrage options — they experience moderate d (0.4-0.5 range), producing moderate effective extraction despite their power. Lay believers are powerless with trapped exit — they experience high d (0.8-0.9 range), producing high effective extraction despite the constraint's overall low extractiveness. The engine's directionality derivation captures this asymmetry: the same constraint produces different experienced extraction for different agents based on their power and exit options. Beneficiary/victim declarations feed the derivation: madhab institutions and legal predictability are beneficiaries; no agent is declared as victim (the constraint is not primarily extractive), but the lay believer perspective reveals victimhood through the trapped exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   Taqlid does not exhibit mandatrophy in the classical sense — its mandate (preserve madhab plurality while preventing interpretive chaos) remains live and functional. However, the constraint shows signs of mandate drift: the original function was to enable jurisprudential development within madhab frameworks; the contemporary function increasingly emphasizes gatekeeping and restriction of mujtahid claims. The theater ratio increase (0.32 → 0.38) suggests that performative elements (ritual invocation of madhab authority) are becoming more prominent relative to substantive coordination. This is not yet mandatrophy (the function has not atrophied) but it is drift toward piton classification. The constraint remains Rope because the coordination function is still primary, but the trajectory suggests monitoring for future mandatrophy development.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mujtahid_status_contestation,
    'What criteria determine mujtahid status, and who has authority to certify it? Is the boundary between mujtahid and muqallid a natural epistemic distinction or a socially constructed institutional gate?',
    'Historical analysis of mujtahid certification across madhabs and centuries; examination of whether criteria are applied consistently or serve institutional gatekeeping; comparison of self-identified vs institutionally-recognized mujtahids',
    'If boundary is natural/epistemic: taqlid is coordination of genuine expertise hierarchy (Rope confirmed). If boundary is institutional/constructed: taqlid is extraction mechanism disguised as expertise recognition (Snare from mujtahid perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mujtahid_status_contestation, conceptual, 'Whether mujtahid status is epistemic or institutional').

omega_variable(
    madhab_switching_cost_structure,
    'What are the actual costs (social, epistemic, institutional) of switching madhabs or adopting independent reasoning? Are these costs structural (inherent to the system) or contingent (historical/cultural)?',
    'Ethnographic study of madhab switching; historical cases of jurists adopting new methodologies; analysis of institutional barriers vs cultural norms; comparison across regions and time periods',
    'If costs are structural: suppression is inherent to taqlid (Snare from lay believer perspective confirmed). If costs are contingent: suppression could be reduced through institutional reform (Rope classification more robust).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(madhab_switching_cost_structure, empirical, 'Cost structure of madhab switching and independent reasoning').

omega_variable(
    interpretive_chaos_counterfactual,
    'Would Islamic jurisprudence without taqlid actually experience interpretive chaos, or would alternative coordination mechanisms (consensus, hierarchical authority, market competition) emerge?',
    'Historical analysis of periods/regions with weak taqlid enforcement; comparison with legal systems lacking centralized authority; theoretical modeling of coordination mechanisms in decentralized interpretation',
    'If chaos is inevitable: taqlid is necessary coordination (Rope confirmed). If alternatives are viable: taqlid is one choice among many (extraction component becomes visible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_chaos_counterfactual, conceptual, 'Whether interpretive chaos is inevitable without taqlid').

omega_variable(
    madhab_epistemological_incommensurability,
    'Are the four madhabs'' epistemological frameworks (Hanafi rationalism, Maliki customary practice, Shafi''i hadith hierarchy, Hanbali literalism) genuinely incommensurable, or can they be reconciled within a unified framework?',
    'Comparative analysis of madhab methodologies; examination of whether modern jurisprudence has developed meta-frameworks that encompass all four; study of how contemporary jurists navigate between madhabs',
    'If incommensurable: madhab plurality is irreducible, taqlid is necessary to prevent conflict (Rope confirmed). If reconcilable: madhab boundaries are contingent, taqlid enforces artificial separation (extraction component visible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(madhab_epistemological_incommensurability, conceptual, 'Whether madhab epistemologies are incommensurable').

omega_variable(
    taqlid_natural_law_vs_constructed,
    'Is taqlid a natural law of how legal systems must function (inevitable coordination mechanism), or a constructed institutional arrangement that benefits specific actors?',
    'Comparative analysis with non-Islamic legal systems; historical analysis of how taqlid emerged and was institutionalized; examination of whether beneficiary groups actively promoted taqlid doctrine',
    'If natural law: classification as Mountain is appropriate (false summit detection may trigger). If constructed: Rope classification is accurate and extraction component is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_natural_law_vs_constructed, conceptual, 'Whether taqlid is natural law or constructed arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taqlid_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taqlid_theater_t0, taqlid_mechanism, theater_ratio, 0, 0.32).
narrative_ontology:measurement(taqlid_theater_t3, taqlid_mechanism, theater_ratio, 3, 0.35).
narrative_ontology:measurement(taqlid_theater_t6, taqlid_mechanism, theater_ratio, 6, 0.38).
narrative_ontology:measurement(taqlid_theater_t10, taqlid_mechanism, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(taqlid_extractiveness_t0, taqlid_mechanism, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(taqlid_extractiveness_t3, taqlid_mechanism, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(taqlid_extractiveness_t6, taqlid_mechanism, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(taqlid_extractiveness_t10, taqlid_mechanism, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(taqlid_suppression_t0, taqlid_mechanism, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(taqlid_suppression_t3, taqlid_mechanism, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(taqlid_suppression_t6, taqlid_mechanism, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(taqlid_suppression_t10, taqlid_mechanism, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taqlid_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(taqlid_mechanism, hanafi_reading).
narrative_ontology:affects_constraint(taqlid_mechanism, maliki_reading).
narrative_ontology:affects_constraint(taqlid_mechanism, shafii_reading).
narrative_ontology:affects_constraint(taqlid_mechanism, hanbali_reading).

% DUAL FORMULATION NOTE:
% Taqlid mechanism is the institutional arrangement that enables madhab plurality. The four madhab readings (hanafi_reading, maliki_reading, shafii_reading, hanbali_reading) are separate constraint stories with different epistemological frameworks and different beneficiary/victim structures. Each madhab reading has its own claimed_type (tangled_rope) reflecting that each school both coordinates jurisprudential interpretation and extracts authority from individual jurists. Taqlid mechanism is upstream of all four readings — it is the institutional structure that makes their coexistence possible. The ε-invariance principle applies: taqlid mechanism has one stable ε (0.35) reflecting its coordination function; each madhab reading has its own ε reflecting its epistemological framework and institutional gatekeeping. The constraint family is: taqlid_mechanism (Rope) → {hanafi_reading, maliki_reading, shafii_reading, hanbali_reading} (all Tangled Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
