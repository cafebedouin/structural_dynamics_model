% ============================================================================
% CONSTRAINT STORY: maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maliki_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: maliki_reading
 *   human_readable: Maliki Jurisprudential Method: 'Amal and Maslaha Primacy
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The Maliki jurisprudential method privileges Medina customary practice
 *   ('amal) and public interest (maslaha mursala) as authoritative sources
 *   alongside Qur'an and Hadith. This reading of Islamic jurisprudential
 *   methodology creates a structured constraint that benefits Maliki jurists
 *   and customary law practitioners while imposing costs on hadith purist
 *   schools and non-Medina regional traditions. The constraint exhibits
 *   tangled-rope characteristics: it coordinates genuine legal pluralism
 *   (regional customs are recognized as authoritative sources) while
 *   simultaneously extracting from those whose methodologies are
 *   subordinated. The Medina exemplarity doctrine, once a functional
 *   principle for legal reasoning, has become increasingly theatrical —
 *   invoked for legitimacy while actual jurisprudential work relies on
 *   maslaha reasoning and contemporary public interest considerations. The
 *   constraint operates across the Islamic world but is most institutionally
 *   embedded in North Africa, West Africa, and parts of the Middle East where
 *   Maliki jurisprudence remains the dominant school.
 *
 * KEY AGENTS:
 *   - Maliki Jurists: Primary beneficiary (institutional/arbitrage) — gain methodological flexibility and institutional authority from 'amal and maslaha primacy
 *   - Medina Customary Practitioners: Primary beneficiary (moderate/constrained) — their practices are validated as authoritative legal sources within the Maliki framework
 *   - Hadith Purist Scholars: Primary victim (powerless/trapped) — their textual arguments are systematically subordinated to customary practice and public interest reasoning
 *   - Non-Medina Regional Traditions: Secondary victim (moderate/constrained) — their customary practices are recognized but hierarchically subordinated to Medina exemplarity
 *   - Public Interest Advocacy Movement: Secondary beneficiary (organized/constrained) — contemporary reformers use maslaha reasoning as tool for progressive jurisprudential adaptation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as immutable feature of jurisprudence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maliki_reading, 0.35).
domain_priors:suppression_score(maliki_reading, 0.42).
domain_priors:theater_ratio(maliki_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maliki_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(maliki_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(maliki_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maliki_reading, tangled_rope).
narrative_ontology:human_readable(maliki_reading, "Maliki Jurisprudential Method: 'Amal and Maslaha Primacy").
narrative_ontology:topic_domain(maliki_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maliki_reading, 'daf8c9de-3227-4975-b379-46b0bff7b3c4').
narrative_ontology:cs_kernel_codification('daf8c9de-3227-4975-b379-46b0bff7b3c4', formalized).
narrative_ontology:cs_authority_grounding('daf8c9de-3227-4975-b379-46b0bff7b3c4', lineage).
narrative_ontology:cs_interpretation_layer_present('daf8c9de-3227-4975-b379-46b0bff7b3c4').
narrative_ontology:cs_reading_relation('daf8c9de-3227-4975-b379-46b0bff7b3c4', maliki_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('daf8c9de-3227-4975-b379-46b0bff7b3c4', maliki_reading__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('daf8c9de-3227-4975-b379-46b0bff7b3c4', maliki_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('daf8c9de-3227-4975-b379-46b0bff7b3c4', foundational, medina_practice_authoritative_source).
narrative_ontology:cs_axiom_status(medina_practice_authoritative_source, holdable).
narrative_ontology:cs_axiom_grounding('daf8c9de-3227-4975-b379-46b0bff7b3c4', medina_practice_authoritative_source, conventional).
narrative_ontology:cs_axiom('daf8c9de-3227-4975-b379-46b0bff7b3c4', foundational, maslaha_mursala_override_capacity).
narrative_ontology:cs_axiom_status(maslaha_mursala_override_capacity, holdable).
narrative_ontology:cs_axiom_grounding('daf8c9de-3227-4975-b379-46b0bff7b3c4', maslaha_mursala_override_capacity, deontological).
narrative_ontology:cs_reference_frame('daf8c9de-3227-4975-b379-46b0bff7b3c4', medina_exemplary_community_model).
narrative_ontology:cs_drift_state('daf8c9de-3227-4975-b379-46b0bff7b3c4', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('daf8c9de-3227-4975-b379-46b0bff7b3c4', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maliki_reading, maliki_jurists).
narrative_ontology:constraint_beneficiary(maliki_reading, medina_customary_practitioners).
narrative_ontology:constraint_beneficiary(maliki_reading, public_interest_advocates).
narrative_ontology:constraint_victim(maliki_reading, hadith_purist_schools).
narrative_ontology:constraint_victim(maliki_reading, literal_text_interpreters).
narrative_ontology:constraint_victim(maliki_reading, non_medina_regional_practices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maliki_reading, non_medina_regional_traditions).
narrative_ontology:constraint_victim(maliki_reading, hadith_purist_scholars).
narrative_ontology:constraint_victim(maliki_reading, non_medina_regional_traditions).
narrative_ontology:constraint_vindicates(maliki_reading, medina_as_exemplary_community).
narrative_ontology:constraint_vindicates(maliki_reading, customary_practice_as_legal_source).
narrative_ontology:constraint_vindicates(maliki_reading, public_interest_override_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maliki jurists set the methodological agenda within the Maliki school, determining how 'amal and maslaha are applied to contemporary legal questions. They benefit from the flexibility of the method, which allows adaptation to local conditions while maintaining doctrinal coherence. They can exit to other schools or methodologies, but doing so would abandon their institutional position and scholarly authority within the Maliki tradition.
narrative_ontology:constraint_stakeholder(maliki_reading, maliki_jurists, agenda_setter,
    institutional, generational, arbitrage, regional).

% Customary law practitioners in Medina and regions following Maliki jurisprudence benefit from the method's validation of their practices as authoritative legal sources. Their customs are recognized and applied in legal reasoning. However, they are constrained by the hierarchical ordering of Medina exemplarity — their practices are valued primarily insofar as they align with or can be traced to Medina precedent.
narrative_ontology:constraint_stakeholder(maliki_reading, medina_customary_practitioners, beneficiary,
    moderate, biographical, constrained, regional).

% Hadith purist scholars within the Maliki tradition face systematic subordination of their textual arguments to customary practice and public interest reasoning. Their methodology — privileging hadith authenticity and textual analysis — is treated as secondary to 'amal and maslaha. They are trapped within the Maliki framework because exiting would require abandoning their scholarly identity and institutional affiliation. They bear the cost of methodological marginalization.
narrative_ontology:constraint_stakeholder(maliki_reading, hadith_purist_scholars, payer,
    powerless, biographical, trapped, regional).

% Non-Medina regional customary practices (from Kufa, Basra, Syria, Egypt) are recognized as legal sources within Maliki jurisprudence but are hierarchically subordinated to Medina exemplarity. These traditions benefit from recognition as authoritative sources but bear the cost of being ordered below Medina precedent. They are constrained by the need to justify their practices through reference to Medina exemplarity or to demonstrate compatibility with Maliki doctrine.
narrative_ontology:constraint_stakeholder(maliki_reading, non_medina_regional_traditions, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maliki_reading, non_medina_regional_traditions, beneficiary).

% Contemporary Islamic reformers and human rights advocates benefit from maslaha mursala reasoning as a tool for progressive jurisprudential adaptation. The method enables them to argue for legal reforms in the name of public interest and social benefit. However, they are constrained by the need to maintain coherence with Maliki doctrine and to avoid appearing to abandon textual authority entirely. Their advocacy depends on the continued institutional authority of the Maliki school.
narrative_ontology:constraint_stakeholder(maliki_reading, public_interest_advocates, beneficiary,
    organized, generational, constrained, global).

% The doctrine that Medina's practices during the Prophet's lifetime represent the exemplary model for Islamic law is a non-agent entity — a proposition rather than an actor. It is invoked as a legitimating narrative in Maliki jurisprudence but functions increasingly as theater rather than as a source of substantive legal guidance. The doctrine does not collect rents or bear costs; it is a vindicated proposition that appears in the constraint's operation.
narrative_ontology:constraint_stakeholder(maliki_reading, medina_exemplarity_doctrine, observer,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(maliki_reading, medina_exemplarity_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Maliki method solves the genuine coordination problem of reconciling textual sources (Qur'an and Hadith) with evolving social practice and regional variation. Islamic law must adapt to changing circumstances while maintaining doctrinal coherence; the method coordinates this adaptation by recognizing customary practice and public interest as authoritative sources alongside textual authority.
% TRANSFER_FUNCTION: The constraint transfers methodological authority from hadith purists to Maliki jurists and customary practitioners. Textual arguments are subordinated to 'amal and maslaha reasoning; institutional authority flows toward those who can invoke customary practice and public interest. The transfer also moves legitimacy from non-Medina regional traditions toward Medina exemplarity, even as it recognizes regional customs as authoritative.
% ABSENT_VOICES: Hadith purist scholars who reject the subordination of textual authority to customary practice and public interest are present but marginalized within the Maliki tradition. Non-Medina regional traditions that resist hierarchical subordination to Medina exemplarity are recognized but constrained. Scholars from other schools (Hanafi, Shafi'i, Hanbali) who dispute the Maliki methodological balance are absent from Maliki jurisprudential deliberation, though they maintain parallel institutional structures.
% DISAPPEARANCE_RATIONALE: If the Maliki methodological constraint disappeared, Islamic jurisprudence would rearrange itself significantly. The balance of textual authority, customary practice, and public interest would shift — hadith purist approaches might gain institutional authority, or alternative methodologies might emerge. However, some argue that the constraint reflects natural features of any legal system (the need to balance text and practice), suggesting the world would rearrange only superficially. The parties dispute whether the constraint is contingent institutional arrangement or natural jurisprudential law.
% FOUNDING_PROBLEM: The founding problem was the need to reconcile the textual sources of Islamic law (Qur'an and Hadith) with the diverse customary practices and social conditions of the expanding Islamic empire. Early Maliki jurisprudence developed the method of 'amal (Medina customary practice) and maslaha (public interest) to enable legal adaptation while maintaining doctrinal coherence and textual authority.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Islamic legal scholars and historians attest that the founding problem (reconciling textual sources with social practice) remains live. However, they dispute whether the Maliki solution (privileging Medina exemplarity and maslaha) remains the most effective approach. Reformers argue the problem is still urgent and the method is still functional; traditionalists argue the problem has been solved and the method is now theatrical. No consensus exists on the founding problem's current status.
narrative_ontology:disappearance_verdict(maliki_reading, contested).
narrative_ontology:founding_problem_status(maliki_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HADITH PURIST SCHOLAR (SNARE) — Trapped within a jurisprudential framework that privileges customary practice and public interest over textual hadith authority. Cannot exit the Maliki method without abandoning scholarly legitimacy within the Maliki tradition. Bears the cost of methodological subordination: their textual arguments are overridden by 'amal precedent and maslaha reasoning. Maximum extraction from this position — no alternative authority structure available within the tradition.
constraint_indexing:constraint_classification(maliki_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL CUSTOMARY PRACTICE COMMUNITY (TANGLED ROPE) — Constrained by the need to maintain coherence with Maliki doctrine while also benefiting from the method's validation of local custom. The constraint coordinates genuine legal pluralism (regional practices are recognized as authoritative) while extracting from non-Medina regions whose customs are subordinated to Medina exemplarity. Moderate extraction — some communities benefit, others bear costs; active enforcement required to maintain the hierarchy.
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MALIKI JUDICIAL INSTITUTION (ROPE) — Benefits from the flexibility of 'amal and maslaha reasoning, which enables adaptation to local conditions while maintaining doctrinal coherence. Experiences the constraint as coordination: the method solves the genuine problem of reconciling textual sources with evolving social practice. Net beneficiary — the institutional framework gains legitimacy and operational flexibility from this methodological approach.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PUBLIC INTEREST ADVOCACY MOVEMENT (TANGLED ROPE) — Organized agents (contemporary Islamic reformers, human rights advocates, development practitioners) benefit from maslaha reasoning as a tool for progressive jurisprudential reform. The constraint coordinates genuine social adaptation while extracting from traditionalist scholars who see maslaha as a cover for abandoning textual authority. Moderate extraction with clear beneficiary structure — the movement gains methodological legitimacy; traditionalists bear the cost of marginalization.
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL MEDINA EXEMPLARITY DOCTRINE (PITON) — The foundational claim that Medina's practices during the Prophet's lifetime and the Rightly Guided Caliphate represent the exemplary model for Islamic law has atrophied as a functional principle. Contemporary Maliki jurisprudence invokes Medina exemplarity theatrically — as a legitimating narrative — while actual reasoning relies on maslaha and contemporary public interest. The doctrine persists through institutional inertia and textual authority, not because it generates coherent legal guidance for modern conditions. Theater ratio reflects the gap between the invoked principle and actual methodological practice.
constraint_indexing:constraint_classification(maliki_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Maliki method appears as an immutable feature of Islamic jurisprudential logic: any legal system must balance textual authority with customary practice and public interest. The constraint appears as a natural law of jurisprudence itself — an irreducible structural feature of how law adapts to social change. However, the structural data reveals beneficiaries (Maliki jurists, customary practitioners) and victims (hadith purists, non-Medina regions), indicating this is a false summit: the 'natural' balance is actually a contingent institutional arrangement that benefits specific actors.
constraint_indexing:constraint_classification(maliki_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maliki_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maliki_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maliki_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(maliki_reading, TR),
    TR >= 0.70.

:- end_tests(maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Maliki method benefits specific actors (Maliki jurists, customary practitioners) while imposing costs on others (hadith purists, non-Medina traditions). However, the extraction is not severe because the method does coordinate genuine legal pluralism — customary practices ARE recognized as authoritative, not merely suppressed. The benefit to Maliki jurists is real (methodological flexibility, institutional authority) but not monopolistic (other schools coexist). Suppression (0.42): Moderate. Hadith purist arguments are subordinated but not eliminated — they remain part of the jurisprudential conversation, just with lower priority. Non-Medina regional practices are recognized but hierarchically ordered below Medina exemplarity. Theater ratio (0.38): Moderate. The Medina exemplarity doctrine is increasingly theatrical — contemporary Maliki jurisprudence invokes it for legitimacy while actual reasoning relies on maslaha and public interest. However, the theater is not dominant (ratio < 0.5) because 'amal and maslaha reasoning do generate substantive legal guidance, not merely performative justification.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Maliki institution's rope classification and the hadith purist's snare classification reveals the constraint's asymmetric structure. The institution benefits from methodological flexibility; the purist bears the cost of subordination. The gap between the analytical observer's mountain and the actual beneficiary/victim structure reveals the false-summit mechanism: what appears as natural jurisprudential law is actually a contingent institutional arrangement that benefits specific actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki jurists (institutional/arbitrage): beneficiary status + arbitrage exit → low d → low χ. Hadith purists (powerless/trapped): victim status + trapped exit → high d → high χ. Customary practitioners (moderate/constrained): beneficiary status + constrained exit → moderate d → moderate χ. Public interest advocates (organized/constrained): beneficiary status + constrained exit → moderate d → moderate χ. The engine computes these values from the structural declarations; the commentary reflects the reasoning behind the beneficiary/victim assignments.
 *
 * MANDATROPHY ANALYSIS:
 *   The Maliki reading resolves mandatrophy by clarifying that the jurisprudential method's mandate (to balance textual authority with customary practice and public interest) remains live and functional. However, the Medina exemplarity doctrine's mandate (to use Medina's historical practices as the exemplary model) has outlived its function — contemporary Maliki jurisprudence rarely derives substantive guidance from Medina exemplarity alone, instead using it as a legitimating narrative while actual reasoning relies on maslaha and public interest. The constraint is tangled_rope rather than piton because the core mandate (methodological balance) is still functional; the theatrical element (Medina exemplarity) is secondary. The false-summit risk arises when the analytical observer naturalizes the methodological balance as immutable law rather than recognizing it as a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medina_exemplarity_historical_accuracy,
    'Does the historical record of Medina practice during the Prophet''s lifetime and Rightly Guided Caliphate actually support the Maliki characterization, or is this a constructed narrative that privileges certain sources over others?',
    'Comparative hadith analysis across schools; examination of which reports are cited vs. excluded in Maliki ''amal reconstruction; historical analysis of Medina''s actual legal pluralism and regional variation',
    'If historical: Medina exemplarity is a legitimate empirical claim grounding the method. If constructed: the method naturalizes a particular reading of history as universal principle, strengthening the false-summit diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medina_exemplarity_historical_accuracy, empirical, 'Whether Medina exemplarity claim is historically accurate or constructed narrative').

omega_variable(
    maslaha_criteria_determinacy,
    'Does maslaha mursala (unrestricted public interest) have determinate criteria for application, or does it function as a discretionary override that different jurists apply inconsistently?',
    'Comparative analysis of maslaha invocations across Maliki jurisprudence; identification of consistent criteria vs. ad hoc applications; examination of whether maslaha reasoning produces predictable outcomes or depends on individual jurist judgment',
    'If determinate: maslaha is a genuine methodological tool (coordination function strengthened). If discretionary: maslaha functions as cover for extraction (snare classification strengthened for victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maslaha_criteria_determinacy, empirical, 'Whether maslaha mursala has determinate application criteria').

omega_variable(
    amal_regional_variation_suppression,
    'To what extent does the Maliki privileging of Medina ''amal suppress recognition of equally valid customary practices in other Islamic regions?',
    'Historical analysis of non-Medina regional legal traditions; examination of how Maliki jurisprudence treats Kufa, Basra, Syria, and Egypt customary practices; documentation of which regional practices are incorporated vs. marginalized',
    'If suppression is substantial: the constraint extracts from non-Medina regions (snare classification for those communities). If suppression is minimal: regional pluralism is genuine (rope classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_regional_variation_suppression, empirical, 'Degree of suppression of non-Medina regional customary practices').

omega_variable(
    kernel_reading_contest_live,
    'Is the contest between Maliki and other schools (Hanafi, Shafi''i, Hanbali) a live jurisprudential dispute with real institutional stakes, or a historical artifact maintained for doctrinal completeness?',
    'Examination of contemporary Islamic legal institutions (courts, fatwa councils, academic jurisprudence); documentation of which schools are actively applied vs. historically referenced; analysis of whether schools compete for institutional authority or coexist as parallel traditions',
    'If live: the reading contest is a genuine structural feature (coexists_with relation confirmed). If artifact: the schools are theatrical alternatives maintained for legitimacy (piton-level dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_live, empirical, 'Whether Maliki-other school contest is live or historical artifact').

omega_variable(
    natural_law_vs_constructed_method,
    'Is the Maliki balance of textual authority, customary practice, and public interest a natural feature of any legal system, or a specific institutional construction that benefits Maliki jurists and customary practitioners?',
    'Comparative jurisprudence across Islamic schools and non-Islamic legal traditions; analysis of whether the balance is inevitable or contingent; examination of alternative methodological arrangements and their structural consequences',
    'If natural: mountain classification is correct (immutable feature of jurisprudence). If constructed: false-summit diagnosis confirmed (contingent institutional arrangement naturalized as law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_method, conceptual, 'Whether Maliki methodological balance is natural or constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maliki_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(malik_theater_t0, maliki_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(malik_theater_t3, maliki_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(malik_theater_t6, maliki_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(malik_theater_t10, maliki_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(malik_extract_t0, maliki_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(malik_extract_t3, maliki_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(malik_extract_t6, maliki_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(malik_extract_t10, maliki_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(malik_supp_t0, maliki_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(malik_supp_t3, maliki_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(malik_supp_t6, maliki_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(malik_supp_t10, maliki_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maliki_reading, 0.12).
narrative_ontology:affects_constraint(maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(maliki_reading, hanbali_reading).
narrative_ontology:affects_constraint(maliki_reading, maslaha_mursala_doctrine).
narrative_ontology:affects_constraint(maliki_reading, amal_medina_exemplarity).

% DUAL FORMULATION NOTE:
% The Maliki reading is part of a constraint family decomposing the contested kernel usul_al_fiqh_method. Each school reading (Maliki, Hanafi, Shafi'i, Hanbali) has distinct ε values reflecting different methodological priorities and institutional beneficiaries. The Maliki reading's moderate extractiveness (0.35) reflects the genuine coordination function of balancing textual authority with customary practice, distinguishing it from more extractive readings that privilege specific schools' institutional interests. The family is linked through network.affects_constraints; each reading influences the others by creating structural pressure on alternative methodologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maliki_reading, powerless, 0.85).
constraint_indexing:directionality_override(maliki_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
