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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Taqlid Mechanism: Jurisprudential Authority and Lay Compliance in Sunni Islam
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   Taqlid (imitation or adherence) is the Islamic jurisprudential principle
 *   requiring lay Muslims to follow a qualified scholar of Islamic law
 *   (mufti) and prohibiting them from independent legal reasoning if they
 *   lack the knowledge to do so. Institutionally, this has manifested as
 *   adherence to one of four canonical Sunni schools (madhabs): Hanafi,
 *   Maliki, Shafi'i, and Hanbali. These schools constitute a contested
 *   kernel: they share a common commitment to jurisprudential methodology
 *   (reasoning from Qur'an, Sunna, scholarly consensus, and analogy) while
 *   disagreeing fundamentally on how these sources weight against each other,
 *   what counts as valid evidence, and how much priority to give tradition
 *   versus rational interpretation. The constraint operates at multiple
 *   levels simultaneously: as a coordination mechanism organizing vast bodies
 *   of legal knowledge into teachable frameworks; as an extraction mechanism
 *   that concentrates interpretive authority in credentialed scholars and
 *   their institutional successors; as an identity-constituting obligation
 *   that shapes how Muslims understand their religious and legal
 *   responsibilities; and as an increasingly theatrical performance of
 *   unified Islamic law in state contexts where political authorities appoint
 *   official muftis. The four madhabs have maintained mutual recognition for
 *   centuries without displacing one another, creating a structure of
 *   legitimate pluralism that is simultaneously unified (by common
 *   jurisprudential commitment) and fractured (by substantive disagreement on
 *   results). The constraint has experienced rising theater and stable
 *   suppression over the measurement interval, as mass literacy and global
 *   legal discourse make ijtihad (independent reasoning) technically more
 *   accessible while institutional enforcement of madhab loyalty persists.
 *
 * KEY AGENTS:
 *   - Lay Muslim Population: Primary victim (powerless/identity_locked) — obligated to follow a madhab scholar; exit requires identity reconstruction
 *   - Madhab Scholarly Establishment: Primary beneficiary (institutional/arbitrage) — maintains interpretive authority, controls fatwa production, ensures continuity
 *   - Regional Jurist: Secondary agent (moderate/constrained) — benefits from madhab framework but constrained by loyalty expectations; can issue ijtihad but at reputational cost
 *   - Islamic Legal Reform Movement: Organized actors (organized/mobile) — build alternative interpretive frameworks (maqasid-based jurisprudence, constitutional synthesis) that bypass madhab gatekeeping
 *   - State-Aligned Mufti Office: Institutional performer (institutional/constrained) — maintains theater of unified Islamic law; actual scholarly authority eroded by state instrumentalization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing madhab authority as law-like necessity when it is partially constructed through institutional incentive structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taqlid_mechanism, 0.35).
domain_priors:suppression_score(taqlid_mechanism, 0.48).
domain_priors:theater_ratio(taqlid_mechanism, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taqlid_mechanism, extractiveness, 0.35).
narrative_ontology:constraint_metric(taqlid_mechanism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(taqlid_mechanism, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taqlid_mechanism, tangled_rope).
narrative_ontology:human_readable(taqlid_mechanism, "Taqlid Mechanism: Jurisprudential Authority and Lay Compliance in Sunni Islam").
narrative_ontology:topic_domain(taqlid_mechanism, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(taqlid_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(taqlid_mechanism, 'beb7affd-1590-4472-8b13-25bffe635637').
narrative_ontology:cs_kernel_codification('beb7affd-1590-4472-8b13-25bffe635637', distributed).
narrative_ontology:cs_authority_grounding('beb7affd-1590-4472-8b13-25bffe635637', lineage).
narrative_ontology:cs_interpretation_layer_present('beb7affd-1590-4472-8b13-25bffe635637').
narrative_ontology:cs_reading_relation('beb7affd-1590-4472-8b13-25bffe635637', taqlid_mechanism__hanafi_jurisprudential_method, coexists_with).
narrative_ontology:cs_reading_relation('beb7affd-1590-4472-8b13-25bffe635637', taqlid_mechanism__maliki_jurisprudential_method, coexists_with).
narrative_ontology:cs_reading_relation('beb7affd-1590-4472-8b13-25bffe635637', taqlid_mechanism__shafii_jurisprudential_method, coexists_with).
narrative_ontology:cs_reading_relation('beb7affd-1590-4472-8b13-25bffe635637', taqlid_mechanism__hanbali_jurisprudential_method, coexists_with).
narrative_ontology:cs_axiom('beb7affd-1590-4472-8b13-25bffe635637', foundational, qualified_scholar_mediation_necessary).
narrative_ontology:cs_axiom_status(qualified_scholar_mediation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('beb7affd-1590-4472-8b13-25bffe635637', qualified_scholar_mediation_necessary, empirically_contingent).
narrative_ontology:cs_axiom('beb7affd-1590-4472-8b13-25bffe635637', foundational, madhab_institutional_continuity_preserves_truth).
narrative_ontology:cs_axiom_status(madhab_institutional_continuity_preserves_truth, holdable).
narrative_ontology:cs_axiom_grounding('beb7affd-1590-4472-8b13-25bffe635637', madhab_institutional_continuity_preserves_truth, conventional).
narrative_ontology:cs_axiom('beb7affd-1590-4472-8b13-25bffe635637', secondary, madhab_disagreement_compatible_with_validity).
narrative_ontology:cs_axiom_status(madhab_disagreement_compatible_with_validity, holdable).
narrative_ontology:cs_axiom_grounding('beb7affd-1590-4472-8b13-25bffe635637', madhab_disagreement_compatible_with_validity, deontological).
narrative_ontology:cs_reference_frame('beb7affd-1590-4472-8b13-25bffe635637', jurisprudential_authority_through_madhab_lineage).
narrative_ontology:cs_drift_state('beb7affd-1590-4472-8b13-25bffe635637', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('beb7affd-1590-4472-8b13-25bffe635637', '2026-02-26T14:32:18Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taqlid_mechanism, madhab_establishment).
narrative_ontology:constraint_beneficiary(taqlid_mechanism, scholarly_authority_structure).
narrative_ontology:constraint_victim(taqlid_mechanism, individual_reasoning_capacity).
narrative_ontology:constraint_victim(taqlid_mechanism, legal_pluralism_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY MUSLIM (SNARE) — Structurally bound by religious obligation ('whoever lacks knowledge must ask the knowledgeable'). Identity-locked: becoming a qualified mujtahid is not materially impossible but is identity-constituting work (decades of study, professional repositioning). Exit would require abandoning religious identity as it has been internalized. Maximum experienced extraction: the obligation to follow is non-negotiable and carries no exit except apostasy.
constraint_indexing:constraint_classification(taqlid_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL JURIST (TANGLED ROPE) — Benefits from the madhab structure (institutional standing, precedent database, interpretive framework). Also constrained: originality is theoretically permitted (ijtihad) but practically suppressed by expectation of madhab loyalty. Can issue novel opinions but they carry reputational cost within the school. Significant extraction but not total — has agency within bounds.
constraint_indexing:constraint_classification(taqlid_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MADHAB ESTABLISHMENT (ROPE) — Benefits from centralized authority: funding, institutional prestige, control over legal interpretation, role in state governance. But also coordinating function: organizes legal knowledge transmission, provides consistent interpretive framework, enables collective reasoning. Net beneficiary with genuine coordination function. Arbitrage option available: can shift schools (though costly) without losing professional standing.
constraint_indexing:constraint_classification(taqlid_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ISLAMIC LEGAL REFORM MOVEMENT (SCAFFOLD) — Organized agents (modern legal scholars, constitutional movements, transnational jurists) see taqlid as a temporary coordination mechanism appropriate to pre-modern knowledge transmission but increasingly obsolete with mass literacy, printed texts, and global legal discourse. Build alternative frameworks (maqasid-based jurisprudence, comparative legal synthesis) that bypass madhab authority. Mobile: can adopt new interpretive pathways without loss of Islamic legitimacy. Sunset logic: as mass legal literacy expands, taqlid's monopoly on interpretation erodes naturally.
constraint_indexing:constraint_classification(taqlid_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE-ALIGNED MUFTI OFFICE (PITON) — Institution that performs taqlid authority but increasingly divorced from the organic scholarly hierarchy it claims to represent. Issues fatwas that citizens follow out of state backing rather than epistemic authority. Theater high: the ritual of state-appointed muftis issuing rulings creates appearance of unified Islamic law where actual scholarly discourse is contested. Piton: the function (providing consistent legal guidance) is performed, but the method (scholarly ijtihad) is increasingly theatrical — actual rulings often reflect state interest more than jurisprudential reasoning.
constraint_indexing:constraint_classification(taqlid_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal epistemic perspective, taqlid appears as an inevitable feature of knowledge transmission: lay persons cannot master entire jurisprudential tradition and must defer to experts; expert networks require hierarchical organization; interpretation requires institutional continuity. Appears as law-like necessity, not contingent arrangement. However, the constraint declares beneficiaries (madhab establishment) which triggers false summit detection: the 'naturalness' of taqlid is partially constructed through institutional incentives.
constraint_indexing:constraint_classification(taqlid_mechanism, mountain,
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
    constraint_indexing:constraint_classification(taqlid_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(taqlid_mechanism, TR),
    TR >= 0.70.

:- end_tests(taqlid_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The madhab system does provide genuine coordination benefits — it organizes legal knowledge transmission, enables collective reasoning, provides consistency across time and space, and reduces the cognitive burden on lay Muslims of mastering entire jurisprudential traditions. However, this coordination function is bundled with extraction: scholars capture authority over interpretation, control fatwa production, restrict legitimate ijtihad, and maintain institutional prestige. The value reflects that coordination benefits are real but asymmetric (concentrated among scholars) while extraction costs are diffuse (lay Muslims bear cognitive closure, replication groups bear suppression of novel reasoning). Suppression (0.48): Moderate-high. Structural barriers to ijtihad exist at multiple levels: the knowledge requirement (madhab-era standard was very high, though modern literacy has lowered it); professional consequences (scholars who break from madhab face institutional sanctions); institutional gatekeeping (fatwa certification requires madhab approval); religious obligation itself (lay Muslims experience taqlid as a binding command, not a choice). However, suppression is not total: some ijtihad occurs, madhabs themselves reformed historical positions, and modern legal reforms have successfully created alternatives outside the madhab framework. Theater ratio (0.61): Moderately high and rising. The state-aligned mufti structure performs Islamic law authority while actual scholarly authority is distributed and contested. Official fatwas increasingly represent state interest rather than scholarly consensus. The rise from 0.35 to 0.61 over the interval reflects increasing instrumentalization of taqlid authority by state institutions, making the performance of unified Islamic law more theatrical while substantive scholarly debate continues underground.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are extreme. The lay Muslim sees obligation and identity threat (snare). The scholar sees coordination and prestige (rope). The jurist sees mixed benefits and constraints (tangled rope). The reformer sees obsolescence and transitional opportunity (scaffold). The state mufti sees ritualized authority (piton). The civilizational observer risks naturalizing all of this (mountain). These gaps reveal that 'taqlid' is not a unified constraint but a cluster of structurally distinct claims that different actors experience differently: a coordination mechanism for knowledge transmission bundled with authority extraction, wrapped in identity obligation for lay Muslims, increasingly performed by state institutions, and being unwound by reform movements that build alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Taqlid's effective extractiveness for each agent depends on their structural position: (1) Lay Muslims: powerless + identity-locked + obligated (victim) → maximum d → maximum chi. (2) Madhab scholars: institutional + arbitrage + beneficiary → low d → low/negative chi. (3) Regional jurists: moderate + constrained + mixed (partial victim) → moderate d → moderate chi. (4) Reformers: organized + mobile + challenging the system → low d → minimal chi (not trapped by taqlid). (5) State muftis: institutional + constrained + performing → moderate d → moderate chi masked by authority performance. The engine derives d automatically from beneficiary/victim declarations and exit options; no manual override is needed here.
 *
 * MANDATROPHY ANALYSIS:
 *   Taqlid's mandate was to ensure lay Muslims could follow Islamic law through qualified scholars when they themselves lacked training — addressing the genuine problem of knowledge transmission in pre-literate or low-literacy contexts. The mandate remains formally stated: lay Muslims still 'lack knowledge' and must 'ask the knowledgeable.' But the functional problem has shifted dramatically. Mass literacy, printed texts, global legal discourse, and accessible Islamic scholarship have substantially lowered the threshold for lay Muslims to understand jurisprudential reasoning. The mandate to restrict interpretation to madhab scholars increasingly appears to be protecting institutional authority rather than solving the knowledge problem. However, the mandate has NOT been formally abandoned — it remains theoretically operative even as its functional justification has eroded. This is classic mandatrophy territory: the constraint persists because institutional incentives (madhab prestige, scholarly authority, state gatekeeping) sustain it, even though the founding problem (lay ignorance requiring expert deference) is no longer the primary driver. Modern Islamic legal reform movements are resolving this mandatrophy by building alternatives (maqasid-based jurisprudence, constitutional synthesis, transnational Islamic law) that obsolete taqlid without formally replacing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijtihad_competence_threshold,
    'What constitutes legitimate ijtihad capacity in contemporary contexts? Has printing, mass literacy, and global legal discourse substantially lowered the barrier below the traditional madhab-era standard?',
    'Comparative analysis of ijtihadist output before and after mass literacy; tracking of legal reasoning quality across self-certified ijtihadists outside traditional madhab hierarchy; historical documentation of how madhab schools themselves lowered ijtihad thresholds during periods of institutional pressure.',
    'If threshold has substantially lowered: taqlid appears as constructed monopoly rather than natural epistemic hierarchy. If threshold remains high due to jurisprudential complexity: taqlid remains closer to coordination. If threshold is contested (different schools disagree): structure is inherently tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijtihad_competence_threshold, empirical, 'Whether ijtihad capacity threshold has changed with mass literacy').

omega_variable(
    natural_law_vs_constructed_authority,
    'Is taqlid a natural consequence of human epistemic limits (natural law), or a constructed institutional arrangement that maintains authority through suppression of alternatives?',
    'Historical analysis: periods when taqlid was weakened (Islamic Golden Age horizontal scholarship, modern legal reform movements); documentation of institutional enforcement mechanisms that suppress ijtihad (fatwa suppression, professional sanctions); comparison with other knowledge traditions'' handling of lay deference (medicine, science) to identify contingent vs universal aspects.',
    'If natural law: mountain classification confirmed. If constructed: false summit classification confirmed, reclassifying to tangled_rope or snare. If mixed: classification depends on temporal context (natural in pre-literate era, constructed in post-literate era).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, conceptual, 'Whether taqlid is natural epistemic necessity or constructed institutional arrangement').

omega_variable(
    madhab_mutual_recognition_stability,
    'What structural mechanisms sustain mutual recognition across the four madhabs? Are they grounded in shared jurisprudential principles or in power equilibrium (any madhab could exclude others but chooses not to)?',
    'Doctrinal analysis: identify explicit jurisprudential commitments that require mutual madhab recognition (e.g., ijma requirements); historical analysis of periods when mutual recognition was tested or broken; investigation of whether recognition breaks when institutional power balance shifts (e.g., state preference for one madhab).',
    'If grounded in shared principle: rope coordination structure. If grounded in power equilibrium: fragile tangled_rope with suppression/force requirements. If recognition mechanism is primarily performance: piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(madhab_mutual_recognition_stability, conceptual, 'Grounds for sustained mutual recognition across madhabs').

omega_variable(
    lay_identity_lock_vs_structural_trap,
    'For lay Muslims under taqlid obligation, is the binding mechanism primarily cognitive/identity-based (identity-locked) or material/structural (trapped)?',
    'Ethnographic/sociological analysis of exit narratives from lay Muslims who leave madhab authority; documentation of whether suppression persists after exit from taqlid structure (internalized obligation); comparison with other identity-locked constraints (religious conversion, professional identity dissolution).',
    'If identity-locked: lay agent''s barrier is cognitive/identity-constituting; exit is materially possible but psychologically structured as apostasy. If trapped: lay agent faces structural barriers (no alternative Islamic authority). If mixed: different lay populations experience different bindings (educated vs uneducated, urban vs traditional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_identity_lock_vs_structural_trap, empirical, 'Whether lay Muslims are identity-locked or structurally trapped').

omega_variable(
    kernel_reading_ambiguity,
    'Does the four-madhab system instantiate ONE kernel (jurisprudential method) read differently by each school, or FOUR separate kernels with superficial unity?',
    'Doctrinal reconstruction: identify the claimed unified kernel (e.g., ''interpretation of Qur''an and Sunna through scholarly reasoning''); map how each madhab reads this kernel differently (Hanafi: primacy of rational analogy; Maliki: inclusion of local practice; Shafi''i: precise methodology; Hanbali: literal traditionalism); assess whether the differences are interpretive (same kernel, different reading) or foundational (different kernels disguised as readings).',
    'If one kernel: constraint story is a kernel reading with sibling readings (cs_structure applies). If four kernels: constraint should decompose into separate stories per school, linked via network. If hybrid: one unified procedural kernel (the madhab method itself) with four substantive readings of legal principles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether madhab plurality reflects one kernel or four').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taqlid_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taqlid_tr_t0, taqlid_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(taqlid_tr_t5, taqlid_mechanism, theater_ratio, 5, 0.48).
narrative_ontology:measurement(taqlid_tr_t10, taqlid_mechanism, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(taqlid_be_t0, taqlid_mechanism, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(taqlid_be_t5, taqlid_mechanism, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(taqlid_be_t10, taqlid_mechanism, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(taqlid_su_t0, taqlid_mechanism, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(taqlid_su_t5, taqlid_mechanism, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(taqlid_su_t10, taqlid_mechanism, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taqlid_mechanism, identity_coordination).
narrative_ontology:affects_constraint(taqlid_mechanism, fatwa_authority_legitimacy).
narrative_ontology:affects_constraint(taqlid_mechanism, islamic_legal_modernization).
narrative_ontology:affects_constraint(taqlid_mechanism, madhab_institutional_capture).

% DUAL FORMULATION NOTE:
% Taqlid as a unified constraint is complicated by its status as a contested kernel. The four madhabs represent different readings of the same jurisprudential-method commitment. Separate constraint stories could be authored for each madhab's specific version of jurisprudential authority (e.g., Hanafi rational analogy emphasis vs. Maliki traditional practice emphasis), but this story treats taqlid as the unified coordination/extraction mechanism that all four schools instantiate, while noting in cs_structure that the kernel readings differ substantially.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
