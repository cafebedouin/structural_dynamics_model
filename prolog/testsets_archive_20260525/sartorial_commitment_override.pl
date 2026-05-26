% ============================================================================
% CONSTRAINT STORY: sartorial_commitment_override
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sartorial_commitment_override, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sartorial_commitment_override
 *   human_readable: Sartorial Commitment Override in Meiji State Formation
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Meiji Restoration (1868) imposed Western dress codes and Gregorian
 *   calendar adoption as instruments of state modernization and international
 *   legitimacy. This constraint exemplifies a top-down kernel override: the
 *   Meiji state apparatus intentionally displaced Tokugawa-era sartorial
 *   commitments (traditional garments as markers of status, region, and
 *   cultural continuity) by legal prohibition, economic privilege for Western
 *   textile imports, and bureaucratic standardization. The case tests whether
 *   the kernel/reading framework can accommodate exogenous override (top-down
 *   state imposition of a new kernel) as distinct from endogenous
 *   displacement (climbing from fringe to mainstream). The extractiveness
 *   trajectory reveals lifecycle patterns: maximum extraction in early Meiji
 *   (1868-1880) when enforcement was strict and alternatives were suppressed;
 *   declining extraction after 1895 as international recognition stabilized
 *   and the state's need for visible Western dress compliance diminished;
 *   pitonization by 1910 as bureaucratic dress codes persisted through
 *   institutional inertia rather than strategic extraction. Theater ratio
 *   rises as the constraint's functional purpose declines: early theater
 *   reflects genuine administrative need (hierarchy legibility); late theater
 *   reflects performative perpetuation.
 *
 * KEY AGENTS:
 *   - Meiji State Apparatus: Primary beneficiary (institutional/arbitrage) — captures international diplomatic recognition and administrative legibility through dress standardization
 *   - Commoner Class: Primary victim (powerless/trapped) — bears identity lock and legal suppression; identity fused with traditional garments; no structural exit option
 *   - Regional Textile Producers: Secondary beneficiary (moderate/constrained) — face displacement costs but gain access to national market and modernization technologies (status ambiguous; see omega 4)
 *   - Ceremony Practitioners: Secondary victim (organized/constrained) — suppressed traditional ritual dress; circumvent through preservation movement by late Meiji
 *   - International Diplomatic Community: Structural enabler (institutional/arbitrage) — the state's claim that Western dress is a 'civilization' marker required for treaty recognition (necessity claim questioned in omega 6)
 *   - Analytical Observer: Risk of naturalizing contingent choice (analytical/analytical) — May incorrectly classify the constraint as mountain (immutable law of modernization) when it is actually a contingent state choice to use sartorial standardization as legitimacy marker
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sartorial_commitment_override, 0.58).
domain_priors:suppression_score(sartorial_commitment_override, 0.68).
domain_priors:theater_ratio(sartorial_commitment_override, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sartorial_commitment_override, extractiveness, 0.58).
narrative_ontology:constraint_metric(sartorial_commitment_override, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sartorial_commitment_override, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sartorial_commitment_override, tangled_rope).
narrative_ontology:human_readable(sartorial_commitment_override, "Sartorial Commitment Override in Meiji State Formation").
narrative_ontology:topic_domain(sartorial_commitment_override, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(sartorial_commitment_override).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(sartorial_commitment_override, formalized).
narrative_ontology:cs_authority_grounding(sartorial_commitment_override, extraction).
narrative_ontology:cs_interpretation_layer_present(sartorial_commitment_override).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sartorial_commitment_override, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(sartorial_commitment_override, international_diplomatic_recognition).
narrative_ontology:constraint_victim(sartorial_commitment_override, commoner_sartorial_identity).
narrative_ontology:constraint_victim(sartorial_commitment_override, regional_textile_producers).
narrative_ontology:constraint_victim(sartorial_commitment_override, ceremony_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMONER SARTORIAL IDENTITY (SNARE) — The commoner class has no structural alternative; legal prohibition on wearing Western dress until enforcement relaxed, combined with deep identity fusion with traditional garments (kimono as marker of cultural continuity and family status). Maximum suppression: material barriers (legal consequence) + identity barriers (self-concept constituted through sartorial tradition). This agent experiences pure extraction — the constraint extracts sartorial autonomy with minimal coordination benefit. The Meiji state appropriates the symbolic resource (dress as marker of modernity/civilization) for state legitimacy while crushing the prior commitment (traditional garments as markers of cultural belonging).
constraint_indexing:constraint_classification(sartorial_commitment_override, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL TEXTILE PRODUCER (TANGLED ROPE) — Constrained by capital requirements to shift production and by market displacement as state privileges Western textile imports. But also coordinates with Meiji modernization objectives: many textile producers eventually benefit from integration into national market and access to new production technologies. The constraint exhibits both coordination (economic integration) and extraction (displacement of regional production advantage) simultaneously. Suppression is high (capital barriers, market manipulation) but not total — some regional producers find new market niches.
constraint_indexing:constraint_classification(sartorial_commitment_override, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEIJI STATE APPARATUS (ROPE) — Experiences the sartorial override as pure coordination: standardizing dress codes solves the state's need for visual legibility of rank, institutional affiliation, and bureaucratic hierarchy. Western dress becomes the kernel legitimacy marker — 'civilized' equals 'Western-dressed' in the international diplomatic context. The state extracts sartorial autonomy from subjects to solve its own administrative and diplomatic coordination problem. Net beneficiary: international recognition (treaty power, diplomatic standing) flows toward agents who enforce Western dress adoption. This agent has arbitrage options (can exit or modify the constraint) and benefits maximally.
constraint_indexing:constraint_classification(sartorial_commitment_override, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRADITIONALIST PRESERVATION MOVEMENT (SCAFFOLD) — Organized actors (Shinto preservationists, noble families, ceremony practitioners) see the sartorial override as a temporary extraction mechanism with an implicit sunset: as Japan's international standing stabilizes, the performative need for visible Western dress compliance diminishes. By the Taishō period (1912-1926), formal kimono reclaims space in elite ceremony and state occasions. The constraint is experienced as extractive (suppression of cultural continuity) but with a visible exit path as the state's legitimacy claim shifts from 'imitating Western markers' to 'synthesizing Eastern tradition with Western technology.' Theater moderately high (formal dress codes perform bureaucratic hierarchy) but declining as the constraint's functional need declines.
constraint_indexing:constraint_classification(sartorial_commitment_override, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BUREAUCRATIC DRESS CODE LEGACY (PITON) — By the early 1900s, the Western dress mandate persists through institutional inertia rather than state legitimacy need. The original functional problem (international recognition gap) has been solved; Western dress becomes a performative marker of bureaucratic continuity rather than a functional necessity. Theater very high (0.70+): officials wear Western dress not because diplomatic recognition requires it but because 'that's what officials wear.' The constraint is maintained by administrative habit, not by strategic extraction.
constraint_indexing:constraint_classification(sartorial_commitment_override, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EXOGENOUS OVERRIDE FRAMING (MOUNTAIN) — From a universal/civilizational view, one might see the sartorial override as an immutable law of state modernization: all societies undergoing rapid state centralization and international integration must break prior sartorial commitments. The override appears as structural necessity rather than contingent choice. However, this naturalizes what the structural data reveals as a contingent institutional arrangement with identifiable beneficiaries (the Meiji state) and victims (commoner identity). The engine's false summit detector will flag this perspective as a false summit: the 'law of modernization' covers exogenous override (a specific mechanism) that is not necessary but contingent on the state's choice to use sartorial standardization as a legitimacy marker.
constraint_indexing:constraint_classification(sartorial_commitment_override, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sartorial_commitment_override_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sartorial_commitment_override, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sartorial_commitment_override, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sartorial_commitment_override, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sartorial_commitment_override, TR),
    TR >= 0.70.

:- end_tests(sartorial_commitment_override_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting time-averaged extraction across the 44-year interval. Early Meiji (1868-1880) extractiveness is 0.72 (near-snare threshold); by Taisho (1912) extractiveness declines to 0.40 (rope-adjacent). The 0.58 value is the midpoint across the measurement series and captures the constraint's lifecycle: high extraction during the state's active override phase, declining as the constraint's functional need declines. This is correctly tangled_rope (not snare) because the constraint coordinates bureaucratic hierarchy and international recognition (genuine coordination function) while simultaneously extracting sartorial autonomy. Suppression (0.68): High. Material barriers include legal prohibition on commoner Western dress until 1870s relaxation, economic privilege for Western textile imports, and career risk of non-compliance for bureaucrats. Identity barriers are extremely high (commoner self-concept constituted through garment tradition). However, suppression is not near-total (0.95) because regional variation and ceremonial exceptions permit some traditional dress practice. Theater ratio (0.65): Moderate-high, reflecting that the constraint contains both functional elements (bureaucratic hierarchy legibility) and performative elements (dress as 'civilization' marker in diplomatic contexts). Theater rises over time as the functional need declines, reaching 0.72 by 1912 (piton threshold). Claimed type (Tangled Rope) is appropriate because the beneficiaries (state apparatus, international recognition seekers) experience genuine coordination benefits alongside the extraction borne by victims (commoner identity, textile producers, ceremony practitioners).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is exceptional. The Meiji state apparatus (perspective 3) experiences pure coordination: solving the administrative need for visual legibility and international recognition. The commoner class (perspective 1) experiences pure extraction: suppression of identity and autonomy. The textile producers (perspective 2) experience mixed coordination and extraction. The preservation movement (perspective 4) experiences temporary extraction with a sunset clause. The bureaucratic legacy (perspective 5) experiences performative perpetuation. The analytical observer (perspective 6) risks naturalizing the state's top-down choice as immutable modernization law. The classification ranges from rope (state) to snare (commoner) to mountain (false summit). This gap is diagnostic: it reveals that the constraint's 'extraction' and 'coordination' are not objective features of the constraint itself but relational features that depend entirely on the observer's structural position. The state genuinely solves a coordination problem (bureaucratic hierarchy, international recognition). The commoner genuinely has autonomy suppressed (identity lock, legal prohibition). Both are true simultaneously. The constraint is tangled rope precisely because it coordinates for beneficiaries while extracting from victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (Meiji state apparatus): d ≈ 0.10 (strong beneficiary). The state architecture extraction privileges are high, and the agent has arbitrage options (can modify or exit the constraint). The sigmoid f(d) at this low d value yields strong negative effective extraction (the constraint subsidizes the state). Victim directionality (commoner class): d ≈ 0.90 (strong victim). The commoner class bears maximum suppression (legal + identity barriers) and has trapped exit options. The sigmoid f(d) at this high d value yields strong positive effective extraction (~1.28 by legacy π equivalence). The perspectival gap is extreme: the state sees coordination (rope) with arbitrage benefits; the commoner sees pure extraction (snare) with no exit. Regional textile producers occupy an ambiguous middle position: if genuine beneficiaries of market access, d ≈ 0.40 (near-neutral); if false beneficiaries displaced by state-privileged imports, d ≈ 0.75 (victim). Omega 4 specifically addresses this directionality ambiguity. The commission does not resolve it here; the empirical work omega 4 specifies is necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled: it coordinates bureaucratic hierarchy and international recognition (real coordination function) while simultaneously extracting sartorial autonomy (real extraction from commoner perspective). The false summit risk is in perspective 6 (the analytical mountain perspective), which might naturalize the state's necessity claim ('modernization requires Western dress') as a law of nature. The structural data reveals this as contingent: the state chose to use sartorial standardization as a legitimacy claim; it was not forced to do so by modernization logic. The omega variables (especially omega 6 on necessity, omega 5 on mechanism) prevent misclassification of a contingent institutional choice as immutable law. The constraint's lifecycle (declining extractiveness, rising theater) is diagnostic: if the constraint were a genuine natural law, extractiveness should remain constant. The fact that extractiveness declines as the state's diplomatic position stabilizes suggests the constraint's extraction was always contingent on the state's felt need for Western-dress-as-civilization-marker, not on any immutable requirement of modernization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_override_vs_reading_displacement,
    'Does the Meiji sartorial constraint represent exogenous override of an existing kernel (traditional dress norms as commitment system) or top-down installation of a new kernel (Western dress as bureaucratic legitimacy marker)?',
    'Historical evidence: were traditional dress norms codified as formal law with enforcement mechanisms BEFORE Meiji, or were they diffuse social practices? If codified, the Meiji constraint overrides a prior kernel. If diffuse, the Meiji constraint installs a new kernel. Examine Tokugawa sumptuary laws and their enforcement apparatus.',
    'If override: the constraint_system framework accommodates top-down kernel displacement as a distinct mechanism beyond ''climb from fringe.'' If installation: the traditional practices were not a commitment system proper, and the Meiji action installs the first kernel. This reshapes the theoretical claim about exogenous override.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_override_vs_reading_displacement, empirical, 'Whether Meiji sartorial constraint overrides a prior kernel or installs a new one').

omega_variable(
    identity_lock_mechanism_source,
    'Is the identity lock on commoner sartorial identity (perspective 1) a structural feature of the prior commitment system or a contingent artifact of how the Meiji state performed the override?',
    'Ethnographic analysis of pre-Meiji dress practices: did commoners report identity fusion with garments before the override (organic identity lock), or did identity lock emerge as a reactive defense against the override (constructed lock)? Compare commoner responses in early vs. late Meiji period.',
    'If organic: the constraint''s suppression reflects deep cultural structure. If constructed: the suppression is amplified by the state''s enforcement and by reactive identity crystallization. This affects interpretation of whether the constraint is necessary or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_source, empirical, 'Whether identity lock on traditional dress is pre-existing or emerges from the override').

omega_variable(
    extractiveness_attribution_temporal,
    'Does extractiveness (0.58) correctly reflect the early Meiji period (1868-1880, strict enforcement with high suppression) or is it a time-averaged measure that conflates high early extraction with declining extraction after 1895?',
    'Temporal decomposition: measure suppression, theater ratio, and beneficiary/victim clarity across three sub-periods (1868-1880, 1881-1895, 1896-1912). Plot separately to identify the constraint''s lifecycle.',
    'If extractiveness is underestimated in early period: the constraint classifies as snare (not tangled rope) in 1868-1880. If declining extraction is weighted equally: the tangled rope classification conflates high-extraction and declining-extraction phases. Recommend decomposing into separate stories per sub-period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_attribution_temporal, empirical, 'Whether extractiveness measure reflects early strict period or time-averaged decline').

omega_variable(
    textile_producer_beneficiary_status_ambiguous,
    'Are regional textile producers true beneficiaries (gaining market access and modernization) or false beneficiaries (displaced by state-privileged Western imports while appearing to participate in modernization)?',
    'Economic data: compare regional textile producer output, profit margins, and workforce size before (1850) vs. after (1880, 1900, 1910). If output and margins rise, beneficiary status is genuine. If they decline or remain flat while market access widens, beneficiary status is performative (false summit effect in beneficiary declaration).',
    'If true beneficiaries: the constraint''s coordination function is real and tangled rope classification is robust. If false beneficiaries: the victims group should include textile producers, and the constraint may classify as snare from their perspective (high d, high f(d), high χ). This requires decomposition into separate constraint stories per actor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textile_producer_beneficiary_status_ambiguous, empirical, 'Whether textile producers are genuine beneficiaries or displaced by the constraint').

omega_variable(
    exogenous_override_mechanism_sui_generis,
    'Does the Meiji sartorial override instantiate a theoretically distinct mechanism (top-down kernel installation by state power) that the M-set framework cannot accommodate, or is it adequately modeled as a high-power, low-time-horizon perspective displacing a fringe commitment system?',
    'Comparative historical analysis: examine other cases of state-imposed sartorial standardization (Peter the Great''s beard tax, Nazi uniforms, Soviet dress codes) to identify common structural features. If pattern is consistent, the mechanism is distinct and requires theoretical incorporation. If highly context-dependent, the M-set framework may be adequate with refinement to power asymmetries.',
    'If distinct mechanism: the constraint_system framework requires a new axiom for exogenous override (top-down kernel displacement). If adequately modeled: refine the framework''s treatment of power-asymmetric perspective conflicts. This is a meta-level omega about the adequacy of the DR formalism itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exogenous_override_mechanism_sui_generis, conceptual, 'Whether exogenous override is a theoretically distinct mechanism beyond M-set framework').

omega_variable(
    sartorial_signaling_necessity_claim,
    'Is the Meiji state''s claim that Western dress was necessary for international diplomatic recognition (''civilization'' marker required by Western treaty powers) factually true, strategically motivated, or a post-hoc rationalization of a preferred aesthetic choice?',
    'Diplomatic historical record: examine treaty negotiations with Western powers and identify explicit dress-code requirements in treaty text. If required: the claim is genuine constraint. If not required: examine state officials'' correspondence to determine whether dress codes were believed necessary or strategically chosen. If post-hoc: identify when the ''necessity'' narrative was first articulated and by whom.',
    'If genuine necessity: the constraint may partially legitimize as rope (solving real coordination problem). If strategic choice: the constraint classifies more clearly as snare (pure extraction with a false necessity narrative). This affects the false summit detection on perspective 6.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sartorial_signaling_necessity_claim, empirical, 'Whether Western dress requirement was genuinely necessary for diplomatic recognition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sartorial_commitment_override, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_meiji_1868, sartorial_commitment_override, theater_ratio, 0, 0.5).
narrative_ontology:measurement(theater_mid_meiji_1881, sartorial_commitment_override, theater_ratio, 13, 0.58).
narrative_ontology:measurement(theater_late_meiji_1895, sartorial_commitment_override, theater_ratio, 27, 0.68).
narrative_ontology:measurement(theater_taisho_1912, sartorial_commitment_override, theater_ratio, 44, 0.72).

% Extraction over time
narrative_ontology:measurement(extraction_early_meiji_1868, sartorial_commitment_override, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(extraction_mid_meiji_1881, sartorial_commitment_override, base_extractiveness, 13, 0.6).
narrative_ontology:measurement(extraction_late_meiji_1895, sartorial_commitment_override, base_extractiveness, 27, 0.45).
narrative_ontology:measurement(extraction_taisho_1912, sartorial_commitment_override, base_extractiveness, 44, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sartorial_commitment_override, identity_coordination).
narrative_ontology:boltzmann_floor_override(sartorial_commitment_override, 0.22).
narrative_ontology:affects_constraint(sartorial_commitment_override, gregorian_calendar_adoption_meiji).
narrative_ontology:affects_constraint(sartorial_commitment_override, technology_import_privilege_meiji).

% DUAL FORMULATION NOTE:
% The Meiji sartorial override is part of a constraint family of top-down institutional impositions during state modernization (1868-1912). Sartorial, calendrical, and technological standardization operate in parallel with shared structural features (state beneficiary, commoner victim, international recognition justification, lifecycle decline). These three constraints should be linked as a modernization family, each with its own extractiveness measure reflecting different victim identity-lock depths and different coordination functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sartorial_commitment_override, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
