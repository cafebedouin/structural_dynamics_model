% ============================================================================
% CONSTRAINT STORY: state_capacity_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_capacity_expansion, []).

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
 *   constraint_id: state_capacity_expansion
 *   human_readable: State Capacity Expansion via Alphabet Reform
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Turkish alphabet reform (1928) exemplifies state capacity expansion
 *   through orthographic standardization. The Ottoman Empire used Arabic
 *   script for administrative, religious, and literary purposes. The Kemalist
 *   state replaced it with Latin script, ostensibly to modernize literacy and
 *   align with European standards. The reform created genuine coordination
 *   benefits (standardized literacy enabled census-taking, tax collection,
 *   military recruitment, and inter-regional communication) alongside severe
 *   extraction: the Ottoman literate class lost their accumulated cultural
 *   capital overnight, regional autonomy was undermined by centralized
 *   literacy standards, and cultural continuity mechanisms (Quranic
 *   transmission, Ottoman literary tradition) were disrupted. The constraint
 *   exhibits all six DR types from different perspectives. The central state
 *   bureaucracy experiences pure coordination (Rope). The Ottoman literate
 *   class experiences pure extraction (Snare). Regional elites experience
 *   mixed coordination and extraction (Tangled Rope). The analytical observer
 *   risks naturalizing the reform as an inevitable consequence of
 *   modernization (Mountain/false summit). The constraint's extractiveness
 *   (0.62) reflects the asymmetric distribution of costs and benefits: the
 *   state and new literate class benefit; the Ottoman literate class and
 *   regional autonomy bearers bear the costs. The suppression (0.68) reflects
 *   the enforcement machinery required to suppress alternatives: banning
 *   Arabic script in schools, requiring Latin script in official documents,
 *   controlling educational content. The theater ratio (0.35) is relatively
 *   low because the reform's coordination function is genuine — standardized
 *   literacy does enable state capacity — but the extraction mechanism is
 *   also genuine and visible.
 *
 * KEY AGENTS:
 *   - Central State Bureaucracy: Primary beneficiary (institutional/arbitrage) — gains expanded administrative capacity, tax collection efficiency, military recruitment standardization
 *   - Ottoman Literate Class: Primary victim (powerless/trapped) — loses accumulated literacy capital, professional status, cultural authority; no exit options within national territory
 *   - Regional Autonomy Bearers: Secondary victim (moderate/constrained) — lose administrative autonomy, regional script authority, cultural transmission control; constrained by state enforcement
 *   - New Literate Class: Secondary beneficiary (organized/mobile) — gains career opportunities, educational expansion, cultural prestige; mobile exit (could learn Arabic if needed)
 *   - Religious and Literary Institutions: Mixed (institutional/constrained) — coordinate genuine functions (Quranic transmission, literary preservation) but lose autonomy over curriculum and script
 *   - Regional Cultural Continuity Bearers: Victim (powerless/identity_locked) — identity fused with Arabic script and Ottoman cultural transmission; cannot exit without abandoning identity frame
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inevitable modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_capacity_expansion, 0.62).
domain_priors:suppression_score(state_capacity_expansion, 0.68).
domain_priors:theater_ratio(state_capacity_expansion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_capacity_expansion, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_capacity_expansion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_capacity_expansion, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_capacity_expansion, tangled_rope).
narrative_ontology:human_readable(state_capacity_expansion, "State Capacity Expansion via Alphabet Reform").
narrative_ontology:topic_domain(state_capacity_expansion, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(state_capacity_expansion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_capacity_expansion, '687eb5b4-27ec-45a9-989b-dc26d2dc596d').
narrative_ontology:cs_kernel_codification('687eb5b4-27ec-45a9-989b-dc26d2dc596d', formalized).
narrative_ontology:cs_authority_grounding('687eb5b4-27ec-45a9-989b-dc26d2dc596d', extraction).
narrative_ontology:cs_interpretation_layer_present('687eb5b4-27ec-45a9-989b-dc26d2dc596d').
narrative_ontology:cs_reading_relation('687eb5b4-27ec-45a9-989b-dc26d2dc596d', state_capacity_expansion__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('687eb5b4-27ec-45a9-989b-dc26d2dc596d', state_capacity_expansion__cultural_rupture_reading, coexists_with).
narrative_ontology:cs_axiom('687eb5b4-27ec-45a9-989b-dc26d2dc596d', foundational, latin_script_enables_modernization).
narrative_ontology:cs_axiom_status(latin_script_enables_modernization, holdable).
narrative_ontology:cs_axiom_grounding('687eb5b4-27ec-45a9-989b-dc26d2dc596d', latin_script_enables_modernization, empirically_contingent).
narrative_ontology:cs_axiom('687eb5b4-27ec-45a9-989b-dc26d2dc596d', secondary, technological_progress_requires_european_alignment).
narrative_ontology:cs_axiom_status(technological_progress_requires_european_alignment, holdable).
narrative_ontology:cs_axiom_grounding('687eb5b4-27ec-45a9-989b-dc26d2dc596d', technological_progress_requires_european_alignment, instrumental).
narrative_ontology:cs_axiom('687eb5b4-27ec-45a9-989b-dc26d2dc596d', foundational, national_sovereignty_requires_orthographic_standardization).
narrative_ontology:cs_axiom_status(national_sovereignty_requires_orthographic_standardization, holdable).
narrative_ontology:cs_axiom_grounding('687eb5b4-27ec-45a9-989b-dc26d2dc596d', national_sovereignty_requires_orthographic_standardization, conventional).
narrative_ontology:cs_reference_frame('687eb5b4-27ec-45a9-989b-dc26d2dc596d', ottoman_multilingual_administrative_system).
narrative_ontology:cs_drift_state('687eb5b4-27ec-45a9-989b-dc26d2dc596d', post_reform_consolidation, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('687eb5b4-27ec-45a9-989b-dc26d2dc596d', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_capacity_expansion, central_state_bureaucracy).
narrative_ontology:constraint_beneficiary(state_capacity_expansion, new_literate_class).
narrative_ontology:constraint_victim(state_capacity_expansion, regional_autonomy).
narrative_ontology:constraint_victim(state_capacity_expansion, ottoman_literate_class).
narrative_ontology:constraint_victim(state_capacity_expansion, cultural_continuity_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_capacity_expansion, religious_and_literary_institutions).
narrative_ontology:constraint_beneficiary(state_capacity_expansion, european_aligned_intellectuals).
narrative_ontology:constraint_victim(state_capacity_expansion, regional_administrative_elites).
narrative_ontology:constraint_victim(state_capacity_expansion, religious_and_literary_institutions).
narrative_ontology:constraint_victim(state_capacity_expansion, regional_cultural_continuity_bearers).
narrative_ontology:constraint_vindicates(state_capacity_expansion, national_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(state_capacity_expansion, modernization_imperative).
narrative_ontology:constraint_vindicates(state_capacity_expansion, technological_determinism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state bureaucracy sets the script reform agenda and enforces it through educational policy, official documentation requirements, and legal mandates. They benefit from expanded administrative capacity (standardized literacy enables census-taking, tax collection, military recruitment). They can exit by reverting to regional scripts if the reform fails, but have no incentive to do so. They experience the constraint as enabling rather than extractive.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, central_state_bureaucracy, agenda_setter,
    institutional, immediate, arbitrage, national).

% The Ottoman literate class (scholars, administrators, religious leaders, literary figures) loses their accumulated literacy capital overnight. Their decades of education in Arabic script become economically worthless. They cannot exit the constraint without abandoning professional identity and social status. They face maximum extraction: their cultural capital is confiscated and their knowledge base is rendered obsolete by state mandate.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, ottoman_literate_class, payer,
    powerless, biographical, trapped, national).

% Regional administrative elites lose autonomy over educational policy and script standards. They must implement the script reform in their regions, which requires retraining staff, updating documentation systems, and suppressing alternative literacy pathways. They benefit from improved inter-regional communication and standardized administrative procedures, but lose regional authority. They face high costs to resist (state enforcement) and moderate costs to comply (learning new script, retraining).
narrative_ontology:constraint_stakeholder(state_capacity_expansion, regional_administrative_elites, payer,
    moderate, biographical, constrained, regional).

% The new literate class (educators, publishers, intellectuals aligned with modernization) benefits from the script reform. The new script creates career opportunities, educational expansion, and cultural prestige. They are organized agents who can mobilize to support the reform. They can learn Arabic script if needed, but have no incentive to do so. They experience the constraint as enabling rather than extractive.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, new_literate_class, beneficiary,
    organized, generational, mobile, national).

% Religious and literary institutions (mosques, madrasas, literary societies) coordinate genuine functions (Quranic transmission, literary preservation, theological education) that depend on Arabic script. The reform creates both coordination problems (how to maintain religious literacy?) and extraction (state control over educational content and script standards). They benefit from state-sponsored education expansion but lose autonomy over curriculum and script. They face high costs to resist (state enforcement) and moderate costs to comply (adapting religious education to new script).
narrative_ontology:constraint_stakeholder(state_capacity_expansion, religious_and_literary_institutions, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_capacity_expansion, religious_and_literary_institutions, beneficiary).

% Regional cultural continuity bearers (families, communities, cultural practitioners) are identity-locked by the constraint. Their cultural transmission mechanisms (Quranic literacy, Ottoman literary tradition, regional administrative practices) are constituted through the Arabic script. The script reform is not merely a barrier to exit — it is an attack on the identity frame itself. They are structurally mobile (could migrate, could learn new script) but identity-fused with the continuity function. The constraint forces a choice between cultural identity and state participation.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, regional_cultural_continuity_bearers, payer,
    powerless, generational, identity_locked, regional).

% European-aligned intellectuals and modernizers benefit from the script reform as a symbol of national alignment with European standards and technological modernity. They are organized agents who can mobilize to support the reform through intellectual and cultural production. They experience the constraint as enabling national modernization and European integration.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, european_aligned_intellectuals, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized literacy enables inter-regional communication, census-taking, tax collection, military recruitment, and centralized administrative coordination. The genuine coordination problem is: how can a multi-regional state with diverse local scripts achieve administrative standardization and unified governance? The script reform solves this by imposing a single orthographic standard.
% TRANSFER_FUNCTION: The reform transfers literacy capital from the Ottoman literate class (who lose their accumulated knowledge) to the new literate class (who gain career opportunities and cultural prestige). It transfers administrative autonomy from regional elites to the central state bureaucracy. It transfers cultural authority from religious and literary institutions to the state educational apparatus.
% ABSENT_VOICES: The Ottoman literate class and regional cultural continuity bearers are present in the constraint but have no seat at the agenda-setting table. Religious and literary institutions are consulted but their objections are overridden. The voices absent from the decision-making process are those who would defend Arabic script preservation and Ottoman cultural continuity — these voices are excluded by design, not by accident.
% DISAPPEARANCE_RATIONALE: If the script reform disappeared overnight (reverted to Arabic script), the world would rearrange substantially. The state would lose administrative standardization and would need to rebuild regional script autonomy. Educational systems would need to revert to Arabic script instruction. Official documentation would need to accommodate multiple scripts. The new literate class would lose their career advantages. The Ottoman literate class would regain cultural authority. Regional autonomy would be restored. The constraint's disappearance would trigger major institutional reorganization.
% FOUNDING_PROBLEM: The founding problem was to expand state capacity for centralized governance and modernization. The Ottoman Empire's multi-regional, multi-script administrative system was seen as inefficient and as an obstacle to technological modernization and European alignment. The script reform was designed to solve this by imposing a single orthographic standard that would enable standardized literacy, centralized education, and unified administrative procedures.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Kemalist state documents, educational policy records, and administrative reforms. The state capacity mandate is confirmed by the measurable outcomes: census accuracy improved, tax collection efficiency increased, military recruitment standardization was achieved. However, the founding problem is also contested by cultural continuity bearers and Ottoman literate class representatives, who argue that the problem was artificially constructed to justify cultural rupture. The corroboration is strongest from the state apparatus and new literate class; weakest from the Ottoman literate class and regional autonomy bearers.
narrative_ontology:disappearance_verdict(state_capacity_expansion, world_rearranges).
narrative_ontology:founding_problem_status(state_capacity_expansion, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OTTOMAN LITERATE CLASS (SNARE) — Trapped by the script reform. Decades of accumulated literacy in Arabic script become economically worthless overnight. Cannot exit the constraint without abandoning professional identity and social status. Faces maximum extraction: their cultural capital is confiscated and their knowledge base is rendered obsolete by state mandate. No alternatives exist within the national territory.
constraint_indexing:constraint_classification(state_capacity_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL CULTURAL CONTINUITY BEARERS (SNARE) — Identity-locked by the constraint. Their cultural transmission mechanisms (Quranic literacy, Ottoman literary tradition, regional administrative practices) are constituted through the Arabic script. The script reform is not merely a barrier to exit — it is an attack on the identity frame itself. Structurally mobile (could migrate, could learn new script) but identity-fused with the continuity function. The constraint forces a choice between cultural identity and state participation.
constraint_indexing:constraint_classification(state_capacity_expansion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL ADMINISTRATIVE ELITES (TANGLED ROPE) — Constrained by resource requirements and career risk. The reform creates genuine coordination benefits (standardized literacy enables inter-regional communication and census accuracy) alongside asymmetric extraction (their regional authority is undermined by centralized literacy standards). They benefit from improved administrative capacity but lose autonomy. High cost to resist; moderate cost to comply.
constraint_indexing:constraint_classification(state_capacity_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL STATE BUREAUCRACY (ROPE) — Experiences the constraint as pure coordination. The script reform solves a genuine collective-action problem: standardized literacy enables census-taking, tax collection, military recruitment, and inter-departmental communication. The bureaucracy benefits from expanded state capacity without bearing the costs of the transition. Arbitrage exit: can always revert to regional scripts if the reform fails, but has no incentive to do so.
constraint_indexing:constraint_classification(state_capacity_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NEW LITERATE CLASS (ROPE) — Organized agents (educators, publishers, intellectuals aligned with modernization) who benefit from the reform. The new script creates career opportunities, educational expansion, and cultural prestige. Mobile exit: can learn Arabic script if needed, but have no incentive to do so. Experience the constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(state_capacity_expansion, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: RELIGIOUS AND LITERARY INSTITUTIONS (TANGLED ROPE) — Constrained by the reform. These institutions coordinate genuine functions (Quranic transmission, literary preservation, theological education) that depend on Arabic script. The reform creates both coordination problems (how to maintain religious literacy?) and extraction (state control over educational content and script standards). They benefit from some aspects of state-sponsored education but lose autonomy over curriculum and script.
constraint_indexing:constraint_classification(state_capacity_expansion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECHNOLOGICAL DETERMINISM (MOUNTAIN) — From a civilizational perspective, script standardization appears as an inevitable consequence of state modernization: complex administrative systems require standardized literacy, and technological progress demands alignment with international standards. This perspective naturalizes the reform as a law of state development. However, the structural data reveals this as a false summit: the reform's extractiveness (0.62) and suppression (0.68) indicate a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(state_capacity_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_capacity_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_capacity_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_capacity_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_capacity_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_capacity_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The reform creates genuine coordination benefits (standardized literacy enables state capacity) but distributes costs asymmetrically. The Ottoman literate class bears maximum extraction (their literacy becomes worthless); the state and new literate class capture benefits. The measurement trajectory shows extractiveness rising sharply in years 2-5 (peak enforcement period) then declining by year 20 (as new generation grows up with Latin script and Ottoman literacy becomes historical artifact). The moderate-high value reflects that the coordination function is real but the extraction is substantial. Suppression (0.68): High. The reform requires active enforcement: banning Arabic script in schools, requiring Latin script in official documents, controlling educational content, suppressing alternative literacy pathways. The measurement trajectory shows suppression rising sharply in years 2-5 (peak enforcement) then declining by year 20 (as enforcement becomes normalized and internalized). Theater ratio (0.35): Low-moderate. The reform's coordination function is genuine — standardized literacy does enable state capacity — so the constraint is not primarily performative. However, some theater exists in the modernization narrative (the claim that Latin script is inherently superior to Arabic for modernization is partly ideological). The low theater ratio reflects that the constraint's primary function is coordination, not performance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is driven by directionality and power asymmetry. The central state bureaucracy (institutional/arbitrage) experiences the constraint as Rope because their directionality is low (beneficiary status) and their power is high (can enforce the reform). The Ottoman literate class (powerless/trapped) experiences the constraint as Snare because their directionality is high (victim status) and their power is low (cannot resist enforcement). Regional elites (moderate/constrained) experience the constraint as Tangled Rope because their directionality is moderate (mixed victim/beneficiary status) and their power is moderate (can negotiate but cannot override state authority). The analytical observer risks seeing a Mountain because they occupy a civilizational/universal perspective that naturalizes the reform as inevitable modernization, but the structural data reveals this as a false summit: the reform's extractiveness and suppression indicate a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the constraint. The central state bureaucracy is a beneficiary with arbitrage exit options (d ≈ 0.1): they can always revert to regional scripts if the reform fails, but have no incentive to do so. The Ottoman literate class is a victim with trapped exit options (d ≈ 0.95): they cannot exit without abandoning professional identity and social status. Regional elites are victims with constrained exit options (d ≈ 0.65): they face high costs to resist (state enforcement) but moderate costs to comply (learning new script, retraining staff). The new literate class are beneficiaries with mobile exit options (d ≈ 0.25): they benefit from the reform but could learn Arabic if needed. Religious institutions are mixed (d ≈ 0.55): they coordinate genuine functions but lose autonomy. The engine derives d from these context parameters and applies the sigmoid f(d) to produce experienced extractiveness chi. Beneficiaries with arbitrage options experience low or negative effective extraction; trapped agents with no exit bear maximum extraction; constrained agents experience moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CANDIDATE: The constraint's founding mandate was to expand state capacity for administrative purposes (census-taking, tax collection, military recruitment). This mandate is live and functional — the reform does achieve these goals. However, the constraint exhibits secondary mandatrophy in the cultural transmission domain: the reform was justified partly as enabling modernization, but it also functioned as cultural rupture (severing Ottoman/Islamic continuity). The cultural transmission mandate (preserving Ottoman literary tradition, enabling Quranic literacy) was abandoned or subordinated to the state capacity mandate. This is not full mandatrophy (the primary mandate is still live) but partial mandatrophy (secondary mandates were abandoned). The constraint does not resolve mandatrophy because the state capacity mandate remains functional and the cultural rupture was intentional (not accidental degradation). However, the constraint exhibits the structural signature of mandatrophy: a secondary function (cultural continuity) was sacrificed to the primary function (state capacity), and this sacrifice is now maintained through enforcement rather than through genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed,
    'Is script standardization an inevitable consequence of state modernization, or a contingent institutional choice that benefits specific actors?',
    'Comparative historical analysis: examine cases where state capacity expanded without script reform (multilingual empires, federal systems); identify whether technological modernization requires orthographic standardization or merely benefits from it.',
    'If inevitable: mountain classification confirmed. If contingent: false summit detected; reclassify as tangled_rope or snare depending on extraction magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, empirical, 'Whether script standardization is natural law or constructed constraint').

omega_variable(
    literacy_expansion_vs_exclusion,
    'Does the script reform expand overall literacy rates, or does it exclude the previously literate while including new populations?',
    'Longitudinal literacy data: pre-reform literacy rates by script, post-reform literacy rates by script; tracking of individuals who lost literacy vs gained literacy; comparison of total literate population before and after reform.',
    'If net expansion: coordination function is genuine (Rope elements confirmed). If net exclusion: extraction function dominates (Snare elements confirmed). If mixed: tangled_rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_expansion_vs_exclusion, empirical, 'Whether reform expands or excludes literacy').

omega_variable(
    regional_autonomy_loss_mechanism,
    'Does the script reform directly cause regional autonomy loss, or does it enable state capacity that would have expanded anyway through other mechanisms?',
    'Counterfactual analysis: examine what state capacity expansion would have looked like without script reform; identify whether regional autonomy loss is attributable to the script reform specifically or to broader centralization dynamics.',
    'If direct causation: victim set (regional_autonomy) is correctly identified. If indirect/enabling: regional autonomy loss is a side effect rather than a primary extraction mechanism; reclassify regional_autonomy as secondary victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_autonomy_loss_mechanism, empirical, 'Causal mechanism linking script reform to regional autonomy loss').

omega_variable(
    kernel_reading_foreclosure,
    'Do the continuity and rupture readings of the orthographic kernel logically foreclose each other, or can they coexist as different parties'' commitments?',
    'Textual analysis of foundational commitments: examine whether continuity reading''s claim (orthographic stability is necessary for cultural transmission) logically contradicts rupture reading''s claim (orthographic discontinuity is necessary for cultural transformation). Assess whether a single coherent framework could hold both.',
    'If foreclosure: readings are mutually exclusive; one must be overridden or abandoned. If coexistence: readings represent different parties'' simultaneous commitments; constraint exhibits genuine kernel contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether kernel readings logically foreclose each other').

omega_variable(
    modernization_imperative_grounding,
    'Is the modernization imperative grounded in genuine technological necessity, or in European alignment preferences that could be satisfied through other means?',
    'Technical analysis: identify which specific state functions require Latin script vs which merely benefit from it; examine whether equivalent state capacity could be achieved through Arabic script modernization (e.g., standardized Arabic orthography, printing technology adaptation).',
    'If necessity: modernization reading''s authority grounding is empirically justified. If preference: modernization reading''s authority grounding is instrumental/political rather than technological; reclassifies the vindicated_propositions as contingent rather than natural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_imperative_grounding, empirical, 'Whether modernization imperative is technological necessity or European alignment preference').

omega_variable(
    ottoman_literate_class_exit_options,
    'Could the Ottoman literate class have maintained their literacy and social status through alternative mechanisms (e.g., bilingual education, script coexistence, regional autonomy)?',
    'Historical counterfactual: examine whether bilingual or multilingual literacy systems existed in comparable contexts; assess whether regional script autonomy was structurally possible within the state formation project.',
    'If alternatives existed: trapped classification is confirmed (exit was suppressed, not impossible). If no alternatives: trapped classification may be overstated; reclassify as constrained (high-cost exit was available).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ottoman_literate_class_exit_options, empirical, 'Whether Ottoman literate class had viable exit options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_capacity_expansion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sce_tr_t0, state_capacity_expansion, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sce_tr_t5, state_capacity_expansion, theater_ratio, 5, 0.35).
narrative_ontology:measurement(sce_tr_t10, state_capacity_expansion, theater_ratio, 10, 0.42).
narrative_ontology:measurement(sce_tr_t20, state_capacity_expansion, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(sce_be_t0, state_capacity_expansion, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sce_be_t2, state_capacity_expansion, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(sce_be_t5, state_capacity_expansion, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(sce_be_t10, state_capacity_expansion, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sce_be_t20, state_capacity_expansion, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sce_su_t0, state_capacity_expansion, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sce_su_t2, state_capacity_expansion, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(sce_su_t5, state_capacity_expansion, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(sce_su_t10, state_capacity_expansion, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(sce_su_t20, state_capacity_expansion, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_capacity_expansion, information_standard).
narrative_ontology:boltzmann_floor_override(state_capacity_expansion, 0.08).
narrative_ontology:affects_constraint(state_capacity_expansion, ottoman_cultural_continuity).
narrative_ontology:affects_constraint(state_capacity_expansion, regional_administrative_autonomy).
narrative_ontology:affects_constraint(state_capacity_expansion, quranic_literacy_transmission).

% DUAL FORMULATION NOTE:
% The state capacity expansion constraint is upstream of three sibling constraints representing the three kernel readings. The modernization reading (this story) has moderate extractiveness (0.62) because the coordination function is genuine. The continuity reading would have high extractiveness (≈0.75) because the victim set (Ottoman literate class) is larger and the coordination function is weaker. The rupture reading would have very high extractiveness (≈0.85) because the victim set includes the entire pre-reform literate population and the coordination function is explicitly rejected. These three stories should be linked via network.affects_constraints to show the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_capacity_expansion, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
