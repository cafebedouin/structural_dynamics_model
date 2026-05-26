% ============================================================================
% CONSTRAINT STORY: generational_literacy_rupture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generational_literacy_rupture, []).

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
 *   constraint_id: generational_literacy_rupture
 *   human_readable: Generational Literacy Rupture: Turkey 1928 Alphabet Reform
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   Turkey's 1928 alphabet reform (Law 1353, enacted by Atatürk's government)
 *   mandated replacement of Ottoman Arabic script with Latin-based alphabet
 *   within three months, creating a generational literacy rupture with
 *   near-zero prior practitioners of the new script. The reform represents an
 *   extreme case of top-down linguistic engineering deployed as state
 *   consolidation and cultural modernization tool. It severed access to
 *   Ottoman institutional knowledge, delegitimized religious scholarship
 *   grounded in Arabic script literacy, and created asymmetric literacy costs
 *   across age cohorts. Simultaneously, it functioned as genuine coordination
 *   mechanism for state administrative unification and future education. The
 *   constraint exhibits tangled structure: a legitimate modernization benefit
 *   (unified script, integration with European standards, lower cognitive
 *   load for universal literacy) overlaid with significant extraction
 *   (destruction of alternative institutional pathways, forced literacy loss
 *   for educated elderly, institutional capture of religious authorities).
 *   The reform's 3-month enforcement timeline is diagnostic: adequate to
 *   prevent organized resistance, compressed enough to foreclose gradual
 *   transition alternatives that would distribute costs more evenly. Theater
 *   ratio trajectory shows increasing performativity: early implementation
 *   (t=0-1) is substantive disruption; over subsequent decades (t=5-30), the
 *   reform increasingly functions as symbolic marker of modernization ('look
 *   how we progressed') while actual modernization benefits plateau and costs
 *   of rupture become normalized as historical fact.
 *
 * KEY AGENTS:
 *   - Atatürk's State Modernization Apparatus: Institutional beneficiary (institutional/arbitrage) — captures legitimacy, state consolidation, cultural authority; enforces constraint through monopoly on education and legal script status
 *   - Elderly Literate Class (40+ years): Primary victim (powerless/trapped) — lifetime literacy capital rendered invalid; no age-compatible acquisition pathway; trapped in illiteracy
 *   - Religious Scholars and Quranic Traditionalists: Institutional victim (moderate/trapped) — face existential constraint on knowledge transmission; religious literacy systems become institutionally inaccessible; institutional capture via linguistic rupture
 *   - Young Students and Children (under 15): Secondary beneficiary (moderate/mobile) — experience the new script as natural native literacy; benefit from unified standard; no loss perception; coordination-only experience
 *   - Middle-Aged Transitional Cohort (20-40 years): Mixed (moderate/constrained) — dual literacy burden; some can acquire new script but at cost; experience both extraction (lost capital) and coordination benefit (future integration)
 *   - Administrative-Bureaucratic Continuity: Institutional complexity — Ottoman bureaucracy must transition; new state apparatus benefits from unified script; mixed extraction and coordination
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political choice as linguistic inevitability; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generational_literacy_rupture, 0.58).
domain_priors:suppression_score(generational_literacy_rupture, 0.72).
domain_priors:theater_ratio(generational_literacy_rupture, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generational_literacy_rupture, extractiveness, 0.58).
narrative_ontology:constraint_metric(generational_literacy_rupture, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(generational_literacy_rupture, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generational_literacy_rupture, tangled_rope).
narrative_ontology:human_readable(generational_literacy_rupture, "Generational Literacy Rupture: Turkey 1928 Alphabet Reform").
narrative_ontology:topic_domain(generational_literacy_rupture, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(generational_literacy_rupture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generational_literacy_rupture, state_modernization_apparatus).
narrative_ontology:constraint_beneficiary(generational_literacy_rupture, educated_urban_elites).
narrative_ontology:constraint_beneficiary(generational_literacy_rupture, future_standardized_literacy).
narrative_ontology:constraint_victim(generational_literacy_rupture, elderly_literate_population).
narrative_ontology:constraint_victim(generational_literacy_rupture, religious_scholars).
narrative_ontology:constraint_victim(generational_literacy_rupture, administrative_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELDERLY LITERATE CLASS (SNARE) — Agents (40+ years old) who spent their lives acquiring mastery of Ottoman Arabic script face immediate illiteracy. No exit option: they cannot unlearn their script or rapidly acquire new one at advanced age. The constraint extracts their lifetime literacy capital and offers no compensation. Maximum suppression — their literacy is rendered legally invalid within months; books, documents, and knowledge systems become inaccessible. No alternative pathway exists; resistance is criminalized as reactionary.
constraint_indexing:constraint_classification(generational_literacy_rupture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS SCHOLARS & QURANIC TRADITIONALISTS (SNARE) — Face existential constraint on their primary function. The Quran is written in Arabic script; Quranic transmission, interpretation, and sacred knowledge transmission depend on script recognition. The reform traps them: they cannot teach the next generation to read the Quran in its original form within the new institutional context. No arbitrage available — their specialized knowledge becomes institutionally inaccessible. Suppression operates through state education monopoly and religious institution capture.
constraint_indexing:constraint_classification(generational_literacy_rupture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-AGED TRANSITIONAL COHORT (TANGLED ROPE) — Adults aged 20-40 face dual literacy demands: they must abandon one script and acquire another while maintaining economic productivity. This cohort experiences both extraction (loss of acquired literacy capital, retraining burden) and coordination benefit (the reform provides unified national literacy, enabling broader communication and state administration efficiency). Mixed experience: constrained exit (can physically exit Ottoman institutions but socially/economically cannot exit the state) with asymmetric costs.
constraint_indexing:constraint_classification(generational_literacy_rupture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: YOUNG STUDENTS & CHILDREN (ROPE) — The reform functions as pure coordination mechanism for agents under age 15. The new script becomes their native literacy; no comparison cost or loss of alternative knowledge is experienced. Suppression is minimal from this perspective — they acquire the script as natural childhood learning. The constraint coordinates a unified national literacy standard. No extraction perceived; the apparent benefit (modern, standardized, Latin-based script aligning with European modernity) is structural coordination, not coercive.
constraint_indexing:constraint_classification(generational_literacy_rupture, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE MODERNIZATION APPARATUS (TANGLED ROPE) — Institutional beneficiary (Atatürk's government, military, civil service) pursues genuine coordination goal: unified national literacy as a prerequisite for state consolidation, universal education, and cultural modernization. The reform also functions as cultural extraction: it severs Ottoman institutional memory, delegitimizes religious scholarship, and eliminates an alternative administrative pathway. The state perceives coordination; populations without power perceive extraction. Institutional perspective shows the hybrid clearly: enforcement mechanisms are genuine (state monopoly on education, legal script status) and extraction is real (loss of institutional pluralism).
constraint_indexing:constraint_classification(generational_literacy_rupture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: OTTOMAN ADMINISTRATIVE CONTINUITY (PITON) — From the civilizational time horizon, the Ottoman Arabic script administration is a degraded institution awaiting replacement. The reform treats it as performative (maintains continuity fiction while destroying actual continuity) rather than functional. The theater ratio is low from this perspective (the reform is substantive disruption, not theatrical performance); but the perspective is markedly institutional inertia—the Ottoman system persists as a reference frame even as the reform destroys it. The piton classification reflects viewing the pre-reform system as already defunct, merely waiting institutional replacement.
constraint_indexing:constraint_classification(generational_literacy_rupture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LINGUISTIC DETERMINISM (MOUNTAIN) — From a universal, civilizational perspective, language script evolution appears as an inevitable historical process: all writing systems eventually modernize toward phonetic simplicity and integration with dominant international standards. The shift from logographic/abjadic scripts to phonetic alphabets is presented as a natural law of linguistic evolution. This perspective risks naturalizing what is actually a state-coercive reorganization. The engine's false summit detector should flag that beneficiaries exist (state apparatus, modernization elites) and that the constraint involves explicit enforcement, contradicting the natural law framing.
constraint_indexing:constraint_classification(generational_literacy_rupture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generational_literacy_rupture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(generational_literacy_rupture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(generational_literacy_rupture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(generational_literacy_rupture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(generational_literacy_rupture, TR),
    TR >= 0.70.

:- end_tests(generational_literacy_rupture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, declining over 30 years. At t=1 (immediate post-reform), extractiveness peaks at 0.65 — elderly cohort faces near-total literacy loss, religious institutions lose institutional pathways, administrative continuity is severed. By t=5, extractiveness drops to 0.58 as young generation grows up literate in new script and the accrued coordination benefit becomes real (unified national literacy). By t=30, extractiveness declines to 0.25 as the pre-reform generation ages out and new script becomes hegemonic normal. The trajectory reflects that extractiveness is highest where there is a cohort with alternative literacy options whose capital is devalued — once that cohort passes, the constraint becomes structural background. Suppression (0.72): High and stable. Suppression operates through state monopoly on education (no private Arabic-script schools permitted), legal status of scripts (only Latin-based script is official), religious institution capture (Quranic education is restricted), and eliminination of alternative knowledge pathways. The suppression is not circumstantial but structural — the state explicitly prohibits the old script in public/official contexts. This high suppression, combined with moderate extractiveness and moderate theater, keeps the classification as Tangled Rope rather than Snare (which would require ε ≥ 0.46 and χ ≥ 0.66). Theater ratio (0.48, rising to 0.68): Moderate and rising. The initial reform (t=0-1) is substantive — an actual change in official practice and institutional capacity. By t=5-30, the theater ratio rises as the reform increasingly functions as symbolic marker of modernization ('we are modern because we have Latin script') while the actual administrative and educational benefits plateau. The rising theater suggests Piton-like degradation: the modernization narrative becomes increasingly performative, maintained through institutional inertia and official rhetoric rather than ongoing functional necessity. Mandatrophy is NOT resolved — the constraint remains a hybrid mixing genuine coordination (national literacy unification) with significant extraction (knowledge system rupture, institutional capture), and the trajectory shows increasing performance masking declining functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   Extreme perspectival divergence across power/exit positions. The young student (moderate/mobile) experiences pure coordination — they acquire the new script naturally and enjoy unified literacy benefits. The beneficiary state apparatus (institutional/arbitrage) experiences Tangled Rope — genuine coordination goal paired with extractive institutional consolidation. The elderly literate class (powerless/trapped) experiences Snare — maximum extraction with no exit. The religious scholars (moderate/trapped through institutional capture) experience Snare — their primary function (Quranic transmission) becomes institutionally inaccessible. The middle-aged cohort (moderate/constrained) experiences Tangled Rope — mixed cost (lost literacy capital) and benefit (future integration). The analytical observer risks seeing Mountain (natural law of linguistic evolution) but the structural data reveals a false summit: the beneficiaries and enforcement mechanisms indicate political contingency, not natural necessity. The perspectival range from pure coordination (young students) to pure extraction (elderly/religious) to natural law (analytical false summit) demonstrates why single-perspective analysis fails — the constraint simultaneously IS all three depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. The state apparatus receives d ≈ 0.15 (beneficiary + arbitrage = low d → negative χ). The young students receive d ≈ 0.45 (neither beneficiary nor victim, mobile exit = moderate d). The elderly literate class receives d ≈ 0.95 (victim + trapped = high d → high f(d) → high χ). Religious scholars receive d ≈ 0.90 (victim + institutional capture that prevents exit = very high d). The middle-aged cohort receives d ≈ 0.60 (victim-beneficiary mix + constrained = moderate-high d). The analytical observer receives d ≈ 0.72 (observer + analytical). The formula χ = ε × f(d) × σ(S) produces: state apparatus χ ≈ 0.58 × (-0.12) × 1.0 ≈ -0.07 (perceived as coordination); elderly χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (perceived as severe extraction); young students χ ≈ 0.58 × 0.65 × 1.0 ≈ 0.38 (perceived as mild coordination). The perspectival gap is mathematically encoded in the directionality derivation — same ε produces wildly different χ across perspectives because d values differ radically.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is NOT resolved in this constraint. The classification depends on the perspective: from the beneficiary state apparatus and young generation, the constraint appears genuinely coordinative (Rope, Tangled Rope with strong coordination function). From the elderly and religious scholars, the constraint appears purely extractive (Snare). From the analytical observer, it appears as natural law (Mountain, triggering FSM false summit detection). The mandatrophy is not resolved because all three readings are structurally defensible from their respective positions — each observer perceives real coordination or real extraction or real natural law depending on their power/exit position. The engine's resolution is perspectival: the constraint IS both extractive and coordinative simultaneously, depending on which victims and beneficiaries you measure. The unified national literacy is genuinely coordinative (Rope). The institutional capture and knowledge rupture are genuinely extractive (Snare). These are not errors or measurement uncertainty — they are structural facts about asymmetric constraints. No single type correctly represents all perspectives. The constraint's type MUST vary by index (P,T,E,S), which is precisely what the framework's perspectival structure captures. Mandatrophy 'resolution' here means accepting that Tangled Rope (mixed coordination and extraction) is the correct classification when averaging across perspectives, while acknowledging that the beneficiary perspective undersells the extraction and the victim perspective oversells the extraction. The time-based evolution shows extractiveness declining as the cohort with alternative literacy options ages out — by t=30, the constraint approaches pure Rope (unified literacy as standard, extraction component disappears as comparison class vanishes). This is not mandatrophy resolution but rather temporal migration from Tangled Rope (t=1) toward Rope (t=30).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    script_transition_cost_distribution,
    'What proportion of the measured extractiveness (0.58) represents inevitable transition friction versus state-engineered asymmetric burden?',
    'Comparative analysis: literacy transition policies in other countries (script shifts in post-Soviet states, romanization in Vietnam, China''s simplified character adoption). Measure cost distribution across age/class cohorts versus uniform transition overhead. Historical reconstruction of pre-reform debate: were less disruptive phasing schedules technically feasible but rejected for political reasons?',
    'If friction is largely inevitable: the constraint approaches Tangled Rope with genuine coordination function. If engineered: the constraint approaches Snare with instrumental extraction cloaked in modernization rhetoric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(script_transition_cost_distribution, empirical, 'Proportion of extractiveness due to transition necessity versus state design').

omega_variable(
    religious_knowledge_system_irreversibility,
    'Is the loss of Quranic Arabic script literacy truly irreversible, or could an educated minority have maintained parallel transmission?',
    'Historical counterfactual: countries that underwent script reform but maintained religious/scholarly minority literacy in original script (Malaysia, Indonesia, parts of post-Ottoman Levant). Were such dual pathways technically or institutionally viable in Turkey? Did state policy explicitly foreclose them?',
    'If genuinely irreversible: the constraint operates as a structural rupture (high suppression justified by transition necessity). If reversible but prohibited: the constraint''s suppression is politically chosen rather than technically inevitable, elevating the extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_knowledge_system_irreversibility, empirical, 'Whether religious minority literacy preservation was technically or politically foreclosed').

omega_variable(
    modernization_legitimacy_claim,
    'Is the state''s modernization benefit (unified literacy, European alignment) genuinely accrued, or is it a post-hoc legitimation narrative for coercive script replacement?',
    'Institutional analysis: Did unified literacy measurably improve state capacity before vs. after? Compare literacy rates, administrative efficiency, and education reach in Turkey post-1928 versus contemporaneous countries without script rupture. Distinguish correlation from causal attribution. Did the modernization gains require the specific 3-month enforcement timeline, or would gradual transition achieve the same outcomes with less extraction?',
    'If modernization gains are genuine and required rapid transition: the constraint is Tangled Rope with substantial coordination function justifying asymmetric cost. If gains are marginal or could be achieved gradually: the constraint is primarily extractive (Snare with modernization theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_legitimacy_claim, empirical, 'Whether modernization benefits justify the 3-month enforcement timeline').

omega_variable(
    institutional_capture_via_literacy,
    'Did the alphabet reform function as institutional capture — neutralizing Ottoman/Islamic institutional independence through literacy disruption?',
    'Institutional genealogy: Track religious institution capacity before vs. after reform. Measure: number of active Quranic schools, scholarly output in religious domains, institutional autonomy of religious authorities relative to state. Examine explicit state coordination: Did education ministry decisions to enforce Latin script in all settings preclude minority preservation?',
    'If institutional capture is demonstrable: the constraint is Tangled Rope operating asymmetrically against religious institutions (extraction disguised as modernization). If religious institutions adapted or maintained parallel structures: the constraint is less extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_via_literacy, empirical, 'Whether script reform functioned as institutional capture against religious authority').

omega_variable(
    intergenerational_knowledge_transmission_rupture,
    'Is the measured suppression (0.72) accurate for the knowledge transmission rupture, or does it under-represent the civilizational epistemic cost?',
    'Long-term cultural genealogy: What Ottoman/Islamic knowledge systems became inaccessible to the post-1928 generation? Medical treatises, philosophical commentaries, historical documents, scientific works in Arabic script — measure archival/library accessibility and scholarly engagement post-reform. Did the script shift create a genuine knowledge barrier, or primarily an inconvenience for specialists?',
    'If knowledge systems remain accessible to motivated scholars: suppression is moderate (0.72 may be accurate as institutional/career suppression). If entire knowledge domains became inaccessible: suppression approaches 0.85+ and extraction approaches 0.70+, reclassifying toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_knowledge_transmission_rupture, empirical, 'Magnitude of civilizational knowledge transmission rupture').

omega_variable(
    false_summit_naturalization,
    'Is the ''natural law'' perspective (mountain) genuinely describing linguistic evolution, or naturalizing a politically contingent choice?',
    'Comparative script history: Other major script transitions (Vietnam, Korea, post-Soviet states) — which were coercive state actions, which were organic evolution? Did those transitions show the same extraction/suppression signature? If coercive transitions universally show high extraction, the mountain perspective is naturalizing contingent political action as inevitable linguistic law.',
    'If linguistic evolution is a genuine pressure independent of state coercion: the mountain perspective has epistemic legitimacy. If script transitions are primarily state-driven: the mountain perspective is a false summit, masking institutional extraction as natural law. Triggers FSM engine evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether linguistic evolution or state coercion better explains script transitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generational_literacy_rupture, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gen_lit_theater_t0, generational_literacy_rupture, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gen_lit_theater_t1_reform, generational_literacy_rupture, theater_ratio, 1, 0.35).
narrative_ontology:measurement(gen_lit_theater_t5, generational_literacy_rupture, theater_ratio, 5, 0.48).
narrative_ontology:measurement(gen_lit_theater_t10, generational_literacy_rupture, theater_ratio, 10, 0.55).
narrative_ontology:measurement(gen_lit_theater_t20, generational_literacy_rupture, theater_ratio, 20, 0.62).
narrative_ontology:measurement(gen_lit_theater_t30, generational_literacy_rupture, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(gen_lit_extract_t0, generational_literacy_rupture, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gen_lit_extract_t1_reform, generational_literacy_rupture, base_extractiveness, 1, 0.65).
narrative_ontology:measurement(gen_lit_extract_t5, generational_literacy_rupture, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(gen_lit_extract_t10, generational_literacy_rupture, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(gen_lit_extract_t20, generational_literacy_rupture, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(gen_lit_extract_t30, generational_literacy_rupture, base_extractiveness, 30, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generational_literacy_rupture, identity_coordination).
narrative_ontology:affects_constraint(generational_literacy_rupture, ottoman_institutional_memory_loss).
narrative_ontology:affects_constraint(generational_literacy_rupture, quranic_transmission_pathway_closure).
narrative_ontology:affects_constraint(generational_literacy_rupture, state_cultural_authority_consolidation).

% DUAL FORMULATION NOTE:
% The generational literacy rupture is part of a constraint family around state modernization and institutional consolidation in post-Ottoman Turkey. The immediate constraint (alphabet reform itself) has its own ε (0.58); the upstream constraints (state consolidation project) and downstream constraints (effects on religious institution capacity, effects on administrative continuity) each have their own ε values. This story addresses the literacy rupture specifically. Upstream: state_modernization_apparatus (higher ε, extraction-heavy). Downstream: quranic_transmission_pathway_closure (higher ε, pure snare; decomposed from this story because the Quranic knowledge rupture is structurally distinct from the literacy transition itself, though causally dependent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(generational_literacy_rupture, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
