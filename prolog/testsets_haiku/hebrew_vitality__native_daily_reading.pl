% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Vernacular Vitality: Native Daily Use Only
 *   domain: sociolinguistic/cultural/political
 *
 * SUMMARY:
 *   This reading instantiates one interpretation of Hebrew vitality: ONLY
 *   native daily speech constitutes vitality; liturgical and learned Hebrew
 *   are preservation, not life. This is a reading of the contested kernel
 *   'hebrew_vitality', held by the Zionist institutional network and dominant
 *   in Israeli state ideology. It claims that Hebrew, to be truly 'alive' as
 *   a national language, must function as the mother tongue of daily secular
 *   life — spoken first by children in homes and schools, used without sacred
 *   context in commerce and governance. Under this reading, the 1,900-year
 *   liturgical and legal use of Hebrew becomes 'preserved heritage' rather
 *   than 'living language'. The constraint systematically enforces this
 *   distinction through educational policy, media prestige allocation, and
 *   institutional resource distribution. This reading conflicts directly with
 *   the liturgical_reading (which holds that unbroken prayer-use constitutes
 *   vitality) and creates structural downstream effects on the
 *   hybrid_continuity_reading (which argues both forms matter). The expected
 *   structural delta: moderate extractiveness (institutional enforcement
 *   required, lexical expansion and native-speaker population building
 *   demanded organized effort), clear beneficiary (Zionist state-building and
 *   secular Hebrew speakers), identifiable victim (liturgical tradition and
 *   ultra-orthodox communities). The claim/metric gap is deliberate: the
 *   constraint claims to be coordination (solving the problem of national
 *   language for a dispersed population) while the metrics reflect
 *   substantially extractive, actively suppressed operation (it subordinates
 *   one legitimate linguistic form to another).
 *
 * KEY AGENTS:
 *   - labor_zionist_institutional_network: agenda-setter (institutional power). Sets the definition of vitality through state education, labor movements, youth movements. Enforces it by allocating resources and prestige exclusively to vernacular speakers.
 *   - secular_hebrew_speaker_community: beneficiary (organized power). Achieves linguistic sovereignty; their speech becomes THE authoritative form. Educational and media resources flow to native speakers.
 *   - liturgical_hebrew_tradition: victim (moderate power, identity-locked). Desacralized and reclassified as 'preserved' rather than 'living'. Loses cultural primacy despite maintaining continuous multi-generational use.
 *   - ultra_orthodox_communities: payer and excluded (moderate power, identity-locked). Marginalized through institutional enforcement; their Hebrew variant becomes non-normative. Structurally excluded from definition-setting.
 *   - diaspora_jewish_communities: observer (moderate power, analytical). Maintain Hebrew through study and liturgy; the constraint positions their use as 'not vital'. Provide historical evidence but hold no seat in enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.62).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.71).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vernacular Vitality: Native Daily Use Only").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistic/cultural/political").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'b090133f-26bc-4a54-8e3d-84c205bafac3').
narrative_ontology:cs_kernel_codification('b090133f-26bc-4a54-8e3d-84c205bafac3', fixed_text).
narrative_ontology:cs_authority_grounding('b090133f-26bc-4a54-8e3d-84c205bafac3', extraction).
narrative_ontology:cs_interpretation_layer_present('b090133f-26bc-4a54-8e3d-84c205bafac3').
narrative_ontology:cs_reading_relation('b090133f-26bc-4a54-8e3d-84c205bafac3', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('b090133f-26bc-4a54-8e3d-84c205bafac3', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('b090133f-26bc-4a54-8e3d-84c205bafac3', foundational, native_speech_sole_vitality_criterion).
narrative_ontology:cs_axiom_status(native_speech_sole_vitality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('b090133f-26bc-4a54-8e3d-84c205bafac3', native_speech_sole_vitality_criterion, conventional).
narrative_ontology:cs_axiom('b090133f-26bc-4a54-8e3d-84c205bafac3', foundational, liturgical_use_constitutes_preservation_not_life).
narrative_ontology:cs_axiom_status(liturgical_use_constitutes_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('b090133f-26bc-4a54-8e3d-84c205bafac3', liturgical_use_constitutes_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('b090133f-26bc-4a54-8e3d-84c205bafac3', native_daily_vernacular_criterion).
narrative_ontology:cs_drift_state('b090133f-26bc-4a54-8e3d-84c205bafac3', contemporary_post_universal_native_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b090133f-26bc-4a54-8e3d-84c205bafac3', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, secular_hebrew_speaker_community).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, ultra_orthodox_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 (1880: early ideological framing, pre-institutional) to 0.62 (2025: fully institutionalized, asymmetric benefit consolidated). The trajectory tracks institutional embedding: before state formation, the constraint was a competing ideological claim among multiple Zionist visions; after 1948, it became state policy with enforcement machinery (education curriculum, broadcasting standards, immigration/settlement policies). Suppression is consistently higher than extractiveness (0.71 vs 0.62 at t=end) because the constraint persists by actively marginalizing alternatives — suppression defends the definition itself, not just the benefits. Theater ratio rises modestly (0.08 to 0.28) because as native speech became universal among secular-majority population by 1970, the constraint's active enforcement became less necessary for coordination and increasingly performative (defending the boundary between 'living' and 'preserved' when the population balance had already shifted). The measurement series is aligned on one grid: every metric is authored at every shared time point (1880, 1920, 1948, 1970, 1995, 2025), enabling lifecycle drift analysis. Early measurements (1880-1920) are marked as 'projected' because they represent the ideology pre-institutionalization; post-1920 are 'observed' from educational policy, census linguistic data, and media presence.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (labor_zionist network) experiences this as genuine coordination: they solved a real problem of national language formation for a dispersed population. The constraint's framing as 'vitality' is the truth as experienced from their institutional seat. The victims (liturgical tradition and ultra-orthodox communities) experience it as enforced subordination: their Hebrew use was continuous, sacred, and living; the constraint's reclassification as 'preserved' is an institutional act of desacralization, not a discovery. The boundary marker ('native daily speech') appears natural from the beneficiary seat (it is how they speak) and appears arbitrary from the victim seat (it excludes forms that have been continuously regenerated across centuries). The engine computes directionality per seat: the zionist network gets low d (beneficiary, arbitrage exit); secular speakers get low d (beneficiary, mobile exit); liturgical tradition and ultra-orthodox get high d (victims, identity-locked exit). This structural divergence IS the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The zionist_institutional_network is the primary beneficiary and agenda-setter — it collects legitimacy, institutional resources, and state power from the constraint. Its directionality should compute near full-beneficiary (~0.1-0.2 on d scale) because it sets the rules, has arbitrage exit (could change the definition but chooses not to), and captures the primary extraction (institutional prestige). The secular_hebrew_speaker_community benefits (linguistic sovereignty, prestige, educational investment) but does not run the constraint — it is more accurately a beneficiary than agenda-setter, with moderate power and mobile exit options, placing d around 0.25-0.35. The liturgical_hebrew_tradition is a victim with high extraction cost (desacralization, resource starvation, institutional marginalization) and identity-locked exit (cannot simply switch linguistic traditions) — d should be high, 0.75-0.85. Ultra-orthodox communities are payers (pay the cost of marginalization) and excluded from enforcement decisions — d should be high, 0.70-0.80. Derivation from beneficiary/victim declarations should produce these values; no override needed if the structural data is declared cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to build a native-speaker Hebrew population fast enough for state formation) was genuine and acute in 1880-1948. By 1948-1960, vernacular Hebrew was native for the majority population; the problem was substantially solved. The constraint persists, but the mandatrophy question asks: does the contemporary enforcement serve the original function or has the function atrophied? The theater ratio trajectory provides diagnostic evidence: theater_ratio rises from 0.16 (1948, when coordination was still being built) to 0.28 (2025, when vernacular native speech is universal in secular domains and enforcement is increasingly about maintaining the boundary between categories rather than solving a language shortage). The constraint has NOT fully degraded to piton status (theater_ratio is still below 0.5, and suppression remains high — the constraint actively prevents alternative framings, not merely maintained theatrically). But the rising theater trajectory and stable suppression (high and flat, not rising as it would if enforcement were fighting increasing resistance) suggests PARTIAL mandatrophy: the constraint solves a smaller problem now than it did in 1948, but institutional inertia and the benefit to established beneficiaries keep it enforced. The committer frame clarifies the functional shift: this reading coexists with the liturgical_reading and influences the hybrid_continuity_reading. If the hybrid_reading became ascendant (arguing both forms are vital), this reading would lose its basis for the suppression it currently requires. The mandate (native speech as the singular criterion for vitality) has not disappeared, but its operational necessity has diminished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_vs_constructed_vitality,
    'Is the constraint''s definition of vitality (native daily speech) a natural discovery about what makes languages ''alive'', or is it a constructed institutional definition that benefits the Zionist project?',
    'Comparative sociolinguistic analysis: examine how other language revitalization movements (Irish, Basque, Welsh, Navajo) define vitality and whether they privilege native speech or allow multiple legitimate forms. Examine whether the constraint''s metrics (suppression, theater_ratio) spike upward when institutional pressure slackens, suggesting the vitality criterion requires enforcement rather than natural emergence.',
    'If vitality is natural/discovered, the constraint is a mountain (low extraction, no enforcement needed). If vitality is constructed/institutional, the constraint is tangled_rope or snare (high extraction, active suppression). The measurement trajectory (rising suppression from 1948 onward despite universal native speech adoption) suggests constructed institutional boundary maintenance rather than natural fact discovery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_vs_constructed_vitality, empirical, 'Whether vitality is natural linguistic property or constructed political definition.').

omega_variable(
    reading_foreclosure_mechanism,
    'Did the native_daily_reading foreclose the liturgical_reading, or do both readings persist despite institutional pressure from the native_daily interpretation?',
    'Institutional history: trace whether the Zionist institutions actively suppressed liturgical Hebrew claims (yes = foreclosure) or simply marginalized them while they persisted in protected niches (yes = coexistence with pressure). Examine whether ultra-orthodox and diaspora communities have maintained counter-claims to the vitality definition; if so, foreclosure is incomplete.',
    'If foreclosure is complete, the relation is ''forecloses'' (rare, precise). If the liturgical reading persists in smaller institutional spaces despite suppression, the relation is ''coexists_with'' (both held by different parties). Current evidence suggests coexistence with pressure — the liturgical reading is not foreclosed in principle, but its institutional access is severely constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanism, empirical, 'Whether native_daily reading logically forecloses or merely pressures the liturgical reading.').

omega_variable(
    suppression_as_internalized_identity,
    'Is the measured suppression (0.71) structural (institutional barriers, educational exclusion) or internalized (Hebrew speakers have absorbed the vitality criterion and experience liturgical Hebrew as not-fully-living)?',
    'Post-institutional-pressure measurement: in diaspora communities with weaker state enforcement of the vitality criterion, do Hebrew speakers still reproduce the native-daily prioritization, or do they recognize multiple forms as vital? A shift would indicate internalization; absence of shift would indicate structural dependence on institutional enforcement.',
    'If structural, removing institutional enforcement would recover alternative framings. If internalized, the suppression persists even after enforcement weakens — the target population has adopted the constraint''s framing as self-evident truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_internalized_identity, empirical, 'Whether suppression mechanism is structural enforcement or internalized belief.').

omega_variable(
    victim_identity_lock_depth,
    'Is the identity-lock status of ultra-orthodox communities a historical feature (they cannot exit because Hebrew is their ancestral tradition) or an artifact of the constraint''s enforcement (they cannot exit because doing so would require abandoning religious observance)?',
    'Counterfactual: if the constraint were removed and multiple Hebrew forms were recognized as equally vital, would ultra-orthodox communities still experience identity-lock, or would they regain agency over their linguistic practice?',
    'If the lock is historical, the constraint identifies and leverages pre-existing vulnerability. If the lock is artifact, the constraint creates the victim status by narrowing the exit space that previously existed (where liturgical Hebrew was a legitimate national form). This distinction affects whether the constraint is ''discovering'' victims or ''making'' them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_identity_lock_depth, conceptual, 'Whether identity-lock is pre-existing vulnerability or constraint-induced closure.').

omega_variable(
    measuring_vitality_under_contestation,
    'Is the constraint''s definition of vitality the correct one, and therefore the institutional enforcement is justified enforcement of truth? Or is vitality itself defined by the constraint, making the enforcement a circular legitimacy claim?',
    'This is a conceptual question with no empirical resolution, but the committer frame makes it explicit: the native_daily_reading CONSTITUTES a claim about vitality; it does not discover vitality. The constraint defines the category it then claims to enforce.',
    'This is the ε-invariance question for the kernel: different readings assign different ε to the same linguistic phenomenon (Hebrew). The resolution is not to find the ''true'' vitality but to recognize that vitality is multiply instantiated by different readings, each with different structural beneficiaries and victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measuring_vitality_under_contestation, conceptual, 'Whether vitality is a pre-institutional fact or defined into existence by the constraint reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1880, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__native_daily_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_vitality__native_daily_reading, theater_ratio, 1948, 0.16).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_vitality__native_daily_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(hebr_tr_t1995, hebrew_vitality__native_daily_reading, theater_ratio, 1995, 0.26).
narrative_ontology:measurement(hebr_tr_t2025, hebrew_vitality__native_daily_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__native_daily_reading, base_extractiveness, 1880, 0.18).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(hebr_be_t1948, hebrew_vitality__native_daily_reading, base_extractiveness, 1948, 0.51).
narrative_ontology:measurement(hebr_be_t1970, hebrew_vitality__native_daily_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(hebr_be_t1995, hebrew_vitality__native_daily_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(hebr_be_t2025, hebrew_vitality__native_daily_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_vitality__native_daily_reading, suppression_requirement, 1880, 0.42).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.54).
narrative_ontology:measurement(hebr_su_t1948, hebrew_vitality__native_daily_reading, suppression_requirement, 1948, 0.64).
narrative_ontology:measurement(hebr_su_t1970, hebrew_vitality__native_daily_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(hebr_su_t1995, hebrew_vitality__native_daily_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(hebr_su_t2025, hebrew_vitality__native_daily_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__native_daily_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The kernel 'hebrew_vitality' decomposes into three structurally distinct constraint stories, one for each reading. This story (native_daily_reading) claims native-speaker daily use as the sole criterion for vitality and treats liturgical/study-based Hebrew as preservation. The sibling stories (liturgical_reading, hybrid_continuity_reading) instantiate different claims about what counts as vitality. ε-invariance rule: these are three different constraints because they define the observed phenomenon (Hebrew's status as 'living' or 'preserved') differently. A constraint whose ε depends on which reading is adopted is not one constraint with an observer-dependent parameter — it is multiple constraints. Each has its own ε (moderate for this reading, low for liturgical, moderate-low for hybrid), its own beneficiary structure (zionist project here, conservative institutions in liturgical reading), and its own type. All three are linked via network.affects_constraints to enable contamination propagation analysis. The upstream reading (liturgical_reading) has lower empirical contestation and longer institutional continuity; this reading (native_daily) is downstream, more extractive, more recently institutionalized, and dependent on active enforcement. The hybrid_reading sits downstream of both, attempting synthesis under pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_vitality__native_daily_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
