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
 *   constraint_id: maliki_reading
 *   human_readable: Medinan Practice (Amal Ahl al-Madina) as Jurisprudential Authority
 *   domain: islamic_jurisprudence/legal_theory
 *
 * SUMMARY:
 *   The Maliki jurisprudential school's elevation of amal ahl al-Madina (the
 *   living practice of Medina's community) as an independent source of law
 *   creates a distinctive methodological commitment that has shaped Islamic
 *   legal reasoning for over a millennium. This reading instantiates one of
 *   four major jurisprudential approaches in Sunni Islam, each grounded in
 *   different sources hierarchy and each with its own beneficiaries and
 *   victims. The constraint is a Tangled Rope from the base perspective: it
 *   genuinely coordinates jurisprudential reasoning by anchoring abstract
 *   legal methodology in lived communal practice (preventing the abstraction
 *   trap), while simultaneously extracting through methodological asymmetry
 *   (elevating Medinan practices as authoritative while subordinating
 *   non-Medinan regional customs to external judgment). The theater ratio is
 *   moderate (0.38) and has risen slightly over the centuries as the
 *   functional role of amal in jurisprudential reasoning has diminished while
 *   its institutional invocation as a school-differentiating marker has
 *   persisted.
 *
 * KEY AGENTS:
 *   - Maliki School Scholars: Institutional beneficiaries (institutional/arbitrage) — gain methodological flexibility and institutional prestige from elevation of amal as source category
 *   - Medinese Community Practitioners: Regional beneficiary-coordinators (organized/constrained) — their practices are legitimized as source-level while also being subordinated to Maliki school systematization
 *   - Non-Medinan Regional Communities: Victims (powerless/identity_locked) — their customary practices are judged against an external standard (Medinan exemplar) rather than standing on independent jurisprudential foundation; identity-fused with established practices
 *   - Inter-School Convergence Movement: Organized agents (organized/constrained) — seeking to integrate methodological divergences; see amal as temporary emphasis that will dissolve into comparative framework
 *   - Islamic Jurisprudential System: Institutional actor (institutional/constrained) — maintains the constraint through institutional inertia; the functional role has diminished but school identity depends on preserving the distinction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing methodological choice as structural necessity of law itself
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
narrative_ontology:human_readable(maliki_reading, "Medinan Practice (Amal Ahl al-Madina) as Jurisprudential Authority").
narrative_ontology:topic_domain(maliki_reading, "islamic_jurisprudence/legal_theory").

domain_priors:requires_active_enforcement(maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maliki_reading, '4ab57ad7-cea1-486c-92b8-4d6387cfce4a').
narrative_ontology:cs_kernel_codification('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', fixed_text).
narrative_ontology:cs_authority_grounding('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', lineage).
narrative_ontology:cs_interpretation_layer_present('4ab57ad7-cea1-486c-92b8-4d6387cfce4a').
narrative_ontology:cs_reading_relation('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', maliki_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', maliki_reading__shafi_i_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', maliki_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', foundational, medinan_practice_prophetic_authenticity).
narrative_ontology:cs_axiom_status(medinan_practice_prophetic_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', medinan_practice_prophetic_authenticity, empirically_contingent).
narrative_ontology:cs_axiom('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', foundational, community_continuous_legitimacy).
narrative_ontology:cs_axiom_status(community_continuous_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', community_continuous_legitimacy, deontological).
narrative_ontology:cs_reference_frame('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', prophetic_medina_precedent_transmission).
narrative_ontology:cs_drift_state('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', contemporary_hadith_critical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4ab57ad7-cea1-486c-92b8-4d6387cfce4a', '2026-02-26T14:32:17Z').
narrative_ontology:cs_kernel_id(maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maliki_reading, maliki_school_scholars).
narrative_ontology:constraint_beneficiary(maliki_reading, medinese_community_practices).
narrative_ontology:constraint_victim(maliki_reading, alternative_methodological_positions).
narrative_ontology:constraint_victim(maliki_reading, non_medinan_regional_practices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-MEDINAN REGIONAL COMMUNITIES (SNARE) — Structurally mobile (could adopt alternative methodological frameworks) but identity-fused with established regional practices. The Maliki elevation of Medinan practice as source-level authority directly marginalizes their own customary rulings unless those practices happen to align with Medinan precedent. Extraction occurs through methodological subordination: their practices are judged against an external standard (Medinan exemplar) rather than standing on their own jurisprudential foundation. No meaningful exit without identity rupture.
constraint_indexing:constraint_classification(maliki_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: MEDINESE COMMUNITY PRACTITIONERS (TANGLED ROPE) — Genuine coordination function: amal grounds practical jurisprudence in lived communal experience, avoiding the abstraction trap of purely textual reasoning. Real coordination benefit. But also extractive: the Maliki method privileges their own regional continuity as the standard against which all other regions are measured. They coordinate internally while setting terms for all external comparisons. Constrained exit because leaving the framework means losing the legitimacy that flows from continuity with the Prophet's city.
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MALIKI SCHOOL SCHOLARS (ROPE) — Primary beneficiaries. The elevation of amal ahl al-Madina as a methodological source-position creates institutional prestige and scholarly authority. Maliki jurists can cite Medinan practice as an independent evidentiary category, giving them argumentative flexibility unavailable to schools that privilege text alone or consensus. They experience the constraint as coordination: it enables coherent jurisprudential reasoning and institutional continuity. Low experienced extraction because they have full arbitrage — they can adopt alternative methodological frameworks if competitive advantage shifts.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-SCHOOL CONVERGENCE (SCAFFOLD) — Organized actors working toward methodological synthesis across all four schools see amal ahl al-Madina as a temporary methodological emphasis that can be integrated into a larger comparative jurisprudential framework. The sunset is explicit: as inter-school dialogue matures and methodological eclecticism becomes institutionalized, the need for school-specific source hierarchies diminishes. Sunset trajectory: 2-3 generations for institutional implementation.
constraint_indexing:constraint_classification(maliki_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INSTITUTIONAL PITON (MALIKI SCHOOL MAINTENANCE) — The explicit invocation of amal ahl al-Madina as a differentiating methodological marker persists largely as institutional theater: a way to maintain Maliki school identity and distinguish from other schools. The functional role of Medinan practice in actual jurisprudential reasoning has diminished as textual analysis (Quran, hadith) and explicit reasoning (qiyas) have become dominant across all schools. The practice persists through institutional inertia — to abandon it would be to dissolve a key marker of school identity, even though its generative role in jurisprudence has atrophied. Theater ratio is moderate (0.38) because the practice still carries some functional weight in historical justification and regional continuity narratives.
constraint_indexing:constraint_classification(maliki_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (MOUNTAIN) — From a universal/civilizational perspective, some reliance on living community practice as a legitimacy source is inherent to any functional legal system: written law alone cannot address all cases, and communities must ground jurisprudence in their own continuity and collective memory. The Maliki elevation of Medinan practice might appear as a structural necessity of how law operates in human societies. However, the structural data contradicts this — the constraint exhibits real asymmetries (beneficiaries, victims, active enforcement) that the mountain classification misses. The engine will compute this as a false summit: the 'necessity to jurisprudence' frame naturalizes what is actually a specific methodological choice with distributional consequences.
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
    constraint_indexing:constraint_classification(maliki_reading, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.35): Moderate. The Maliki method coordinates jurisprudential reasoning by grounding it in communal practice, avoiding purely textual abstraction — this is genuine coordination benefit. But it also extracts through asymmetry: Medinan practices are elevated to source-level authority while non-Medinan practices must justify themselves against this external standard. The extraction has risen slightly (0.28→0.35) as jurisprudential specialization has increased the stakes of methodological positioning. Suppression (0.42): Moderate. Non-Medinan communities face barriers (not insuperable, but real) in asserting alternative jurisprudential foundations. The barriers are partly material (institutional recognition, scholarly attention) and partly internalized (identity fusion with existing practices). Suppression has remained stable as the institutional hierarchy supporting Maliki authority has remained consistent. Theater ratio (0.38): Moderate and rising. Early Maliki jurisprudence (t=0) had stronger functional reliance on amal reasoning; contemporary Maliki jurisprudence relies more on hadith-based analogy and explicit qiyas. The explicit invocation of amal as a methodological marker persists (theater) even as its generative role in actual reasoning has diminished. This trajectory predicts eventual piton classification if the trend continues.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival fragmentation that characterizes kernel readings. The Maliki school scholars see coordination and institutional benefit (Rope). Non-Medinan communities see methodological subordination and identity lock (Snare). The Medinese practitioners see mixed benefit and extraction (Tangled Rope). Organized convergence movements see a temporary methodological emphasis with a sunset path (Scaffold). Institutional Maliki identity maintenance sees a degraded but inertia-preserved marker (Piton). The analytical civilizational observer risks misclassifying the whole thing as a natural feature of how jurisprudence must work (false-summit Mountain). The perspectival gap reveals that the constraint's classification depends entirely on the observer's structural position: beneficiary versus victim, institutional power versus powerless, arbitrage options versus trapped identity. No single type is 'correct' — the constraint is all six types simultaneously, experienced differently from each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position and relationship to the amal-as-source commitment. Maliki scholars (institutional/arbitrage) derive d toward beneficiary end because they have methodological flexibility and gain prestige — their exit cost is low (they could adopt other methods if incentives shifted). Non-Medinan communities (powerless/identity_locked) derive d toward extraction end because their own practices are subordinated and they cannot exit without identity rupture — their exit cost is prohibitive. Medinese practitioners (organized/constrained) occupy middle ground: they benefit from legitimization but cannot fully exit the framework without losing the prestige that flows from continuity with the Prophet's city. The engine's derivation chain (beneficiary/victim declarations + exit options → d → χ) produces the observed perspectival distribution without requiring manual directionality tuning.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: This constraint avoids the mandatrophy by being explicit about its contested nature. The Maliki reading does not claim universal jurisprudential truth but rather one legitimate emphasis within the larger Islamic legal tradition. The mandate is clear: ground jurisprudence in Medinan communal practice as a source-level category. The mandate has not outlived its function — it remains institutionally active and generatively important in Maliki jurisprudence, even as its practical weight has diminished. The key is that the constraint is authored as a reading of a kernel (jurisprudential method), not as a universal truth. This grounds the analysis in legitimate competing commitments rather than forcing resolution of an undecidable question. The mandatrophy would arise if one tried to claim that amal ahl al-Madina is the only legitimate jurisprudential method — that position would eventually collide with the institutional reality of competing schools. By authoring it as a reading, the constraint acknowledges the reality of competing positions from the start.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_authenticity_boundary,
    'What constitutes authentic Medinan practice (amal) versus later scholarly reconstruction or misattribution to the Medinan community?',
    'Hadith chain analysis; comparison of early Maliki texts with transmitted practice descriptions; identification of practices attested in pre-Maliki sources versus later Maliki compilations',
    'If Medinan practice is largely reconstructed or attributed: the constraint''s legitimacy chain is weakened — beneficiaries are reading backward from desired conclusions rather than forward from historical data. If authenticity is high: the constraint''s empirical grounding is stronger but the false-summit risk remains (even authentic practice does not establish it as universally binding source).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_authenticity_boundary, empirical, 'Degree of historical authenticity of transmitted Medinan practices').

omega_variable(
    amal_versus_qiyas_functional_replacement,
    'To what extent has explicit reasoning (qiyas) and hadith-based analogy functionally replaced amal ahl al-Madina in actual Maliki jurisprudential reasoning?',
    'Content analysis of Maliki fatwas and jurisprudential treatises across three centuries; comparison of frequency and weight given to amal citations versus qiyas and hadith reasoning; identification of cases where amal would yield different rulings than the applied method',
    'If functionally replaced: piton classification confirmed — amal is institutional theater. If still generative: the constraint maintains real functional role in jurisprudence and should reclassify away from piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amal_versus_qiyas_functional_replacement, empirical, 'Functional displacement of amal by explicit reasoning methods').

omega_variable(
    kernel_reading_underdetermination,
    'Is the distinction between the Maliki reading (practice-centered) and the Hanafi reading (text-centered) based on fundamentally different conceptions of jurisprudential authority, or are they alternative emphases within a shared methodological space?',
    'Systematic comparison of foundational jurisprudential premises in Maliki and Hanafi usul texts; identification of whether either school denies the other''s method as illegitimate or merely deprioritizes it; analysis of actual jurisprudential convergence over time',
    'If fundamentally different: the readings genuinely foreclose each other (though they coexist institutionally). If shared methodological space: the readings coexist and merely reflect different weighting of agreed-upon sources. This omega resolves the kernel structure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether Maliki and Hanafi readings represent fundamentally different jurisprudential conceptions or alternative emphases').

omega_variable(
    regional_practice_legitimacy_basis,
    'Does elevation of Medinan practice as a source rest on the empirical claim that Medinan practice uniquely preserves Prophetic precedent (a contingent historical fact), or on a deontological claim about the special legitimacy of the Prophet''s city (a normative commitment)?',
    'Historical analysis of Medinan practice continuity through early Islamic centuries; identification of cases where Medinan practice diverges from Prophetic precedent; textual analysis of Maliki justifications for why Medinan practice carries authority',
    'If empirical: the constraint is vulnerable to empirical falsification (if Medinan practice is shown to diverge from Prophetic example or to have been disrupted in transmission). If deontological: the constraint''s authority rests on normative commitment to the city''s special status regardless of empirical correspondence. This resolves how the constraint responds to historical challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_practice_legitimacy_basis, empirical, 'Whether Medinan practice authority rests on empirical continuity or deontological status').

omega_variable(
    methodological_asymmetry_intentional,
    'Is the Maliki asymmetry (Medinan practices elevate outward, non-Medinan practices must justify upward) a deliberate methodological choice to weight regional authenticity, or an artifact of historical transmission and later rationalization?',
    'Chronological analysis of when amal ahl al-Madina becomes explicit as a methodological category versus when regional asymmetries are instituted; comparison of early Maliki texts with later codifications; identification of alternative methodological framings that Maliki jurists considered and rejected',
    'If deliberate: the extractive asymmetry is a known feature of the method that beneficiaries chose. If artifact: the asymmetry emerged through institutional path-dependence and may be more vulnerable to conscious revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_asymmetry_intentional, empirical, 'Whether the Medinan methodological asymmetry was deliberately designed or emerged through transmission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maliki_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(malik_theater_t0, maliki_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(malik_theater_t5, maliki_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(malik_theater_t10, maliki_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(malik_extract_t0, maliki_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(malik_extract_t3, maliki_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(malik_extract_t6, maliki_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(malik_extract_t10, maliki_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(malik_supp_t0, maliki_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(malik_supp_t5, maliki_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(malik_supp_t10, maliki_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maliki_reading, 0.1).
narrative_ontology:affects_constraint(maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(maliki_reading, shafi_i_reading).
narrative_ontology:affects_constraint(maliki_reading, hanbali_reading).
narrative_ontology:affects_constraint(maliki_reading, islamic_consensus_ijma_authority).
narrative_ontology:affects_constraint(maliki_reading, qiyas_analogical_reasoning).

% DUAL FORMULATION NOTE:
% The jurisprudential method kernel decomposes into four separate reading constraints: maliki_reading, hanafi_reading, shafi_i_reading, hanbali_reading. Each reading has its own ε value reflecting the empirical distribution and institutionalization of that method's practice. All four readings share the same kernel (the question of methodological authority) but produce different beneficiary/victim structures because they privilege different sources and thus create different asymmetries. The four constraints are linked by network.affects_constraints edges in both directions — each reading's institutionalization influences the others' scope and deployment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maliki_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
