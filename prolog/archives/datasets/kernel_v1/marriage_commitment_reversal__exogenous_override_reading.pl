% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Marriage Commitment Reversal by Federal Coercion (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The Latter-day Saint (LDS) Church's public practice reversal on plural
 *   marriage in 1890 (Official Declaration 1, the 'Manifesto') occurred under
 *   explicit federal coercion. The Edmunds Act (1862), Edmunds Act (1882),
 *   and Edmunds-Tucker Act (1887) progressively tightened federal
 *   restrictions: property seizure of the church, legal disability of members
 *   engaging in polygamy, and most critically, the conditioning of statehood
 *   for the Territory of Utah on abandonment of polygamous practice. The LDS
 *   leadership, under President Wilford Woodruff, issued the Manifesto on
 *   September 24, 1890, declaring the discontinuance of plural marriage. This
 *   reading frames the reversal as externally coerced institutional
 *   compliance while internal doctrine (Section 132 of the Doctrine and
 *   Covenants, which prescribes plural marriage as a celestial principle) was
 *   preserved unchanged. The doctrine-practice gap persists: Section 132
 *   remains part of LDS canonical scripture to the present day, taught in
 *   theology courses, cited in temple ceremonies, and preserved as doctrinal
 *   principle, while the lived practice is publicly prohibited. This reading
 *   contrasts with the 'endogenous reinterpretation' reading, which
 *   attributes the reversal to Woodruff's divine vision reinterpreting God's
 *   will under changed circumstances, and the 'practice-doctrine gap'
 *   reading, which treats the gap itself as the primary constraint. The
 *   exogenous override reading emphasizes that the constraint is federal
 *   extraction of institutional sovereignty through coercive threat without
 *   theological reconciliation.
 *
 * KEY AGENTS:
 *   - Plural marriage practitioners: Primary victims (powerless/trapped) — face legal penalties, property seizure, and institutional exclusion if continuing practice. No structural exit available within federal jurisdiction.
 *   - LDS Institution: Organized victim (organized/constrained) — suffers institutional subordination, forced practice reversal, threat of territorial denial and property confiscation. Constrained rather than trapped because the institution retains some negotiating capacity and can theoretically flee federal jurisdiction.
 *   - Federal Government: Primary beneficiary (institutional/arbitrage) — extracts institutional compliance, territorial control, and legal subordination. Experiences the constraint as coordination mechanism from its perspective.
 *   - Wilford Woodruff & LDS Leadership: Mediating institutional actors (institutional/constrained) — navigating between federal coercion and internal doctrinal integrity. The decision to preserve Section 132 while reversing practice suggests a compromise structure.
 *   - Section 132 Doctrine: Institutional artifact (institutional/arbitrage in piton form) — preserved through inertia and symbolic authority, but functionally atrophied.
 *   - Political theology analyst: Observing agent (analytical/analytical) — capable of seeing both the coordination (federal-church settlement) and the asymmetric extraction (federal dominance) simultaneously.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.68).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Marriage Commitment Reversal by Federal Coercion (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '839e4ec5-b08d-4165-98ca-8e03a3f0332a').
narrative_ontology:cs_kernel_codification('839e4ec5-b08d-4165-98ca-8e03a3f0332a', fixed_text).
narrative_ontology:cs_authority_grounding('839e4ec5-b08d-4165-98ca-8e03a3f0332a', extraction).
narrative_ontology:cs_interpretation_layer_present('839e4ec5-b08d-4165-98ca-8e03a3f0332a').
narrative_ontology:cs_reading_relation('839e4ec5-b08d-4165-98ca-8e03a3f0332a', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('839e4ec5-b08d-4165-98ca-8e03a3f0332a', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('839e4ec5-b08d-4165-98ca-8e03a3f0332a', foundational, federal_coercion_primary_driver).
narrative_ontology:cs_axiom_status(federal_coercion_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('839e4ec5-b08d-4165-98ca-8e03a3f0332a', federal_coercion_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('839e4ec5-b08d-4165-98ca-8e03a3f0332a', foundational, section_132_preservation_unrevised).
narrative_ontology:cs_axiom_status(section_132_preservation_unrevised, holdable).
narrative_ontology:cs_axiom_grounding('839e4ec5-b08d-4165-98ca-8e03a3f0332a', section_132_preservation_unrevised, empirically_contingent).
narrative_ontology:cs_reference_frame('839e4ec5-b08d-4165-98ca-8e03a3f0332a', federal_legal_supremacy_framework).
narrative_ontology:cs_drift_state('839e4ec5-b08d-4165-98ca-8e03a3f0332a', post_edmunds_tucker_act, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('839e4ec5-b08d-4165-98ca-8e03a3f0332a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_administration).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLURAL MARRIAGE PRACTITIONER (SNARE) — Structurally trapped by federal coercion (territorial statehood conditioned on abandoning practice). Cannot exit the constraint without abandoning a fundamental faith commitment or fleeing federal jurisdiction. The federal threat extracts institutional compliance while the internal doctrine (Section 132) remains unchanged. Maximum experienced extraction: the agent faces material coercion (legal penalties, property seizure, institutional exclusion) with no structural exit. The doctrine-practice gap creates a second extraction layer — the agent is forced to dissociate their lived practice from their stated principle.
constraint_indexing:constraint_classification(marriage_commitment_reversal__exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: LDS INSTITUTION AS VICTIM (SNARE) — Organized but severely constrained. Institutional sovereignty is extracted via the statehood threat: practice cessation becomes a condition for territorial recognition and legal standing. The institution cannot simply leave the federal system (constrained exit, not trapped). However, the extraction is severe — the institution is forced to publicly renounce a central doctrine-practice link while preserving the doctrine itself, creating institutional schizophrenia. The suppression mechanism is explicit and material: federal military pressure, property seizure of church holdings, legal disability of members.
constraint_indexing:constraint_classification(marriage_commitment_reversal__exogenous_override_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT (ROPE) — Experiences the constraint as coordination (settling the Utah Territory question, assimilating a dissident institutional power into the federal system). The federal actor benefits from institutional compliance and territorial control. From this perspective, the constraint is a coordination mechanism: the government communicates its legitimate demands (marriage law conformity), and the church responds by adapting practice. However, the extraction is real and severe — the government is extracting institutional autonomy and legal subordination. The perspective shifts depending on whether one views the federal actor as beneficiary or both parties as beneficiaries of a settlement. This perspective uses arbitrage exit (the government can threaten but also negotiate), producing moderate experienced extraction rather than snare-level.
constraint_indexing:constraint_classification(marriage_commitment_reversal__exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: POLITICAL THEOLOGY OBSERVER (TANGLED ROPE) — Sees the constraint as a coordination mechanism (both federal and LDS systems coordinating around a settlement term) combined with asymmetric extraction (federal dominance over religious autonomy). The LDS institution gains legal standing and territorial integration; the federal government gains compliance. However, the internal doctrine-practice gap remains unresolved — Section 132 is preserved but practice is suspended. This creates genuine coordination (both parties accept the external settlement) combined with asymmetric extraction (the church cannot revise its doctrine without fracturing). The powerful/mobile perspective reflects the theological analyst's capacity to see both the coordination and the extraction — neither party is powerless, both have some exit options, but the extraction is real.
constraint_indexing:constraint_classification(marriage_commitment_reversal__exogenous_override_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: SECTION 132 DOCTRINE (PITON) — From the civilizational scale, the preservation of Section 132 in official doctrine while practice is suspended represents a degraded institutional commitment. The doctrine persists through inertia and institutional memory, but its functional content has atrophied — it is no longer operative in the lived practice of the community. The piton classification reflects the high theater ratio: the doctrine is maintained (recited, preserved in canonical texts) but has lost its generative force. The perpetuation of Section 132 is partly performative — it preserves historical continuity and doctrinal consistency, but everyone understands that the practice it describes is not acceptable under the current federal regime. Theater is necessary to avoid institutional fracture (denying Section 132 would require admitting the doctrine was false, which would destabilize authority structures). The theater ratio (0.58) reflects that some functional content remains (the doctrine still shapes internal identity and spiritual narrative) but the operational content is nearly zero.
constraint_indexing:constraint_classification(marriage_commitment_reversal__exogenous_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this reading naturalizes the constraint as an immutable outcome of state sovereignty: federal authority over territorial law is a foundational principle of governance; religious authority cannot coexist with federal legal supremacy; therefore, the constraint is inevitable, natural, and unchangeable. The analytical observer might frame this as a law of institutional ecology: institutions subordinate to a superior sovereign lose autonomy over their internal practices when those practices violate the superior sovereign's law. However, this perspective risks falsely summiting — the constraint is not a natural law but a constructed institutional outcome driven by specific political choices, coercive capacity, and religious institutional weakness during the American territorial period.
constraint_indexing:constraint_classification(marriage_commitment_reversal__exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_commitment_reversal__exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_commitment_reversal__exogenous_override_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting federal extraction of institutional autonomy without internal doctrinal resolution. The exogenous override reading attributes the reversal directly to federal coercion (threat of permanent territorial denial and property seizure) rather than doctrinal evolution. The LDS leadership explicitly preserved Section 132 while reversing practice, indicating external compulsion rather than internal conviction. The trajectory shows rising extractiveness from 0.42 (initial federal pressure) to 0.68 (full institutional compliance and suppression) over 30 years. Suppression (0.72): High and rising. Federal suppression mechanisms escalated from legal restrictions (Anti-Bigamy Act 1862) through property seizure (Edmunds Act 1882) to quasi-military pressure (Edmunds-Tucker Act 1887, with federal marshals actively prosecuting polygamists). By 1920, the suppression was stable and comprehensive — no institution in the federal system dared openly practice plural marriage, and the LDS Church had internalized the suppressive regime. Theater ratio (0.58): Moderate-high and rising. The initial practice reversal had lower theater (genuine institutional uncertainty, active internal negotiation) but increased theater over time as Section 132 preservation became performative. By 1920, the doctrine was recited as principle while everyone understood it was inoperative. The institutional performance of doctrinal fidelity while practicing legal subordination is the piton mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits classical perspectival divergence. The plural marriage practitioner experiences pure extraction (snare) — they are forced to choose between federal law and faith commitment, with no structural exit. The LDS institution as a whole experiences extraction with some coordination benefits (snare, organized perspective) — the federal-church settlement enables statehood and legal recognition, but only by surrendering institutional autonomy over marriage law. The federal government experiences coordination (rope) — the settlement mechanism solves the persistent problem of Utah territorial governance by extracting institutional compliance. The political theology observer sees both: genuine coordination (federal-church settlement terms) layered over asymmetric extraction (federal dominance, church subordination). The piton perspective observes that Section 132 is preserved but operationally degraded — the doctrine persists through institutional inertia rather than active commitment. The false summit (mountain perspective) naturalizes the constraint as inevitable state sovereignty, obscuring the constructed nature of the federal-church power dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the agent's structural position relative to the constraint. Federal government: beneficiary with arbitrage exit options (can exit at cost of losing institutional dominance, but chooses to maintain pressure). Derived d ≈ 0.10, producing negative chi (the government benefits). LDS institution: victim with constrained exit (cannot exit without losing statehood, legal standing, and territorial resources, but has some negotiating capacity). Derived d ≈ 0.75, producing high chi (the institution bears extraction). Plural marriage practitioners: victims with trapped exit (cannot exit without abandoning faith commitment or fleeing federal jurisdiction). Derived d ≈ 0.92, producing maximum chi (maximum experienced extraction). These directionality differences explain why the snare classification is correct from the victim perspectives while the rope classification appears from the beneficiary perspective — the same constraint structure produces different experienced extractiveness based on the agent's exit capacity and relationship to the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is resolved by recognizing that the constraint is snare-level pure extraction from the victim perspectives (practitioners, institution) but appears as rope-level coordination from the federal beneficiary perspective. The federal actor genuinely solves a coordination problem (Utah territorial governance, legal uniformity across states) but does so through asymmetric extraction. The exogenous override reading emphasizes that this is not benign coordination — the federal actor extracts institutional autonomy, suppresses religious practice, and threatens institutional dissolution unless the church capitulates. The preservation of Section 132 without internal revision reveals that the church did not adopt federal norms as a matter of doctrine but as a matter of coercive pressure. The mandatrophy resolves into a clear snare classification when the analysis centers the powerless agent (the practitioner) or the victim institution, and a rope classification when it centers the federal beneficiary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_coercion_vs_voluntary_adoption,
    'Is the practice reversal driven by federal military threat and coercive capacity, or by LDS leadership''s voluntary adoption of federal norms after recognizing plural marriage as ethically untenable?',
    'Historical analysis of the immediate causation chain: sequence of federal threats (Anti-Bigamy Act 1862, Edmunds Act 1882, Edmunds-Tucker Act 1887), church leadership statements before and after the 1890 Manifesto, testimonies of decision-makers (Wilford Woodruff), institutional records documenting deliberation. If federal coercion is demonstrably the primary causal driver (rather than doctrinal realization), the exogenous override reading is confirmed. If leadership was moving toward doctrinal rejection anyway, the endogenous reinterpretation reading becomes stronger.',
    'If exogenous coercion is primary: Snare classification confirmed; extractiveness ≥0.65; the constraint is institutional subordination without doctrinal resolution. If endogenous reinterpretation is primary: classification shifts toward Tangled Rope (coordination + extraction both present); extractiveness may fall to 0.50-0.60 range (less purely extractive); the reading decomposition into three readings is more problematic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_coercion_vs_voluntary_adoption, empirical, 'Primary causal driver of practice reversal: federal coercion vs. internal doctrinal realization').

omega_variable(
    doctrine_practice_gap_sustainability,
    'How long can an institution maintain a foundational doctrine while publicly renouncing and discontinuing the practice that doctrine prescribes? Is the gap sustainable indefinitely (piton reading) or does it create internal pressure toward either doctrinal revision or practice resumption?',
    'Longitudinal analysis of LDS institutional discourse over 100+ years post-1890: (1) frequency and framing of Section 132 references in canonical texts, sermons, theological training; (2) evidence of internal debate or hidden practice (fundamentalist schism, temple theology, historical scholarship); (3) institutional stability/instability correlated with doctrine-practice gap salience. If gap persists as stable institutional feature with degraded functional content, piton reading is confirmed. If internal pressure mounts toward either reconciliation or revision, gap is temporally bounded.',
    'If sustainable (piton confirmed): institutional theater provides sufficient coherence; constraint may persist indefinitely. If pressure mounts: either doctrine will eventually be officially revised (invalidating exogenous override reading''s core claim that Section 132 is preserved), or practice will resume (falsifying the claim that federal coercion produced stable reversal). Temporal forecast: endogenous reinterpretation reading becomes increasingly defensible as doctrine-practice gap persists and internal theological work on reconciliation accumulates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap_sustainability, empirical, 'Sustainability of the doctrine-practice gap over generations').

omega_variable(
    external_coercion_necessity,
    'Given the stated federal threats (statehood conditioned on marriage law compliance), was institutional compliance with the practice ban actually coerced (no feasible alternative) or merely incentivized (compliance rewarded with statehood, non-compliance tolerated at cost)?',
    'Counterfactual analysis: if the church had refused federal demands and maintained plural marriage practice, what would have happened? (a) Federal military occupation, forced dissolution of the institution, property seizure, mass incarceration of members. (b) Indefinite territorial denial, but church autonomy preserved in isolated regions, members tolerated as marginal dissident community. If (a): coercion is genuine; the constraint is snare-level. If (b): the threat was real but compliance was incentivized rather than coerced; exit option is constrained rather than trapped; classification shifts toward Tangled Rope.',
    'If coercion is genuine (a): Snare classification confirmed; extractiveness ≥0.65; the institutional victim has no feasible exit. If incentivized (b): classification should shift toward Tangled Rope; extractiveness 0.50-0.60; the institution retained some agency in the settlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_coercion_necessity, empirical, 'Whether federal threat produced coercion (no feasible alternative) or incentivization (rewarded compliance)').

omega_variable(
    section_132_preservation_intentionality,
    'Is the continued official preservation of Section 132 in LDS canon (D&C 132) intentional doctrinal retention pending future divine clarification, or administrative inertia (the text is preserved because removing it would require formal authority decision)?',
    'Analysis of institutional statements, theological justifications, and leadership communications regarding Section 132: (1) Is the preservation framed as temporary (awaiting restoration) or permanent (doctrine transcends current practice)? (2) Is the text taught in official LDS religious education or mentioned only in historical contexts? (3) Have church leaders ever formally explained why the doctrine is preserved if the practice is prohibited? If intentional retention: the doctrine is preserved as principle, supporting exogenous override reading. If inertia: the preservation is incidental to the constraint''s structure, and the piton reading (degraded doctrine) becomes primary.',
    'If intentional: exogenous override reading is strengthened; the church retains the doctrine as unresolved principle, maintaining the doctrine-practice gap as theologically coherent (if paradoxical). If inertia: piton reading becomes primary; Section 132 is maintained through institutional momentum rather than active commitment; the exogenous override reading''s claim that doctrine is preserved becomes weaker (it is merely not-yet-removed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(section_132_preservation_intentionality, conceptual, 'Whether Section 132 preservation is intentional doctrinal retention or administrative inertia').

omega_variable(
    reading_epistemic_underdetermination,
    'Which reading of the contested kernel (exogenous override, endogenous reinterpretation, or practice-doctrine gap) best captures the actual institutional structure, and what evidence would definitively distinguish them?',
    'The three readings coexist in institutional interpretation. Exogenous override emphasizes federal coercion as primary driver. Endogenous reinterpretation emphasizes Woodruff''s revelation as primary driver (treating the federal threat as occasion rather than cause). Practice-doctrine gap emphasizes the structural ambiguity itself as the constraint. Resolution depends on: (1) primary causation analysis (omegas 1); (2) doctrine-practice gap sustainability (omegas 2); (3) coercion necessity (omegas 3); (4) Section 132 intentionality (omegas 4). The four omegas together should determine which reading is most defensible.',
    'If exogenous override is confirmed: this story''s Snare classification stands. If endogenous reinterpretation is confirmed: a sibling story (constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading) becomes primary, with Tangled Rope classification; this story becomes secondary. If practice-doctrine gap is primary: a third sibling story (constraint_marriage_commitment_reversal__practice_doctrine_gap) becomes the primary constraint; this story and endogenous reinterpretation become sub-stories of the gap itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_epistemic_underdetermination, conceptual, 'Which kernel reading best captures the institutional structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1882, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcr_exo_theater_t0, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mcr_exo_theater_t5, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(mcr_exo_theater_t10, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(mcr_exo_extract_t0, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mcr_exo_extract_t5, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(mcr_exo_extract_t10, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mcr_exo_suppression_t0, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mcr_exo_suppression_t5, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(mcr_exo_suppression_t10, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, lds_celestial_marriage_doctrine).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_governance_utah).

% DUAL FORMULATION NOTE:
% The marriage commitment reversal kernel has three structurally distinct readings with different ε values and causal drivers: exogenous_override_reading (ε=0.68, pure extraction by federal coercion), endogenous_reinterpretation_reading (ε≈0.50, theological reframing), and practice_doctrine_gap_reading (ε≈0.55, structural incoherence). The three readings coexist in LDS institutional interpretation and cannot be reduced to one 'correct' story. Each reading should be generated as a separate constraint story linked via network.affects_constraints. This family decomposition follows the ε-invariance principle: measuring the constraint through the lens of federal coercion (exogenous override) versus internal theology (endogenous reinterpretation) versus structural coherence (practice-doctrine gap) yields different ε values and requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
