% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__babel_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__babel_reading, []).

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
 *   constraint_id: human_transcendence_pathway__babel_reading
 *   human_readable: Babel Tower: Unified Technological/Linguistic Homogenization as Secular Transcendence
 *   domain: political_theology/technology_ethics/cultural_authority
 *
 * SUMMARY:
 *   This constraint embodies the Babel reading of the human transcendence
 *   kernel: the claim that collective human power organized through unified
 *   technological and linguistic systems can achieve stability and
 *   self-sufficiency without reference to transcendent (divine, sacred, or
 *   irreducibly plural) authority. The constraint operates by enforcing
 *   homogenization — linguistic minorities are assimilated, culturally
 *   embedded communities must integrate into the system, and alternatives to
 *   the unified framework are actively suppressed as irrational or
 *   inefficient. The Babel reading posits that such enforced uniformity,
 *   justified by promises of coordination and stability, actually produces
 *   high extraction and brittleness: when the system fails (and complex
 *   technical systems do fail), those who abandoned local knowledge and
 *   cultural resilience face catastrophic vulnerability. This is ONE reading
 *   of the contested kernel; the sibling readings (Jerusalem,
 *   Technocratic-vs-Incarnational) offer structurally different accounts of
 *   human transcendence and will be instantiated as separate constraints with
 *   different ε values and victim/beneficiary structures.
 *
 * KEY AGENTS:
 *   - Unified system architects (institutional power, arbitrage exits): design and maintain the universal framework; extract authority through control of coordination mechanisms
 *   - Technological monopolists (institutional power, arbitrage exits): control the physical/digital infrastructure; extract rents through mandatory adoption; benefit from the system's promise of stability
 *   - Culturally embedded communities (powerless, identity-locked exits): bear the cost of assimilation; face dissolution of identity-worlds; trapped because exit means ceasing to be who they are
 *   - Linguistic minorities (powerless, identity-locked exits): experience erasure of native languages; face generational assimilation; carry the identity-lock of inherited speech
 *   - System beneficiaries with integrated identity (powerful, identity-locked exits): elites whose status and identity depend on system mastery; simultaneously beneficiary and victim of identity-fusion; unlikely to recognize the extraction
 *   - Theistic critics (analytical seat, moderate power): religious and philosophical traditions that contest the claim of self-sufficiency without transcendent reference; documentary power but no operational leverage
 *   - Traditional authority holders (excluded, trapped): religious leaders and cultural stewards whose legitimacy derives from transcendent frameworks; systematically delegitimized within the system's discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, 0.82).
domain_priors:suppression_score(human_transcendence_pathway__babel_reading, 0.79).
domain_priors:theater_ratio(human_transcendence_pathway__babel_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__babel_reading, snare).
narrative_ontology:human_readable(human_transcendence_pathway__babel_reading, "Babel Tower: Unified Technological/Linguistic Homogenization as Secular Transcendence").
narrative_ontology:topic_domain(human_transcendence_pathway__babel_reading, "political_theology/technology_ethics/cultural_authority").

domain_priors:requires_active_enforcement(human_transcendence_pathway__babel_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__babel_reading, '00786928-af60-4f22-a3a8-ef5c901a1e33').
narrative_ontology:cs_kernel_codification('00786928-af60-4f22-a3a8-ef5c901a1e33', distributed).
narrative_ontology:cs_authority_grounding('00786928-af60-4f22-a3a8-ef5c901a1e33', extraction).
narrative_ontology:cs_interpretation_layer_present('00786928-af60-4f22-a3a8-ef5c901a1e33').
narrative_ontology:cs_reading_relation('00786928-af60-4f22-a3a8-ef5c901a1e33', human_transcendence_pathway__jerusalem_reading, forecloses).
narrative_ontology:cs_reading_relation('00786928-af60-4f22-a3a8-ef5c901a1e33', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_axiom('00786928-af60-4f22-a3a8-ef5c901a1e33', foundational, transcendence_without_transcendence_possible).
narrative_ontology:cs_axiom_status(transcendence_without_transcendence_possible, holdable).
narrative_ontology:cs_axiom_grounding('00786928-af60-4f22-a3a8-ef5c901a1e33', transcendence_without_transcendence_possible, empirically_contingent).
narrative_ontology:cs_axiom('00786928-af60-4f22-a3a8-ef5c901a1e33', foundational, uniformity_as_stability_condition).
narrative_ontology:cs_axiom_status(uniformity_as_stability_condition, holdable).
narrative_ontology:cs_axiom_grounding('00786928-af60-4f22-a3a8-ef5c901a1e33', uniformity_as_stability_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('00786928-af60-4f22-a3a8-ef5c901a1e33', universal_rational_self_sufficiency).
narrative_ontology:cs_drift_state('00786928-af60-4f22-a3a8-ef5c901a1e33', contemporary_system_fragility_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('00786928-af60-4f22-a3a8-ef5c901a1e33', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__babel_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, unified_system_architects).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, technological_monopolists).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, culturally_embedded_communities).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, linguistic_minorities).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, non_integrated_populations).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, post_system_collapse_survivors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__babel_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__babel_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__babel_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_transcendence_pathway__babel_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores as high extraction (0.82 at interval end) because the measured transfer is not negotiated or proportional to coordination costs; it is enforced through control of access to the system and through suppression of alternatives. Suppression is high (0.79) because the constraint's persistence depends on active delegitimization and structural exclusion of non-integrated frameworks — not on their ineffectiveness but on their threat to the system's claim of self-sufficiency. Theater ratio is moderate (0.41) because the system performs a genuine coordination function (communication at scale, resource distribution) alongside the extractive function, but over the interval, performative justification increases (theater_ratio climbs from 0.25 to 0.41) as actual coordination returns diminish and enforcement machinery must expand to maintain compliance. The leveled coercion grid shows asymmetric pressure: suppression is highest at the class and structural levels (cultural erasure, institutional delegitimization) while individual resistance remains above zero but declining — the system's power lies not in defeating individual resistance but in making organized class resistance and structural alternatives incoherent. The measurement series tracks the constraint's deepening over 40 years: extraction stabilizes around 0.82, theater climbs and plateaus at 0.41, and suppression requirement increases from 0.62 to 0.79 as the system matures and the cost of maintaining the unified framework against latent plurality pressures grows.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and monopolist seats, the constraint computes as rope (genuine coordination mechanism they maintain competently). From the powerless and identity-locked seats, it computes as snare (extraction dressed as coordination). This divergence is structural and real: the architects genuinely experience the system as solving coordination problems; the victims genuinely experience it as enforced assimilation. The engine's per-seat computation captures this asymmetry. The authored claim is snare — high extraction, active enforcement, identifiable victims — which aligns with the victim and analyst seats' experience but diverges from the beneficiary seats' self-perception. This divergence is exactly what the framework measures; the divergence is the diagnosis.
 *
 * DIRECTIONALITY LOGIC:
 *   Unified system architects and technological monopolists occupy the beneficiary end (d near 0.0): they control the system, collect surplus authority, face arbitrage exits (they can always pivot to new systems if this one fails). Culturally embedded communities and linguistic minorities occupy the full-target end (d near 1.0): they bear the extraction directly, face identity-locked exits (leaving the system means ceasing to be who they are), and carry no arbitrage options. System beneficiaries with integrated identity are structurally interesting: they occupy role=beneficiary (they benefit, they collect status and material advantage) but also face identity-lock (their sense of self is fused with system participation) — their d is ambiguous between 0.1 and 0.4 (beneficiary mobility with partial identity-lock). Traditional authority holders are excluded (not counted in d derivation). Theistic critics occupy the analytical seat (d undefined). The directionality profile asymmetrically weights the victim seats: four victim groups, all powerless or moderately powerful with constrained exits, against two beneficiary groups with institutional power and arbitrage. This weighting reflects the constraint's structural nature: it is an extraction mechanism that works by concentrating power and erasing alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordination across human plurality without invoking transcendent authority — was plausibly live at the system's inception. The constraint is contested as to whether the problem remains live. Architects attest it is permanent: human plurality always generates chaos unless centrally managed. Critics and survivors attest it has shifted: the system's own brittleness (its dependence on unbroken infrastructure, its vulnerability to cascade failure) has become the primary source of instability, eclipsing the coordination problem it was built to solve. The mandatrophy reading is strong: the founding problem may be dead (we have learned that plural, distributed systems can coordinate through networks rather than uniformity; we have evidence that system rigidity creates vulnerability), but the constraint persists because it benefits the architects and monopolists. The theater ratio's climb (from 0.25 to 0.41) is diagnostic of degraded function: as the system ages, more enforcement energy goes to defending the uniformity mandate and less to actual coordination, which would indicate the founding problem is no longer the driving constraint — it has become a rationalizing story for institutional capture. An explicit mandatrophy_resolved flag would be warranted if the founding problem is judged dead by parties outside the beneficiary set; the six_questions structure captures this via the founding_problem_status (contested) and the mismatch check (status=contested + disappearance_verdict=world_rearranges flags zombie constraint dynamics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    system_brittleness_vs_coordination_necessity,
    'Is the high suppression and extraction requirement driven by the genuine necessity of uniformity for coordination, or by the technical system''s brittleness when face with heterogeneity and the architects'' interest in maintaining control?',
    'Empirical comparison: examine systems with explicit heterogeneity (federated protocols, pluralist governance models) to measure whether they coordinate less effectively than unified systems or merely reveal different coordination tradeoffs. Also examine: post-collapse studies documenting whether communities with preserved cultural diversity recover faster and more robustly than those that abandoned local knowledge for system dependency.',
    'If brittleness drives suppression (not coordination necessity), the constraint is pure snare, not rope-with-extraction. If heterogeneous systems coordinate adequately, the suppression''s beneficiary claim collapses and mandatrophy becomes obvious. If diverse communities recover better, the system''s promised stability is revealed as illusory and the extraction as asymmetrically punishing those who believed the promise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_brittleness_vs_coordination_necessity, empirical, 'Whether suppression defends necessary coordination or only technical inflexibility and institutional capture.').

omega_variable(
    identity_lock_mechanism_structural_vs_internalized,
    'To what extent is the identity-lock experienced by victims (especially linguistically minoritized and culturally embedded communities) structural (external barriers, legal prohibitions, economic dependency on the system) versus internalized (self-belief that the native language/culture is inferior, that assimilation is necessary progress, that alternatives are irrational)?',
    'Post-system-failure ethnography: When unified system collapses, do communities immediately recover native-language use and cultural practice (suppression is primarily structural), or do recovered communities continue to devalue the heritage language (suppression is substantially internalized)? Also: language revitalization programs in living multilingual contexts; measure speed and extent of re-adoption when structural barriers are removed.',
    'If identity-lock is primarily internalized, the constraint''s effective suppression is higher than the authored 0.79 — the targets carry the suppression with them even after exit, making the constraint''s reach generational and its extraction durable across system-failure events. If primarily structural, suppression weakens rapidly when barriers are removed, but the damage is irreversible (a generation has grown up with erased heritage). The distinction affects mandatrophy assessment: internalized lock suggests the founding problem (coordination without transcendent reference) was reframed as identity diminishment; structural lock suggests the system works by coercion, not by persuasion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_structural_vs_internalized, empirical, 'Whether suppression of non-integrated frameworks is externally enforced or accepted as inevitable by victims themselves.').

omega_variable(
    transcendence_claim_falsifiability,
    'The constraint claims human systems can achieve stability and self-sufficiency without reference to transcendent authority. By what criteria could this claim be falsified? If the system fails, does that falsify the claim, or is failure attributed to insufficient uniformity (reframing back to the original claim)? Is there any evidence that would count as refutation?',
    'Examine responses to system failures and crises: Do architects and beneficiaries revise the claim (perhaps: ''we need even more uniformity, even more technical integration''), or do they interpret failures as evidence that transcendence-without-transcendence is impossible? Also: Do theistic traditions and critics of the constraint successfully propose alternative frameworks that gain institutional adoption?',
    'If the claim is non-falsifiable (any failure triggers calls for deeper integration rather than reconsideration of the premise), the constraint is partly sustained by rhetorical closure rather than empirical success, strengthening the mandate-death assessment. If evidence accumulates that human systems require either explicit transcendent grounding or explicit acceptance of irreducible plurality (not false uniformity), the constraint''s ideological foundation erodes, potentially triggering delegitimization among beneficiaries themselves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transcendence_claim_falsifiability, conceptual, 'Whether the claim of self-sufficiency without transcendence is empirically testable or rhetorical.').

omega_variable(
    sibling_reading_relation_choice_contingency,
    'Are the structural relationships between the Babel reading and its siblings (forecloses/coexists_with/influences) themselves contingent on the outcome of the empirical questions above (system brittleness, identity-lock mechanism, claim falsifiability)? If evidence accumulates that transcendence-without-transcendence is impossible, does Babel then foreclose Technocratic readings while Jerusalem becomes more tenable?',
    'Genealogical analysis: How have the three readings actually competed and displaced each other in institutional contexts (governance, academia, religion, technology)? Has one foreclosed others historically, or have they coexisted? Do crises (system failures, existential risks) trigger shifts in which reading is treated as coherent?',
    'If relations between readings are contingent on empirical outcomes, the cs_structure relations (reading_relations) are forward-looking: they express the present configuration, not a stable logical structure. This would suggest the kernel itself is live and contested, not settled — appropriate for a constraint in the first generation of automated analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_relation_choice_contingency, conceptual, 'Whether the logical structure of sibling readings depends on contingent empirical or institutional facts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__babel_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__babel_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(huma_tr_t5, human_transcendence_pathway__babel_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(huma_tr_t10, human_transcendence_pathway__babel_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(huma_tr_t15, human_transcendence_pathway__babel_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__babel_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(huma_tr_t25, human_transcendence_pathway__babel_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(huma_tr_t30, human_transcendence_pathway__babel_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(huma_tr_t35, human_transcendence_pathway__babel_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__babel_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__babel_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(huma_be_t5, human_transcendence_pathway__babel_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(huma_be_t10, human_transcendence_pathway__babel_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement(huma_be_t15, human_transcendence_pathway__babel_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__babel_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(huma_be_t25, human_transcendence_pathway__babel_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(huma_be_t30, human_transcendence_pathway__babel_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(huma_be_t35, human_transcendence_pathway__babel_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__babel_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__babel_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(huma_su_t5, human_transcendence_pathway__babel_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement(huma_su_t10, human_transcendence_pathway__babel_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(huma_su_t15, human_transcendence_pathway__babel_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(huma_su_t20, human_transcendence_pathway__babel_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(huma_su_t25, human_transcendence_pathway__babel_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement(huma_su_t30, human_transcendence_pathway__babel_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(huma_su_t35, human_transcendence_pathway__babel_reading, suppression_requirement, 35, 0.79).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__babel_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__babel_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__babel_reading, 0.18).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__jerusalem_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (Babel) of the contested human_transcendence_pathway kernel. The kernel decomposes into three structurally distinct constraints with different ε values, victim/beneficiary structures, and institutional histories: the Babel reading (this file, snare, high extraction ~0.82, victims: culturally embedded communities); the Jerusalem reading (alternative constraint file, tangled rope or rope, lower extraction, beneficiary: participatory communities, emphasis on diversity-in-communion rather than enforced uniformity); the Technocratic-vs-Incarnational reading (alternative constraint file, snare-or-mountain-depending-on-reading, victims: those treated as means rather than ends, emphasis on grace vs. optimization). The ε-invariance principle requires separate stories because the same natural-language concept 'human transcendence' instantiates three different extraction dynamics depending on which reading of the kernel one adopts. The families are linked via network.affects_constraints: Babel affects both sibling readings (the claim of self-sufficiency without transcendence shapes how both alternatives are framed); the empirical outcomes affecting Babel's sustainability also constrain how plausible the siblings are.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_transcendence_pathway__babel_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
