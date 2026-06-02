% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_occupation, []).

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
 *   constraint_id: kodashim_obligation__study_as_occupation
 *   human_readable: Kodashim Study as Legitimate Occupational Substitute for Temple Sacrifice Performance
 *   domain: religious_studies/jewish_law/commitment_systems
 *
 * SUMMARY:
 *   The kodashim_obligation (the obligation to study Jewish sacrifice law) is
 *   a central commitment in Jewish legal culture grounded in the Talmud's
 *   claim that study of the temple service constitutes an occupational
 *   equivalent to actual performance. This is one of three structurally
 *   distinct readings of the same contested kernel — the obligation itself.
 *   The study_as_occupation reading instantiates the Talmudic position that
 *   makes scholarly mastery of Kodashim a legitimate substitute for temple
 *   sacrifice when performance is impossible. This reading became
 *   institutionally dominant in rabbinic Judaism after the temple's
 *   destruction in 70 CE and remains normative in Orthodox yeshiva culture.
 *   However, it is structurally a *reading* of the kernel, not the only
 *   defensible interpretation. Alternative readings claim that study can only
 *   preserve law temporarily (memorial_archival), or that the obligation
 *   requires at least some performance context to be meaningful
 *   (performance_prerequisite). The constraint exhibits tangled rope
 *   dynamics: the rabbinic authority structure benefits from the
 *   interpretation (institutional legitimacy grounded in unbroken knowledge
 *   transmission), while scholars bear extractive burden (obligation to
 *   master obscure details with no functional outcome). The theater_ratio
 *   rises sharply over 400 years (0.38 → 0.64), reflecting accumulation of
 *   elaborate interpretive apparatus around laws that cannot be performed.
 *
 * KEY AGENTS:
 *   - Kodashim Scholars: Primary victims (powerless/identity_locked at biographical time; moderate/constrained at generational time) — bear the obligation to maintain competence in laws that cannot be performed; identity fused with mastery role; career contingent on institutional recognition
 *   - Rabbinic Authority Structure: Primary beneficiaries (institutional/arbitrage) — gain institutional legitimacy from claim that study maintains unbroken transmission; gate access to authentic interpretation; consolidate hierarchy through mastery requirements
 *   - Scholarly Community: Secondary beneficiary/victim (moderate/constrained at generational time) — coordinate genuine preservation of knowledge tradition while enforcing extractive obligation; benefit from institutional validation, bear burden of maintenance
 *   - Reform and Conservative Movements: Organized challengers (organized/mobile) — view the obligation as scaffolding with sunset; maintain some Kodashim study but subordinate it to other practices; see the constraint as contextual rather than eternal
 *   - Jewish Community (Non-Initiated): Implicit victim (powerless/constrained) — excluded from mastery; authority structure concentrated in specialists; gatekeeping of legal interpretation
 *   - Analytical Observer: Civilization-level view (analytical/analytical) — risks naturalizing the institutional reading as natural law of Judaism itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_occupation, 0.38).
domain_priors:suppression_score(kodashim_obligation__study_as_occupation, 0.52).
domain_priors:theater_ratio(kodashim_obligation__study_as_occupation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_occupation, extractiveness, 0.38).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_occupation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_occupation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_occupation, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_occupation, "Kodashim Study as Legitimate Occupational Substitute for Temple Sacrifice Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_occupation, "religious_studies/jewish_law/commitment_systems").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_occupation, 'e5fcf872-6271-4774-bffc-ad136c6ee4c3').
narrative_ontology:cs_kernel_codification('e5fcf872-6271-4774-bffc-ad136c6ee4c3', fixed_text).
narrative_ontology:cs_authority_grounding('e5fcf872-6271-4774-bffc-ad136c6ee4c3', lineage).
narrative_ontology:cs_interpretation_layer_present('e5fcf872-6271-4774-bffc-ad136c6ee4c3').
narrative_ontology:cs_reading_relation('e5fcf872-6271-4774-bffc-ad136c6ee4c3', kodashim_obligation__performance_prerequisite, coexists_with).
narrative_ontology:cs_reading_relation('e5fcf872-6271-4774-bffc-ad136c6ee4c3', kodashim_obligation__memorial_archival, coexists_with).
narrative_ontology:cs_axiom('e5fcf872-6271-4774-bffc-ad136c6ee4c3', foundational, study_substitutes_for_performance).
narrative_ontology:cs_axiom_status(study_substitutes_for_performance, holdable).
narrative_ontology:cs_axiom_grounding('e5fcf872-6271-4774-bffc-ad136c6ee4c3', study_substitutes_for_performance, conventional).
narrative_ontology:cs_axiom('e5fcf872-6271-4774-bffc-ad136c6ee4c3', foundational, unbroken_transmission_preserves_authenticity).
narrative_ontology:cs_axiom_status(unbroken_transmission_preserves_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('e5fcf872-6271-4774-bffc-ad136c6ee4c3', unbroken_transmission_preserves_authenticity, deontological).
narrative_ontology:cs_reference_frame('e5fcf872-6271-4774-bffc-ad136c6ee4c3', temple_destroyed_study_becomes_occupational_substitute).
narrative_ontology:cs_drift_state('e5fcf872-6271-4774-bffc-ad136c6ee4c3', contemporary_post_2000_ce, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5fcf872-6271-4774-bffc-ad136c6ee4c3', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_occupation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_occupation, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_occupation, scholarly_elite).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_occupation, scholars_of_kodashim).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_occupation, non_initiated_jewish_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KODASHIM SCHOLAR (SNARE) — The scholar's identity is constituted through the obligation to master Kodashim. They cannot exit without abandoning their professional identity and their understood duty to maintain the knowledge chain. They bear the extraction: continuous labor without performance outcomes, no material benefit from their study beyond institutional recognition. The identity lock prevents them from recognizing they could stop — the unbroken chain narrative has become internal. Trapped at biographical horizon.
constraint_indexing:constraint_classification(kodashim_obligation__study_as_occupation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: SCHOLARLY COMMUNITY (TANGLED ROPE) — Across generations, the community genuinely coordinates the preservation and transmission of sacrifice law. There is a real coordination function: maintaining interpretive continuity, preventing knowledge loss, enabling future renewal of temple practice if conditions permit. Simultaneously, the community bears extractive burden: enforcement of continuous study obligation, normative pressure to prioritize this over other religious vocations, concentration of authority in those who master the most obscure details. The constraint both enables and exploits collective learning.
constraint_indexing:constraint_classification(kodashim_obligation__study_as_occupation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC AUTHORITY (ROPE) — The rabbinic establishment experiences this as pure coordination: study obligation creates legitimacy claim grounded in unbroken transmission. They benefit from the institutional authority this generates (gatekeeping access to authentic interpretation, maintaining hierarchy of knowledge). They experience the constraint as solving a genuine coordination problem: how does the community maintain legal continuity when the temple is destroyed? By making study equivalent to performance. The authority structures gain from this equivalence; it is not extraction from their perspective but legitimate institutional function.
constraint_indexing:constraint_classification(kodashim_obligation__study_as_occupation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM JUDAISM (SCAFFOLD) — Organized movements (especially Reform and Conservative Judaism) view the obligation to study Kodashim as a temporary, contextual accommodation to the loss of the temple. The constraint had a sunset clause built into it: when the temple was destroyed, study became a substitute. If the temple were rebuilt (or if Jewish practice reorganizes around other core practices), the constraint loses force. This reading sees the entire apparatus as scaffolding — necessary maintenance structure during a particular historical moment, but not permanent. The theatrical element (elaborate study of laws that cannot be performed) is justifiable during the interim, but not eternal.
constraint_indexing:constraint_classification(kodashim_obligation__study_as_occupation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PITON / INSTITUTIONAL INERTIA (PITON) — From a civilizational view, the constraint has become largely theatrical maintenance of institutional memory. The obligation persists because discontinuing it would require explicitly acknowledging that centuries-old interpretive practice is no longer functionally operative. The study remains valued (theater_ratio high because much energy is invested in formal study that produces no external performance), but the primary mechanism is now institutional inertia, not the original coordination function. The yeshiva system maintains Kodashim precisely because it always has, not because the knowledge is currently actionable.
constraint_indexing:constraint_classification(kodashim_obligation__study_as_occupation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — A reading that frames the study obligation as a natural law of Jewish law itself: 'study is performance' becomes an ontological claim about the nature of Jewish obligation, not a contingent institutional accommodation. This perspective naturalizes what is actually a reading of the kodashim_obligation kernel. The false summit detector will identify this as beneficiary-capture: the rabbinic authority structure has clear incentive to naturalize the interpretation that consolidates their institutional power. The structural data contradicts mountain classification.
constraint_indexing:constraint_classification(kodashim_obligation__study_as_occupation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_occupation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kodashim_obligation__study_as_occupation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kodashim_obligation__study_as_occupation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kodashim_obligation__study_as_occupation, TR),
    TR >= 0.70.

:- end_tests(kodashim_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does extract: scholars invest enormous labor in mastery that produces no functional performance, while authority accrues to those who master the most recondite details. However, the extraction is not severe because the scholarly community genuinely coordinates knowledge preservation with real institutional value. The measurement trajectory (0.22 → 0.38 over 400 years) shows that extractiveness has accumulated — what began as emergency wartime accommodation has ossified into increasingly elaborate and demanding system. Suppression (0.52): Moderate-high. The obligation is enforced through normative pressure (identity fusion with mastery role, career dependence on yeshiva institutional recognition), institutional gatekeeping (access to prestigious study positions), and social sanction (those who leave intensive study are culturally marked as less committed). However, suppression is not total — alternative paths exist (Reform Judaism, secular Jewish scholarship), and individuals do exit, though at identity cost. Theater ratio (0.64): High. The elaborate study of sacrifice law that cannot be performed is theatrical — the interpretive apparatus (debates about Temple measurements, ritual implements, slaughter protocols, purity laws) has grown increasingly elaborate over centuries with no functional purpose. The theater serves institutional maintenance rather than knowledge preservation — it signals mastery, legitimates authority, and fills the symbolic void left by the destroyed temple. The measurement trajectory (0.38 → 0.64) reflects that as centuries pass and temple restoration becomes less plausible, the performative element of the study obligation has increased relative to its functional justification.
 *
 * PERSPECTIVAL GAP:
 *   This reading creates a perspectival gap between the beneficiary's experience (Rope — legitimate coordination of knowledge continuity) and the scholar's experience (Snare — pure extraction of labor without performance outcome, locked by identity fusion). The authority structure experiences the constraint as solving a genuine problem: how to maintain legal continuity across the temple's destruction. The scholar experiences it as an obligation with no external justification, binding them precisely because their identity has become fused with it. The Reform reading (Scaffold) introduces a temporal structure the beneficiary rejects: it frames the obligation as an interim accommodation with a sunset clause (when the temple is rebuilt, when practice reorganizes, when written texts replace live transmission — all contingent futures that may never arrive, making the 'temporary' structure permanently institutional). The piton perspective identifies the theatrical maintenance of the system as the operative mechanism — the system persists through institutional inertia, not functional justification. The analytical observer's mountain classification is the false summit: attempting to naturalize a contingent institutional reading as an inherent property of Jewish law itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The study_as_occupation reading creates distinct directionality profiles for different agents. The beneficiary (rabbinic authority) derives d ≈ 0.12 from institutional/arbitrage positioning — they benefit from the interpretation, have exit options (could adopt alternative readings), and use the constraint to consolidate power. The scholar victims derive d ≈ 0.88 from powerless/identity_locked positioning at biographical time — they are structurally trapped by identity fusion with the mastery role, and removing external barriers would not free them because their self-concept is constituted through the knowledge obligation. At generational time, the scholarly community's d shifts to ≈ 0.58 (moderate/constrained) — the community does coordinate genuine knowledge preservation, so the pure extraction component reduces. The measuring context's power and exit determine the effective extraction experienced from the same base institutional structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_equivalence_ontology,
    'Is ''study equivalent to performance'' a metaphysical claim about Jewish law, or a contingent institutional accommodation to the temple''s loss?',
    'Historical analysis of Amoraic discussions: do sources present the equivalence as temporary wartime measure, or as eternal law? Examination of parallel cases where functional performance is impossible (e.g., cities of refuge after centralized justice ceased) — is study equivalence applied consistently, or is it specific to Kodashim?',
    'If ontological: mountain classification warranted; constraint is structure of Jewish law itself. If contingent: tangled_rope or scaffold warranted; constraint is institutional arrangement with historical end-point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_equivalence_ontology, conceptual, 'Whether study equivalence is ontological or contingent to temple''s absence').

omega_variable(
    beneficiary_capture_of_kernel,
    'Does the rabbinic authority structure''s claim that ''study maintains the unbroken chain'' represent genuine epistemic preservation, or rationalization of institutional gatekeeping?',
    'Comparison of Kodashim interpretation retention rates across: (a) intensive yeshiva study environments, (b) isolated communities with minimal institutional structure, (c) communities using written texts without live transmission chains. If retention and interpretive coherence are equal, the ''unbroken chain'' narrative is partially theatrical; if dramatically lower, the live transmission provides genuine epistemic value.',
    'If captured: false summit signature confirmed. If genuine: the institutional authority structure''s claim has real epistemic warrant, and the constraint''s beneficiary status for rabbinic authority is justified by true institutional function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_capture_of_kernel, empirical, 'Whether unbroken transmission chain provides genuine epistemic preservation').

omega_variable(
    identity_lock_mechanism_in_scholars,
    'Is the scholar''s binding to the Kodashim obligation primarily structural (no exit due to economic/social barriers), or is it fundamentally cognitive (identity constituted through mastery and chain participation)?',
    'Post-exit trajectories: scholars who leave intensive study — do they maintain connection to Jewish intellectual life, or experience identity dissolution? Do scholars report the obligation as a cost imposed externally, or as an internalized ideal they cannot imagine abandoning? Comparison with scholars who leave other demanding intellectual traditions (Buddhist monastic study, Islamic jurisprudence) — does Jewish Kodashim show higher reported identity-fusion?',
    'If structural: classify as trapped/constrained exit. If cognitive: identity_locked classification confirmed, and the constraint persists through internalized frame even if external barriers were removed. The scholar''s burden is psychological, not merely material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_scholars, empirical, 'Whether scholar binding is structural barrier or identity fusion').

omega_variable(
    competing_reading_coexistence,
    'Can the performance_prerequisite reading (temple sacrifice MUST precede legitimate interpretation) and the study_as_occupation reading coexist within a single Jewish legal framework, or do they logically foreclose one another?',
    'Contemporary Jewish jurisprudence: do authoritative voices hold both readings simultaneously as legitimate (permitting both performance-first and study-substitute approaches)? Or do they explicitly choose one over the other? If coexistence, the relation is coexists_with; if mutual foreclosure, the relation is forecloses.',
    'If coexist: contemporary Judaism holds multiple readings live. If foreclose: one reading has won institutional dominance and the other is relegated to historical interest or minority position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_coexistence, conceptual, 'Logical compatibility of study_as_occupation and performance_prerequisite readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_occupation, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_tr_t0, kodashim_obligation__study_as_occupation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(kodashim_study_tr_t200, kodashim_obligation__study_as_occupation, theater_ratio, 200, 0.51).
narrative_ontology:measurement(kodashim_study_tr_t400, kodashim_obligation__study_as_occupation, theater_ratio, 400, 0.64).

% Extraction over time
narrative_ontology:measurement(kodashim_study_be_t0, kodashim_obligation__study_as_occupation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kodashim_study_be_t200, kodashim_obligation__study_as_occupation, base_extractiveness, 200, 0.3).
narrative_ontology:measurement(kodashim_study_be_t400, kodashim_obligation__study_as_occupation, base_extractiveness, 400, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_study_su_t0, kodashim_obligation__study_as_occupation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(kodashim_study_su_t200, kodashim_obligation__study_as_occupation, suppression_requirement, 200, 0.5).
narrative_ontology:measurement(kodashim_study_su_t400, kodashim_obligation__study_as_occupation, suppression_requirement, 400, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_occupation, kodashim_obligation__performance_prerequisite).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_occupation, kodashim_obligation__memorial_archival).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_occupation, rabbinic_authority_legitimacy__transmission_chain).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_occupation, jewish_legal_continuity_post_temple).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three structurally distinct constraints corresponding to three competing readings. Each reading instantiates a different epsilon, different beneficiary/victim structure, and different constraint type. The study_as_occupation reading (this constraint) has ε=0.38 (moderate extraction with genuine coordination). The performance_prerequisite reading would have higher ε (≈0.55-0.60) reflecting more skeptical view of study's sufficiency. The memorial_archival reading would have lower ε (≈0.15-0.25) reflecting framing as commemorative rather than substitutive. All three are linked via network.affects_constraints because they compete for institutional dominance and influence which interpretation is transmitted to the next generation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_occupation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
