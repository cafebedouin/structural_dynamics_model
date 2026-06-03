% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor Settlement Legitimacy (Contraction Reading): Cognitive Exit from Dueling
 *   domain: cultural_anthropology/legal_history/historical_sociology
 *
 * SUMMARY:
 *   The contraction reading of honor settlement legitimacy models dueling's
 *   cognitive exit from the normative possibility space of European elites
 *   between the 17th and 19th centuries. Unlike drop reading (dueling
 *   persisted as residual practice among honor-culture holdouts) or composite
 *   reading (overdetermined decline with multiple causal mechanisms), the
 *   contraction reading emphasizes the transformation of the cognitive
 *   framework itself: dueling became literally unthinkable as a legitimate
 *   honor settlement mechanism for the emergent literate, urban,
 *   state-integrated elite. This was not suppression by force (though laws
 *   against dueling were enacted) but cognitive impossibility — the framework
 *   within which dueling made sense (honor culture, lineage-based status,
 *   private violence as legitimate) was replaced by an alternative framework
 *   (bourgeois respectability, institutional credential, state monopoly on
 *   legitimate violence, written law as the arbiter of right). The constraint
 *   exhibits low extractiveness (0.15) and minimal suppression (0.08) because
 *   it operates as pure coordination: the new legitimacy framework for honor
 *   settlement (legal process, professional credential, institutional rank)
 *   genuinely enabled broader populations (non-nobles, women, urban
 *   populations) to participate in status competition without dueling. The
 *   theater ratio (0.25) is low because the cognitive shift was not
 *   performative — the reframing of honor settlement from personal combat to
 *   institutional process was substantive. The measurement trajectory shows
 *   theater and extractiveness declining together as the new framework
 *   consolidated: early institutional resistance to the new mechanisms (high
 *   theater as elites tried to preserve dueling despite legal prohibition)
 *   gives way to genuine cognitive integration of the new framework (theater
 *   and extractiveness both decline as the new mechanisms become genuinely
 *   functional). This is a Rope constraint: pure coordination with low
 *   extraction overhead.
 *
 * KEY AGENTS:
 *   - Literate urban bourgeoisie (powerful/mobile): Primary beneficiary — gains social mobility through institutional pathways without dueling legitimacy; experiences constraint as enabling their rise
 *   - State monopoly on legitimate violence (institutional/arbitrage): Primary beneficiary — gains legitimacy for its own authority over violence through displacement of dueling as rival settlement mechanism; experiences constraint as solving state's own legitimacy problem
 *   - Residual honor-culture adherents (powerful/trapped): Secondary agents in drop reading (not this reading) — persist as fringe population after contraction; in contraction reading, they exit the normative possibility space and cease to be structurally relevant
 *   - Literate print culture and new interpretive frameworks (institutional/arbitrage): Mediating mechanism — provides the conceptual vocabulary (reason of state, civilization, civil law) through which the cognitive reframing becomes possible
 *   - Analytical observer (analytical/analytical): Risks naturalizing the cognitive shift as inevitable or law-like rather than contingent and reversible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.15).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.08).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor Settlement Legitimacy (Contraction Reading): Cognitive Exit from Dueling").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "cultural_anthropology/legal_history/historical_sociology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '0a24aacd-b380-4165-8ba9-298222afa823').
narrative_ontology:cs_kernel_codification('0a24aacd-b380-4165-8ba9-298222afa823', fixed_text).
narrative_ontology:cs_authority_grounding('0a24aacd-b380-4165-8ba9-298222afa823', lineage).
narrative_ontology:cs_interpretation_layer_present('0a24aacd-b380-4165-8ba9-298222afa823').
narrative_ontology:cs_reading_relation('0a24aacd-b380-4165-8ba9-298222afa823', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a24aacd-b380-4165-8ba9-298222afa823', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('0a24aacd-b380-4165-8ba9-298222afa823', foundational, honor_culture_cognitive_framework_exit).
narrative_ontology:cs_axiom_status(honor_culture_cognitive_framework_exit, holdable).
narrative_ontology:cs_axiom_grounding('0a24aacd-b380-4165-8ba9-298222afa823', honor_culture_cognitive_framework_exit, deontological).
narrative_ontology:cs_axiom('0a24aacd-b380-4165-8ba9-298222afa823', foundational, institutional_legitimacy_framework_sufficiency).
narrative_ontology:cs_axiom_status(institutional_legitimacy_framework_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('0a24aacd-b380-4165-8ba9-298222afa823', institutional_legitimacy_framework_sufficiency, deontological).
narrative_ontology:cs_reference_frame('0a24aacd-b380-4165-8ba9-298222afa823', honor_culture_framework_early_modern).
narrative_ontology:cs_drift_state('0a24aacd-b380-4165-8ba9-298222afa823', enlightenment_institutional_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a24aacd-b380-4165-8ba9-298222afa823', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, literate_urban_bourgeoisie).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE MONOPOLY ON VIOLENCE (ROPE) — The early-modern state benefits from the cognitive exit of dueling as a legitimate honor settlement mechanism. Dueling's decline frees the state's monopoly claim over legitimate violence and eliminates a rival authority for conflict resolution. However, this is pure coordination: the state must establish alternative legitimacy for its own violence (through law, bureaucracy, public procedure), not suppress dueling through force. The state experiences the constraint as enabling coordination — it solves the state's own legitimacy problem.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__contraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: LITERATE URBAN BOURGEOISIE (ROPE) — Non-noble urban classes benefit from honor culture's cognitive contraction by gaining social mobility through institutional pathways (commerce, law, letters, governance) that do not require dueling legitimacy. Dueling's incomprehensibility enables the bourgeoisie to occupy social rank without participating in honor contests. This is coordination: the constraint enables a new settlement mechanism for social standing (education, wealth accumulation, professional credential) to displace the old one (lineage + martial honor). Exit is mobile — the bourgeoisie can and do exit dueling participation without cost, experiencing the constraint as enabling their rise.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__contraction_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / COGNITIVE IMPOSSIBILITY (MOUNTAIN) — From the civilizational analytical position, the contraction reading naturalizes the cognitive boundary shift: once dueling becomes incomprehensible as a legitimate honor settlement (not prohibited, but unthinkable), no rational agent within that framework can choose to duel. The constraint appears immutable — the transformation is irreversible once the interpretive framework closes. This perspective risks false summitry: the cognitive shift is presented as a natural law of cultural evolution, naturalizing what is actually a contingent institutional transformation.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__contraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).
:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Very low. The contraction reading models the constraint as pure coordination — the displacement of one honor settlement mechanism (dueling) by another (institutional). Neither mechanism extracts in the sense of asymmetric flow of goods or status; both are legitimacy frameworks for settling disputes. The residual asymmetry (state and bourgeoisie benefit more than residual honor-culture adherents) is minimal because the framework transformation itself is not coercive — it is cognitive. Suppression (0.08): Minimal. No significant barriers prevent agents from cognitively accepting the new framework. Legal prohibitions on dueling existed, but the contraction reading emphasizes that these were not the primary mechanism — the cognitive shift rendered dueling incomprehensible, making legal prohibition almost redundant. Theater ratio (0.25): Low and declining. The new honor settlement mechanisms (legal process, professional credential, institutional rank) are functionally effective at what they do — they genuinely settle disputes and confer status. Early in the interval (1650-1750), higher theater (0.45) reflects institutional resistance: elites tried to preserve dueling despite legal prohibition. Late in the interval (1800-1850), theater is minimal (0.10) because the cognitive framework has genuinely shifted — agents no longer need to perform adherence to the new mechanisms; they have authentically internalized them. The declining trajectory is diagnostic of Rope: as the coordination mechanism proves itself functionally adequate, performative elements fall away.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap here is not between different agents experiencing the same constraint differently (as in tangled_rope scenarios) but between the contraction reading itself and the sibling readings. The contraction reading sees pure coordination and cognitive transformation (Rope). The drop reading would see residual practice and cognitive diversity (different agents retain different frameworks simultaneously). The composite reading would see overdetermined decline with legal suppression, economic transformation, and state consolidation all contributing (Tangled Rope or Snare depending on the weight assigned to enforcement vs. coordination). The analytical observer in this reading risks false summitry: treating the cognitive shift as an immutable natural law ('once frameworks change, dueling becomes unthinkable') rather than recognizing the shift as contingent and potentially reversible. Dueling has not reappeared in modern Western contexts, but this is not because it became permanently incomprehensible — it is because the material conditions (honor culture, state weakness, decentralized violence authority) that made dueling rational have not reappeared.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint runs from the beneficiary institutions (state, urban bourgeoisie) through the coordination mechanism (new legitimacy frameworks) to the broader population. The state benefits from the constraint because it solves the state's own legitimacy problem — the monopoly claim over legitimate violence can now rest on law and bureaucracy rather than competing against private honor settlement. The bourgeoisie benefit because institutional credentials and wealth accumulation now confer status without requiring dueling participation. Both are beneficiaries with arbitrage options — they can choose to participate in the new frameworks and do so happily. Victims are minimal because the constraint is not extractive; residual honor-culture adherents are not victims in the economic sense, they are simply rendered cognitively marginal. The constraint's low d (derived from institutional beneficiaries with arbitrage options and absence of victims) produces low f(d) and therefore low effective extractiveness χ. This is diagnostic of Rope: beneficiaries, low suppression, coordination function, minimal extraction overhead.
 *
 * MANDATROPHY ANALYSIS:
 *   NOT APPLICABLE. This constraint is not mandatrophic because extractiveness is very low (0.15) and the classification (Rope) is stable across perspectives. There is no tension between declaring coordination and declaring extraction. However, the analytical observer's mountain perspective risks a different kind of mandatrophy: naturalizing the cognitive shift as inevitable or law-like. The false-summit detection should flag this: the mountain classification (cognitive impossibility) is contradicted by the beneficiary declarations (literate bourgeoisie, state) — if the shift is truly a natural law, why do identifiable agents benefit from it? The false summit fires, indicating that the mountain is actually a contingent institutional arrangement masquerading as necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_shift_versus_legal_prohibition,
    'Did dueling become incomprehensible (cognitive contraction) or merely prohibited (legal suppression)? Which mechanism was causally primary?',
    'Historical analysis of elite discourse over the 17th-18th centuries: track rationalization patterns in legal texts, memoirs, conduct literature. If prohibition came first followed by normative reframing, legal suppression was primary (drop reading). If cognitive reframing came first with law following, cognitive shift was primary (contraction reading). If both emerged simultaneously, composite reading.',
    'If cognitive shift was primary: contraction reading classifies as Rope (coordination). If legal prohibition was primary: classification shifts toward Snare (enforcement-dependent extraction). If simultaneous: composite reading explains the overdetermination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_shift_versus_legal_prohibition, empirical, 'Whether dueling''s decline was cognitive transformation or legal suppression').

omega_variable(
    alternative_honor_settlement_sufficiency,
    'Did the emerging bourgeois legitimacy mechanisms (legal process, professional credential, institutional rank) actually provide psychologically equivalent honor-settlement outcomes for non-noble populations, or merely pragmatic alternatives?',
    'Comparative study of honor-related litigation, narrative sources, and social mobility patterns pre- vs. post-contraction. Examine whether non-dueling honor mechanisms (lawsuits, public apology, institutional position) were perceived as genuinely resolving honor disputes or as mere substitutes.',
    'If psychologically equivalent: the contraction reading holds — cognitive framework transformed and dueling became genuinely unthinkable. If mere pragmatic substitutes: agents may have privately retained honor-culture cognition while publicly conforming (gap between cognitive change and behavioral change), suggesting drop reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_honor_settlement_sufficiency, empirical, 'Whether alternative honor mechanisms provided psychological equivalence to dueling').

omega_variable(
    literacy_and_normative_frame_shift,
    'Did the spread of literacy and print culture causally drive the cognitive reframing of dueling as unthinkable, or is literacy merely correlated with the shift?',
    'Analysis of print discourse on honor, violence, and settlement: track the introduction of new conceptual vocabulary (reason of state, civil law, civilization) into elite discourse. Compare regions with high vs. low print penetration to isolate the literacy effect from other variables (state strength, religious reformation, etc.).',
    'If literacy causally drives reframing: contraction reading is correct — the cognitive shift is driven by the availability of new interpretive frameworks. If merely correlated: other mechanisms (state monopoly, religious reframing, economic transformation) may be primary, weakening the contraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_and_normative_frame_shift, empirical, 'Whether literacy and print culture drove cognitive reframing of honor').

omega_variable(
    reading_identity_contraction_kernel,
    'Is this constraint ONE READING of the contested kernel ''honor_settlement_legitimacy'' (committer frame), or does it represent THE TRUTH about how dueling actually declined?',
    'Acknowledge that different historical schools genuinely hold different readings: the cognitive historians hold contraction; the institutional historians hold composite; the cultural residualists hold drop. Each reading is internally coherent. The engine resolves the contest by treating them as three separate constraints, not as three hypotheses about one constraint.',
    'If this is treated as THE reading: false unity of the historical record; the contest is suppressed. If recognized as ONE reading among three: proper structural representation of the epistemically open question. The constraint story format enforces this recognition by requiring separate files for each reading with separate ε values and separate network relationships.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_contraction_kernel, conceptual, 'Whether this constraint is one reading of a contested kernel or the correct account of dueling''s decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1650, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_contraction_theater_early, honor_settlement_legitimacy__contraction_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(honor_contraction_theater_mid, honor_settlement_legitimacy__contraction_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(honor_contraction_theater_late, honor_settlement_legitimacy__contraction_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(honor_contraction_extract_early, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(honor_contraction_extract_mid, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement(honor_contraction_extract_late, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, state_monopoly_on_violence__early_modern_consolidation).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, bourgeois_legitimacy__institutional_credential).

% DUAL FORMULATION NOTE:
% The honor_settlement_legitimacy kernel is contested across three readings. This file (contraction_reading) models the cognitive framework transformation view. The sibling files (drop_reading and composite_reading) model alternative structural accounts of the same historical phenomenon. Each reading is a distinct constraint with distinct ε, distinct perspectives, and distinct beneficiary/victim structure. They are not hypothesis variations within one constraint — they are three constraints generated from three competing readings of a single kernel. The affects_constraints edges link the readings and their institutional downstream consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
