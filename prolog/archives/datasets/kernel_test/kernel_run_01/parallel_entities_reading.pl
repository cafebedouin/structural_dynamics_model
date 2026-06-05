% ============================================================================
% CONSTRAINT STORY: parallel_entities_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parallel_entities_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: parallel_entities_reading
 *   human_readable: Parallel Kami-Buddha Coexistence: Institutional Coordination Without Ontological Fusion
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   The parallel-entities reading of kami-buddha coexistence models a
 *   religious system in which Shinto kami and Buddhist deities are understood
 *   as ontologically distinct yet functionally integrated at the
 *   institutional level. This reading asserts that kami and buddhas remain
 *   separate categories—they do not fuse into a unified metaphysical system,
 *   nor do they occupy distinct domains governed by a higher-order partition
 *   principle. Instead, particular kami and particular buddhas coordinate
 *   across specific ritual contexts and life domains without requiring either
 *   a systematic theology of their relationship or a formal boundary
 *   demarcation. This constraint operates at the institutional-administrative
 *   level (shrine-temple systems) and the practical level (dual-participation
 *   rituals), with extractive pressure concentrated on doctrinal coherence
 *   seekers whose intellectual work is suppressed by framing the incoherence
 *   as non-problematic or 'not the Japanese way.' The theater ratio reflects
 *   that Meiji-era state imposition of hierarchy (Shinto above Buddhism) left
 *   performative marks (shrine-temple distinction, state ritual protocol)
 *   while the underlying parallel-coordination logic remained functionally
 *   intact. The constraint demonstrates Tangled Rope properties: genuine
 *   coordination function (enabling flexible dual participation without
 *   forced doctrinal choice) plus asymmetric extraction (suppression of
 *   theoretical work, displacement of coherence-seeking onto individual
 *   practitioners). This reading coexists with two structural alternatives:
 *   fused_ontology_reading (kami and buddhas merge under interpretive layer),
 *   and domain_partition_reading (kami govern one domain, buddhas another,
 *   with higher-order principle reconciling the domains). The three readings
 *   are driven by different interpretive strategies applied to the same
 *   historical evidence—different scholars, institutional actors, and time
 *   periods privilege different readings.
 *
 * KEY AGENTS:
 *   - Institutional Shrine-Temple System: Primary beneficiary (institutional/arbitrage) — maintains dual authority, avoids jurisdictional conflict, extracts practitioner labor and ritual participation without imposing doctrinal choice
 *   - Ritual Specialists (Shinto Priests & Buddhist Monks): Secondary beneficiary (institutional/constrained) — dual-authority system allows professional autonomy and role specialization without competition; constrained only by career risk of doctrinal innovation
 *   - Doctrinal Coherence Seekers: Primary victim (powerless/trapped) — theologians, philosophers, and pious practitioners seeking unified ontology face institutional suppression of the question itself; no exit from the incoherence-as-virtue framing
 *   - Practitioners in Dual Participation: Secondary victim (moderate/constrained) — benefit from flexibility of parallel system but bear cognitive burden of managing incoherence and subsidize institutional maintenance through ritual labor
 *   - Meiji-Era Modernization Regime: Historical actor (institutional/constrained) — attempted to reclassify parallel coexistence as hierarchical separation (state Shinto ideology); enforcement created performative distinction that persists though underlying logic unchanged
 *   - Postwar Pluralism Consensus: Organized beneficiary (organized/mobile) — reframed parallel-entities reading as legitimate pluralism rather than deficiency; reduced extractive pressure through doctrinal legitimacy
 *   - Analytical Observer: Meta-institutional perspective (analytical/analytical) — sees genuine coordination function and real asymmetric extraction; identifies false summit risk (temptation to naturalize institutional arrangement as immutable law of Japanese religion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parallel_entities_reading, 0.35).
domain_priors:suppression_score(parallel_entities_reading, 0.42).
domain_priors:theater_ratio(parallel_entities_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parallel_entities_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(parallel_entities_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(parallel_entities_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parallel_entities_reading, tangled_rope).
narrative_ontology:human_readable(parallel_entities_reading, "Parallel Kami-Buddha Coexistence: Institutional Coordination Without Ontological Fusion").
narrative_ontology:topic_domain(parallel_entities_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(parallel_entities_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parallel_entities_reading, 'af2ad435-506e-495a-84cf-6ae014237c37').
narrative_ontology:cs_created_at('af2ad435-506e-495a-84cf-6ae014237c37', '').
narrative_ontology:cs_kernel_codification('af2ad435-506e-495a-84cf-6ae014237c37', distributed).
narrative_ontology:cs_authority_grounding('af2ad435-506e-495a-84cf-6ae014237c37', lineage).
narrative_ontology:cs_interpretation_layer_present('af2ad435-506e-495a-84cf-6ae014237c37').
narrative_ontology:cs_kernel_id(parallel_entities_reading, kami_buddha_ontology).
narrative_ontology:cs_reading_relation('af2ad435-506e-495a-84cf-6ae014237c37', fused_ontology_reading, coexists_with).
narrative_ontology:cs_reading_relation('af2ad435-506e-495a-84cf-6ae014237c37', domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('af2ad435-506e-495a-84cf-6ae014237c37', foundational, kami_buddha_ontological_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinctness, holdable).
narrative_ontology:cs_axiom('af2ad435-506e-495a-84cf-6ae014237c37', foundational, institutional_coordination_without_systematic_theology).
narrative_ontology:cs_axiom_status(institutional_coordination_without_systematic_theology, holdable).
narrative_ontology:cs_reference_frame('af2ad435-506e-495a-84cf-6ae014237c37', traditional_coexistence_unintegrated).
narrative_ontology:cs_drift_state('af2ad435-506e-495a-84cf-6ae014237c37', meiji_modernization_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parallel_entities_reading, institutional_shrine_temple_system).
narrative_ontology:constraint_beneficiary(parallel_entities_reading, ritual_specialists_dual_authority).
narrative_ontology:constraint_victim(parallel_entities_reading, doctrinal_coherence_seekers).
narrative_ontology:constraint_victim(parallel_entities_reading, theological_systematization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(parallel_entities_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

constraint_indexing:constraint_classification(parallel_entities_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(parallel_entities_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(parallel_entities_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(parallel_entities_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

constraint_indexing:constraint_classification(parallel_entities_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parallel_entities_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parallel_entities_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parallel_entities_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(parallel_entities_reading, TR),
    TR >= 0.70.

:- end_tests(parallel_entities_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The parallel-entities reading coordinates a genuine problem—how to enable dual religious participation without forcing doctrinal choice. This coordination function would ordinarily classify as Rope (ε ≤ 0.45). However, the constraint also extracts by suppressing doctrinal coherence work and displacing the incoherence burden onto individual practitioners. The extractiveness value reflects that the constraint has real benefits (practitioners experience genuine flexibility and institutional actors experience reduced conflict) but also real costs (theoretical work is suppressed, intellectual legitimacy is denied to coherence-seeking projects). Suppression (0.42): Moderate-high. Institutional barriers to doctrinal inquiry include: active discouragement of systematic theology, framing coherence-seeking as 'un-Japanese' or pedantic, absence of institutional reward for theological work, and concentration of authority in administrative/ritual specialists rather than philosophers. Not total suppression—some marginal theological projects exist—but significant enough to make coherence-seeking costly. Theater ratio (0.58): Moderate-high. Meiji-era state enforcement created performative distinction between Shinto and Buddhism (visible in shrine-temple administrative separation, state protocol, ritual hierarchy) that persists ceremonially while the underlying coordination logic remains unchanged. Contemporary shrine-temple system performs the distinction without believing in the hierarchy. Postwar pluralism reframed the theater as 'legitimate diversity' rather than 'forced separation,' reducing perceived performativity but not eliminating it. Claimed type (Tangled Rope): The constraint exhibits both coordination function (enabling dual participation) and asymmetric extraction (suppression of coherence work, displacement of incoherence burden). It requires active enforcement (institutional authority to suppress alternatives) and includes beneficiaries (shrine-temple system, dual-authority specialists) and victims (coherence seekers, practitioners managing incoherence).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival heterogeneity. Institutional actors (shrine-temple system, dual-authority priests) see Rope: pure coordination enabling their professional roles without conflict. Practitioners see Tangled Rope: genuine flexibility in ritual participation but also cognitive burden of managing incoherence. Doctrinal coherence seekers see Snare: institutional suppression of the very intellectual work that would resolve their discomfort, with no exit. The postwar pluralism consensus sees Scaffold: a temporary arrangement being replaced by explicit pluralism paradigm and secular choice, with sunset implicit in generational shift. The Meiji regime saw hierarchy (failed attempt to impose domain partition). The analytical observer sees Tangled Rope at global scope, but risks false summit at civilizational scope if naturalizing the arrangement as law. The perspectival gap between beneficiaries and victims is acute: what the institution experiences as functional coordination, doctrinal seekers experience as enforced intellectual deprivation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position within the extraction flow. Beneficiaries with arbitrage options (shrine-temple administrative system with domain-specific authority roles) experience low d, receiving positive benefit from the constraint's coordination function. Trapped coherence seekers (powerless, no exit from institutional framework) experience high d, bearing extraction cost. Moderate practitioners experience mid-range d, gaining flexibility benefit but paying cognitive and labor costs. Institutional ritual specialists are constrained but not trapped—they could theoretically reject the parallel reading, but professional identity is partially fused with its maintenance, raising the cost of exit. The analytical perspective derives d from the observation that the constraint benefits identifiable institutional actors and extracts from identifiable groups, ruling out pure natural law (mountain) classification. The false summit risk is high at the civilizational/global scope: there is temptation to naturalize this as 'the Japanese way' or 'inherent to Asian religious traditions,' which would be naturalizing a contingent institutional arrangement benefiting specific actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The parallel-entities reading resolves mandatrophy by demonstrating that the constraint is neither pure coordination nor pure extraction, but a hybrid with asymmetric benefits. The constraint cannot be classified as Rope alone because it actively suppresses alternative readings and displaces incoherence burden. It cannot be classified as Snare alone because it genuinely enables dual participation and reduces institutional conflict. Tangled Rope captures both: the institutional coordination function plus the asymmetric extraction from coherence seekers. The mandatrophy is resolved by attending to WHO experiences the constraint as coordination (institutional actors, practitioners enjoying flexibility) and WHO experiences it as extraction (theorists, coherence seekers). The constraint is both simultaneously from different structural positions—this is not contradiction but accurate perspectival mapping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parallel_vs_fused_boundary,
    'Is the distinction between ''parallel coexistence'' and ''fusion under interpretive layer'' an ontological reality or a rhetorical choice?',
    'Textual analysis of shrine and temple documents, priestly education curricula, and ritual manuals to identify whether kami-buddha relationship is explicitly theorized as distinct or subsumed under unified metaphysical framework. Historical comparison: did pre-Meiji sources use ''parallel'' language, or is ''parallel'' a modern reconstruction?',
    'If parallel is genuine historical category: this reading is not false summit, but authentic institutional understanding. If parallel is modern retro-projection: constraint is partially theater (performative parallelism covering underlying fusion or partition). Classification unchanged, but mandate shifts from ''explaining real coexistence'' to ''explaining ideological reconstruction''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parallel_vs_fused_boundary, empirical, 'Whether parallel-entities framing is historical or modern reconstruction').

omega_variable(
    suppression_mechanism_institutional_vs_cognitive,
    'Is the suppression of doctrinal coherence questions structural (institutional barriers to theological work) or cognitive (practitioners genuinely do not experience the incoherence as problematic)?',
    'Ethnographic data: do shrine-temple practitioners articulate theoretical discomfort with kami-buddha relationship? Are there suppressed or marginalized doctrinal projects (e.g., Buddhist philosophers attempting systematic integration)? Historical record: have any institutional actors actively discouraged or prohibited coherence-seeking theology?',
    'If structural: suppression metric (0.42) is accurate; victims are real (coherence seekers). If cognitive: suppression is lower; victims are self-selected theorists; constraint may be closer to Rope (coordination without experienced coercion). If both: confirm current Tangled Rope assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_vs_cognitive, empirical, 'Whether suppression of coherence questions is structural or experienced as absent').

omega_variable(
    identity_locked_institutional_commitment,
    'Do institutional actors (Shinto priests, Buddhist monks, shrine-temple administrators) maintain the parallel-entities reading because it genuinely coordinates their practice, or because their professional identity is fused with the incoherence-as-virtue framing?',
    'Survey/interview data: would institutional actors accept a unified ontology if presented without career risk? Historical counterfactual: did any institutional reform movements propose consolidation, and what opposition did they face? Textual analysis: do institutional sources articulate the parallel framing as chosen coordination strategy or as natural/necessary arrangement?',
    'If genuinely coordinating: Rope classification for institutional perspective is correct. If identity-locked: institutional actors are structurally mobile (could theoretically accept unified ontology) but cognitively captured by fusion of professional identity with incoherence. Would shift institutional perspective to identity_locked exit option; may increase suppression metric if the professional identity fusion blocks exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_institutional_commitment, empirical, 'Whether institutional commitment to parallel reading is chosen coordination or identity fusion').

omega_variable(
    reading_kernel_ontological_status,
    'This reading instantiates ''parallel coexistence.'' What is the kernel claim being read? Is it an empirical claim about historical kami-buddha relationship, a normative claim about legitimate coexistence, or an interpretive claim about how to understand sources?',
    'Textual archaeology: track which primary sources support ''parallel'' framing vs. ''fusion'' vs. ''partition''. Identify whether early Japanese religious discourse used ''parallel'' language or whether it is modern theorist''s term. Distinguish empirical claim (kami and buddhas were always understood as separate) from normative claim (they should be understood as separate) from interpretive claim (this source, read carefully, reveals parallel logic).',
    'If empirical claim: this reading competes with fused_ontology_reading and domain_partition_reading as alternative historical fact. Relation to siblings: coexists_with (different scholars hold different empirical claims). If normative claim: relation to siblings may shift to forecloses or influences (normative commitments can rule out or constrain alternatives). If interpretive claim: all three readings coexist as legitimate hermeneutic options; relation to siblings: coexists_with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ontological_status, conceptual, 'The ontological status of the parallel-entities kernel claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parallel_entities_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_meiji_prestate, parallel_entities_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_meiji_stateenforcement, parallel_entities_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(theater_postwar_contemporary, parallel_entities_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_premodernity, parallel_entities_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(extract_meiji_modernization, parallel_entities_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(extract_postwar_pluralism, parallel_entities_reading, base_extractiveness, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parallel_entities_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(parallel_entities_reading, 0.12).
narrative_ontology:affects_constraint(parallel_entities_reading, fused_ontology_reading).
narrative_ontology:affects_constraint(parallel_entities_reading, domain_partition_reading).
narrative_ontology:affects_constraint(parallel_entities_reading, syncretism_vs_pluralism_boundary).
narrative_ontology:affects_constraint(parallel_entities_reading, meiji_state_shinto_ideology).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel decomposes into three structurally distinct readings: parallel_entities_reading (ε=0.35, this file), fused_ontology_reading (ε≈0.25-0.30, separate file), and domain_partition_reading (ε≈0.40-0.45, separate file). Each reading has its own beneficiary/victim structure, institutional logic, and extractiveness value. The parallel reading solves coordination by avoiding forced choice; the fused reading solves it through metaphysical integration; the partition reading solves it through functional allocation. All three are live interpretive options in contemporary Japan. They coexist without one ruling out the others—this is not a case where new evidence will settle which is 'correct,' but rather a case where different institutional actors and scholarly communities maintain different readings as legitimate. Network edges indicate structural influence: e.g., the parallel reading influences the partition reading by making partition seem unnecessary, and influences fused reading by avoiding the metaphysical commitments fusion requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parallel_entities_reading, institutional, 0.15).
constraint_indexing:directionality_override(parallel_entities_reading, analytical, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
