% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_theological_fragmentation, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation Theological Fragmentation: Soteriological Incompatibility and Denominational Extraction
 *   domain: religious/epistemological/political
 *
 * SUMMARY:
 *   The Protestant Reformation is presented here as fundamentally a
 *   theological event driven by soteriological and ecclesiological
 *   incompatibilities. Luther's doctrine of sola fide, Calvin's
 *   predestination theology, and the Reformed assertion of sola scriptura
 *   create truth claims that cannot coexist with Catholic sacramental and
 *   hierarchical theology within a single coherent doctrinal system. The
 *   constraint is the fragmentation of Christian doctrine into
 *   denominationally bounded, mutually incompatible truth claims. This
 *   reading emphasizes doctrinal pluralism as the primary observable:
 *   confessional documents (Augsburg Confession, Heidelberg Catechism,
 *   Westminster Standards) formalize and crystallize the theological
 *   boundaries. Denominational leadership benefits from the fragmentation
 *   because their institutional legitimacy and authority depend on defending
 *   the theological distinctiveness they establish. The unified Catholic
 *   faithful and excluded radical reformers bear the costs of doctrinal
 *   enforcement without participating in the agenda-setting.
 *
 * KEY AGENTS:
 *   - Reformation denominational leadership (Luther, Calvin, Zwingli, Melanchthon, Beza) — agenda-setters, establish doctrinal boundaries and confessional documents
 *   - Reformed theological communities (Lutheran, Reformed, Anabaptist congregations) — beneficiaries of new soteriological frameworks, also bearers of schism costs
 *   - Unified Catholic faithful — payersexperiencing loss of doctrinal unity and institutional coherence
 *   - Radical reformers and dissident lay populations — excluded victims, suppressed from theological discourse
 *   - Roman Catholic institutional hierarchy — payer, loses monopoly on doctrinal authority
 *   - Secular political authorities (princes, nation-states) — excluded from doctrinal agenda-setting but enabled by theological fragmentation to assert sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.68).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.72).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation Theological Fragmentation: Soteriological Incompatibility and Denominational Extraction").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "religious/epistemological/political").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '098b0c82-da90-4574-976c-45393b2634df').
narrative_ontology:cs_kernel_codification('098b0c82-da90-4574-976c-45393b2634df', formalized).
narrative_ontology:cs_authority_grounding('098b0c82-da90-4574-976c-45393b2634df', lineage).
narrative_ontology:cs_interpretation_layer_present('098b0c82-da90-4574-976c-45393b2634df').
narrative_ontology:cs_reading_relation('098b0c82-da90-4574-976c-45393b2634df', reformation_composite__reformation_political_realignment, influences).
narrative_ontology:cs_reading_relation('098b0c82-da90-4574-976c-45393b2634df', reformation_composite__reformation_technological_mediation, influences).
narrative_ontology:cs_axiom('098b0c82-da90-4574-976c-45393b2634df', foundational, soteriological_incompatibility_doctrine).
narrative_ontology:cs_axiom_status(soteriological_incompatibility_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('098b0c82-da90-4574-976c-45393b2634df', soteriological_incompatibility_doctrine, deontological).
narrative_ontology:cs_axiom('098b0c82-da90-4574-976c-45393b2634df', foundational, confessional_crystallization_necessity).
narrative_ontology:cs_axiom_status(confessional_crystallization_necessity, holdable).
narrative_ontology:cs_axiom_grounding('098b0c82-da90-4574-976c-45393b2634df', confessional_crystallization_necessity, instrumental).
narrative_ontology:cs_reference_frame('098b0c82-da90-4574-976c-45393b2634df', unified_medieval_catholicism_framework).
narrative_ontology:cs_drift_state('098b0c82-da90-4574-976c-45393b2634df', post_reformation_consolidation_1648, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('098b0c82-da90-4574-976c-45393b2634df', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, reformation_denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, reformed_theological_communities).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, unified_catholic_faithful).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, dissident_lay_populations_excluded_from_doctrine).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.32 in 1517) because the theological incompatibilities are not yet crystallized into formal denominational boundaries and extraction mechanisms; they exist as intellectual and spiritual dissent. By 1530 (0.42), as confessional documents emerge and denominational leadership consolidates power, extractiveness rises — the constraint now actively enforces doctrinal boundaries and excludes alternative voices. The rise continues through 1545 (0.55, Council of Trent crystallizes Catholic response) and plateaus by 1600 (0.67) as the denominational system stabilizes and extraction becomes routine enforcement of doctrinal conformity. Theater ratio rises steadily (0.15 to 0.41) as the theological debate increasingly takes the form of scholastic performance, confessional polemic, and institutional theater rather than genuine doctrinal exploration — by 1648 (Peace of Westphalia), much of the constraint's operation is maintaining performative denominational boundaries rather than resolving theological questions. Suppression requirement rises sharply (0.42 to 0.72) because maintaining doctrinal incompatibility requires active enforcement: doctrinal discipline, excommunication, political alliance with secular powers to enforce religious conformity, and systematic exclusion of heterodox voices. All three metrics follow one shared time grid so no metric is missing from any examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the denominational leadership seat, the constraint appears as genuine theological coordination: the articulation of coherent truth claims about salvation and the Church, the formalization of those claims in confessional documents, the gathering of communities around shared doctrinal understanding. From the unified Catholic faithful and excluded radical reformer seats, the same structure operates as enforced doctrinal extraction: the imposition of theological truth claims without participation, the enforcement of boundaries through discipline and exclusion, the cost of schism imposed without choice. The divergence is structural, not a matter of perspective tuning — the leadership sets the agenda; the others bear the costs of whatever agenda emerges. The engine computes this seat-divergence from the authored structural data (roles, power, exit options); it is not predicted by the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformation denominational leadership occupies the beneficiary seat with organized power and identity-locked exit — they are fused with the denominational identity they establish and cannot exit without losing their role and authority. Their directionality is near 0.0 (full beneficiary). Reformed theological communities occupy a mixed seat: they are beneficiaries of the new soteriological frameworks (genuine coordination benefit) but payersof schism costs and ongoing enforcement; their directionality sits near 0.5 (symmetric). Unified Catholic faithful are payersof doctrinal fragmentation with powerless status and constrained exit — they experience the loss of doctrinal unity imposed without their participation. Their directionality is near 1.0 (full target). Dissident lay populations are trapped payersexcluded from agenda-setting; their directionality is 1.0 (full target). Secular authorities are excluded (not seated in the doctrinal exchange) but benefit from political side-effects; they are not targets of the theological extraction itself, and they sit outside the constraint's primary operation — their directionality for THIS constraint is not computed (they are excluded seats, not beneficiary or payer seats in the theological fragmentation).
 *
 * MANDATROPHY ANALYSIS:
 *   The theological fragmentation constraint must be classified as tangled_rope (hybrid coordination/extraction) rather than pure snare, because a genuine coordination function is present: the denominations do solve a real problem (articulating coherent doctrine where unity became impossible), and the reformed communities do benefit from the new soteriological frameworks they adopt. However, the constraint is also substantially extractive because the denominational leadership enforces doctrinal boundaries, excludes heterodox voices, and collects institutional legitimacy from the fragmentation they establish. The tension between coordination and extraction is the mandatrophy: if the constraint were purely extractive (snare), the beneficiaries would derive no real coordination value from it, only rents. But reformed congregations do adopt the new doctrine voluntarily (in many cases) and experience genuine theological coordination alongside the schism costs. If the constraint were pure coordination (rope), there would be no identifiable victims — but the unified Catholic faithful and excluded radicals do bear real costs without participating in the agenda. The tangled_rope classification captures both: coordination function + asymmetric extraction + active enforcement to maintain both the coordination and the extraction boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_vs_politics_causal_primacy,
    'Is the Reformation''s theological fragmentation the PRIMARY causal driver, or is it a rationalization of political realignment by emerging nation-states?',
    'Counterfactual historical analysis: would theological incompatibilities have generated permanent denominational boundaries without the political support of princes and nation-states? Would political realignment have occurred without the theological justification the Reformation provided?',
    'If theology is primary, the constraint is the incompatibility of soteriologies and ecclesiologies driving fragmentation; if politics is primary, theology is a secondary effect. The ε value and beneficiary structure change: theological reading has denominational leaders as beneficiaries; political reading has nation-states as beneficiaries. Classification type remains tangled_rope (extraction + coordination) under either, but with different extractive mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theology_vs_politics_causal_primacy, conceptual, 'Whether doctrinal incompatibility or political realignment is the primary causal driver of denominational fragmentation.').

omega_variable(
    theology_vs_technology_diffusion_mechanism,
    'Would soteriological incompatibilities have generated lasting denominational boundaries without the printing press enabling mass circulation of confessional documents and theological polemic?',
    'Comparison to pre-print theological dissents (Wycliffe, Hus, Waldensian movements): did they generate sustained alternative denominations, or were they suppressed and reintegrated? Did post-Gutenberg confessional documents enable durability that pre-print dissent lacked?',
    'If printing is essential to diffusion, the theological incompatibilities alone might have generated only ephemeral movements; the printing press amplifies theological dissent into structural fragmentation. If theology is sufficient, printing merely accelerates an inevitable process. The ε value for the theological constraint depends on whether the incompatibilities are intrinsic or require technological amplification to manifest extractively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_vs_technology_diffusion_mechanism, empirical, 'Whether printing technology is necessary to convert theological incompatibilities into lasting denominational fragmentation.').

omega_variable(
    doctrinal_incompatibility_metaphysical_vs_constructed,
    'Are the soteriological and ecclesiological incompatibilities between reformed and Catholic theology METAPHYSICALLY real (one set of claims is objectively true; the other objectively false), or are they SOCIALLY CONSTRUCTED as incompatible through the act of denominational boundary-setting?',
    'Logical analysis of the doctrinal claims: can sola fide and Catholic justification-through-sacraments both be true under some coherent metaphysical framework? If yes, the incompatibility is constructed; if no, it is intrinsic. Ecumenical theology attempts to show reconciliation paths (both Catholic-Lutheran Joint Declaration on Justification); does their success show constructed incompatibility or shallow resolution of deep division?',
    'If intrinsic, the constraint arises from the objective structure of theological truth; the fragmentation is inevitable and natural. If constructed, the denominational leadership benefits from asserting incompatibility even where compromise might be possible. This affects whether the constraint should be classified as mountain (natural doctrinal limit) or tangled_rope (constructed extraction). Currently classified as tangled_rope assuming constructed incompatibility; if intrinsic, reclassify to mountain (natural theological incompatibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_incompatibility_metaphysical_vs_constructed, conceptual, 'Whether soteriological incompatibilities are metaphysically real or socially constructed through denominational boundary-setting.').

omega_variable(
    radical_reformation_exclusion_mechanism,
    'Is the exclusion and suppression of radical reformers (Anabaptists, Spiritualists, antitrinitarians) a necessary consequence of maintaining coherent denominational theology, or is it an arbitrary exercise of power by mainline Reformation leadership?',
    'Doctrinal analysis: do radical theological positions (believer baptism, separation of church and state, antitrinitarianism) create genuine incompatibilities with Lutheran or Reformed theology that require exclusion? Or are they suppressed primarily for political stability reasons?',
    'If necessary, the suppression is a cost of maintaining theological coherence; if arbitrary, it is pure coercive enforcement of ideological boundaries without doctrinal justification. This affects the magnitude of victimization (dissident_lay_populations) and whether the constraint''s extractiveness is justified by genuine coordination necessity or is pure rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(radical_reformation_exclusion_mechanism, conceptual, 'Whether radical-reformer exclusion is necessitated by doctrinal coherence or exercised as arbitrary power.').

omega_variable(
    committer_constraint_multiplicity,
    'The Reformation is described by three distinct readings (theological, political, technological). Are these three readings of ONE constraint (one ε, three interpretive frames), or three DIFFERENT constraints (three distinct ε values, three distinct structural configurations)?',
    'Apply ε-invariance principle: does measuring the Reformation''s theological fragmentation produce a different ε than measuring its political realignment function? If yes (different observable → different ε), the readings are separate constraints. If no (same ε regardless of observable), they are three interpretations of one constraint.',
    'If separate constraints: author three JSON files (theological, political, technological), each with its own ε, beneficiary structure, and metrics; link via network.affects_constraints. If one constraint: author a single JSON with three interpretive frames (less likely given this prompt''s specification that the reading is ''theological fragmentation'' with expected structural delta of ''confessional documents as artifacts; denominational leadership as beneficiary''). DECISION: treating as three separate constraints based on structural delta specification; this JSON generates the theological reading only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_constraint_multiplicity, conceptual, 'Whether the three Reformation readings are interpretations of one constraint or separate constraints with different ε values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_theofrag_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(reformation_theofrag_tr_t1530, reformation_composite__theological_fragmentation_reading, theater_ratio, 1530, 0.22).
narrative_ontology:measurement(reformation_theofrag_tr_t1545, reformation_composite__theological_fragmentation_reading, theater_ratio, 1545, 0.28).
narrative_ontology:measurement(reformation_theofrag_tr_t1560, reformation_composite__theological_fragmentation_reading, theater_ratio, 1560, 0.33).
narrative_ontology:measurement(reformation_theofrag_tr_t1600, reformation_composite__theological_fragmentation_reading, theater_ratio, 1600, 0.39).
narrative_ontology:measurement(reformation_theofrag_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.41).

% Extraction over time
narrative_ontology:measurement(reformation_theofrag_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.32).
narrative_ontology:measurement(reformation_theofrag_be_t1530, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1530, 0.42).
narrative_ontology:measurement(reformation_theofrag_be_t1545, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1545, 0.55).
narrative_ontology:measurement(reformation_theofrag_be_t1560, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1560, 0.61).
narrative_ontology:measurement(reformation_theofrag_be_t1600, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1600, 0.67).
narrative_ontology:measurement(reformation_theofrag_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(reformation_theofrag_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.42).
narrative_ontology:measurement(reformation_theofrag_su_t1530, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1530, 0.58).
narrative_ontology:measurement(reformation_theofrag_su_t1545, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1545, 0.64).
narrative_ontology:measurement(reformation_theofrag_su_t1560, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1560, 0.68).
narrative_ontology:measurement(reformation_theofrag_su_t1600, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1600, 0.71).
narrative_ontology:measurement(reformation_theofrag_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_composite__theological_fragmentation_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_political_realignment).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_technological_mediation).

% DUAL FORMULATION NOTE:
% The Reformation kernel admits three structurally distinct readings, each with a different primary observable and beneficiary structure. This constraint (theological_fragmentation_reading) emphasizes doctrinal pluralism and soteriological incompatibility as primary; it identifies denominational leadership and reformed communities as beneficiaries. The political_realignment_reading emphasizes sovereignty assertion and national differentiation; it identifies nation-states as primary beneficiaries. The technological_mediation_reading emphasizes printing infrastructure and mass communication; it identifies printers, publishers, and literate urban classes as beneficiaries. These are three distinct constraints (not three frames of one constraint) because the ε-invariance principle distinguishes them: measuring theological fragmentation yields a different ε than measuring political realignment or technological diffusion. Each constraint has its own beneficiary/victim structure, its own metrics, and its own classification. They are linked via this network.affects_constraints array because the Reformation's structural integrity depends on the interaction of all three: theology without politics might not crystallize into lasting denominations; politics without theology lacks legitimation; technology without both would merely multiply ephemeral texts. The three readings coexist in the historical record; no single reading is complete, but each reading is structurally coherent on its own.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
