% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: R2P Intervention Trigger: Sovereignty Conditional on Protection
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   The Responsibility to Protect (R2P) reading of the Article 2(7)/Chapter
 *   VII tension holds that state sovereignty entails a responsibility to
 *   protect populations from four crimes (genocide, war crimes, ethnic
 *   cleansing, crimes against humanity). When a state manifestly fails this
 *   responsibility, the international community has a duty to intervene —
 *   diplomatically, economically, and militarily — authorized by the UN
 *   Security Council. This reading emerged from the 2001 ICISS report and was
 *   endorsed at the 2005 World Summit. It reinterprets sovereignty from a
 *   shield (non-interference) to a conditional status (protection
 *   obligation). The constraint operates as a tangled rope: it coordinates
 *   collective atrocity response (genuine coordination function) while
 *   extracting sovereignty, territorial integrity, and political survival
 *   from targeted states (asymmetric extraction). The Libya 2011 intervention
 *   — authorized under R2P but extending to regime change — crystallized the
 *   extraction dynamic and triggered sustained resistance from sovereignty
 *   advocates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.75).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.78).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "R2P Intervention Trigger: Sovereignty Conditional on Protection").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '906361f7-04b3-4bd2-9cb4-daa19cb8c498').
narrative_ontology:cs_kernel_codification('906361f7-04b3-4bd2-9cb4-daa19cb8c498', formalized).
narrative_ontology:cs_authority_grounding('906361f7-04b3-4bd2-9cb4-daa19cb8c498', lineage).
narrative_ontology:cs_interpretation_layer_present('906361f7-04b3-4bd2-9cb4-daa19cb8c498').
narrative_ontology:cs_reading_relation('906361f7-04b3-4bd2-9cb4-daa19cb8c498', article_2_7_chapter_vii_tension__sovereignty_first_reading, forecloses).
narrative_ontology:cs_axiom('906361f7-04b3-4bd2-9cb4-daa19cb8c498', foundational, sovereignty_conditional_on_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('906361f7-04b3-4bd2-9cb4-daa19cb8c498', sovereignty_conditional_on_protection, empirically_contingent).
narrative_ontology:cs_axiom('906361f7-04b3-4bd2-9cb4-daa19cb8c498', foundational, intervention_legitimacy_trigger).
narrative_ontology:cs_axiom_status(intervention_legitimacy_trigger, holdable).
narrative_ontology:cs_axiom_grounding('906361f7-04b3-4bd2-9cb4-daa19cb8c498', intervention_legitimacy_trigger, deontological).
narrative_ontology:cs_reference_frame('906361f7-04b3-4bd2-9cb4-daa19cb8c498', post_westphalian_protection_framework).
narrative_ontology:cs_drift_state('906361f7-04b3-4bd2-9cb4-daa19cb8c498', post_libya_2011, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('906361f7-04b3-4bd2-9cb4-daa19cb8c498', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, international_community).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_intervention_legitimacy).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_as_responsibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations facing systematic atrocities (genocide, war crimes, ethnic cleansing, crimes against humanity) by their own state or non-state actors the state cannot or will not control. They have no effective exit; their survival depends on external intervention. The constraint's coordination function is nominally for their protection.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% States accused of perpetrating or failing to halt mass atrocities. They bear the extraction: loss of territorial integrity, political sovereignty, leadership survival, and control over resources. Exit from the constraint means ceasing atrocity behavior, which may be regime-threatening. Their constrained exit reflects that compliance requires conceding power they are fighting to retain.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_states, payer,
    powerful, biographical, constrained, national).

% The UN Security Council (especially P5), regional organizations (AU, EU, OAS), and coalitions of willing states that authorize, mandate, or execute interventions. They set the agenda for when R2P is invoked, control the legitimation machinery, and benefit from a normative framework that converts political will into legal authority. Their arbitrage-grade exit reflects ability to selectively invoke or ignore the norm.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_community, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, international_community, beneficiary).

% States and legal traditions (strongly represented in the Non-Aligned Movement, G77, and some permanent UNSC members) that hold sovereignty as near-absolute and view R2P as a Western-driven erosion of the UN Charter's Article 2(7). They are structurally excluded from the norm's core drafting and authorization moments despite representing a majority of UN membership. Their constrained exit reflects inability to unmake the norm once institutionalized.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_advocates, excluded,
    organized, generational, constrained, global).

% International lawyers, political philosophers, and IR scholars who analyze the constraint's coherence, legitimacy, and empirical track record. They do not bear extraction nor collect rents; their exit is analytical — they can change their reading without material cost. Their situation is epistemic: mapping the fault lines between the r2p_reading and sovereignty_first_reading.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating collective military and political action to halt or avert mass atrocities when the territorial state is the perpetrator or is unable/unwilling to protect, converting a moral impulse into a structured decision procedure (UNSC authorization, regional organization mandate, or coalition of the willing).
% TRANSFER_FUNCTION: Transfers the authority to use force across borders from the sovereign state to international institutions or coalitions; transfers the material burden of intervention (troops, treasure, political capital) to intervening actors; transfers the risk of escalation and post-intervention instability to the targeted state's population and region.
% ABSENT_VOICES: Populations in targeted states who may oppose foreign intervention (e.g., nationalist segments, groups fearing post-intervention chaos); non-Western states excluded from the ICISS drafting process and subsequent norm-socialization; victims of interventions that exceeded mandate (Libya 2011) who have no formal seat in R2P governance.
% DISAPPEARANCE_RATIONALE: If the R2P reading vanished overnight, the normative license for cross-border military action against sovereign atrocity perpetrators would collapse to ad hoc humanitarian intervention claims, UNSC vetoes would face no structured counter-norm, and atrocity response would revert to political contingency — the 2005 World Summit consensus would be undone.
% FOUNDING_PROBLEM: How to respond to sovereign-perpetrated mass atrocities (Rwanda 1994, Srebrenica 1995, Kosovo 1999) without either paralyzing the international system with absolute non-interference or licensing unlimited great-power intervention under humanitarian pretext.
% FOUNDING_PROBLEM_CORROBORATION: The ICISS report (2001) and 2005 World Summit Outcome Document attest the founding problem as live. The Non-Aligned Movement, G77, and several permanent UNSC members (Russia, China) contest it, arguing the problem was manufactured to legitimize regime change and that existing Chapter VII tools sufficed. No external corroboration resolves the dispute; the founding problem's status is itself a battlefield.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint legitimizes the most extreme coercion — cross-border military force — against sovereign entities, transferring authority and survival stakes. Suppression (0.78) is higher still because the constraint's persistence depends on active enforcement: UNSC resolutions, peacekeeping mandates, sanctions regimes, and the institutional machinery that makes intervention legally and politically possible. Theater ratio (0.38) reflects that the protection mandate is real but increasingly performed: pillar three (rebuild) is consistently under-resourced, and invocation selectivity tracks great-power interest. Accessibility collapse (0.55) is moderate because alternatives exist (diplomatic pressure, sanctions, ICC referral) but are structurally subordinated to the military option once R2P is invoked. Resistance (0.72) is high from sovereignty advocates who block UNSC authorization (Syria vetoes) and contest the norm in UNGA.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from persecuted_populations' seat, the constraint appears as rope (genuine coordination, they are beneficiaries, low extraction). From targeted_states' seat, it appears as snare (pure extraction, no coordination benefit, high suppression). From international_community's seat, it appears as tangled_rope (they both coordinate and extract via selective invocation). From sovereignty_advocates' seat, it appears as snare (their norm — sovereignty — is being extracted). The analytical observer sees the full tangled_rope structure. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations are full beneficiaries (d ≈ 0.0): the constraint exists nominally for their protection, they bear no cost, and their exit is trapped (they cannot leave the atrocity situation). Targeted states are full targets (d ≈ 1.0): they bear the full extraction (sovereignty loss, regime threat), their exit is constrained (compliance requires conceding power), and their power is high but ineffective against UNSC authorization. International community (UNSC/P5) sits near beneficiary (d ≈ 0.15): they control the trigger, collect normative authority, and have arbitrage exit (selective invocation). Sovereignty advocates are excluded (d derivation reverts to fallback): they are not in the authorization room but bear normative costs. Legal scholars are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (responding to sovereign atrocities without licensing unlimited intervention) remains live — atrocities persist (Myanmar, Sudan, Xinjiang, Gaza debates). But the arrangement's mandate has atrophied in two ways: (1) the 'responsibility to rebuild' pillar is systematically abandoned, converting the constraint into a regime-change license; (2) selectivity has made the coordination function unreliable for populations without great-power patrons. The constraint is not a piton because the agenda_setter (UNSC/P5) still actively maintains it for its utility — it is not inertial. But it risks mandatrophy if pillar three collapse continues and the norm becomes purely an intervention trigger without post-intervention obligation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the r2p_reading a distinct constraint from the sovereignty_first_reading, or a reinterpretation of the same Article 2(7)/Chapter VII tension?',
    'Structural decomposition: if the two readings produce different beneficiary/victim structures, different ε values, and different stakeholder power mappings across the same interval, they are distinct constraints linked by network.affects_constraints.',
    'If distinct, each reading gets its own classification (r2p_reading = tangled_rope; sovereignty_first_reading likely = mountain or rope). If same constraint, the framework must model observable-dependent classification — which it rejects by design (ε-invariance principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s two readings instantiate one constraint or two.').

omega_variable(
    sibling_reading_delta,
    'What would the sovereignty_first_reading''s ε, beneficiaries, and victims look like?',
    'Author the sibling constraint story (article_2_7_chapter_vii_tension__sovereignty_first_reading) and compare: sovereignty_first_reading should have low ε (~0.15), beneficiaries = international_order/stability, victims = none (or intervened-upon states if intervention occurs), claimed_type = mountain or rope.',
    'The structural delta (high ε + persecuted_populations as beneficiaries + targeted_states as victims for r2p_reading vs. low ε + order beneficiaries for sovereignty_first_reading) confirms they are distinct constraints. The network edge documents the contamination risk: r2p_reading''s extraction degrades the sovereignty norm the sibling treats as mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, empirical, 'Expected structural profile of the sibling reading.').

omega_variable(
    disagreement_location,
    'Where exactly do the readings diverge structurally — on the trigger threshold, the authorization mechanism, the post-intervention obligation, or the sovereignty concept itself?',
    'Map each reading''s axioms (see cs_structure.axioms). The r2p_reading''s foundational axiom ''sovereignty_conditional_on_protection'' directly contradicts sovereignty_first_reading''s ''sovereignty_as_absolute_noninterference''. The divergence is at the sovereignty concept level, not procedural details.',
    'Concept-level divergence (forecloses relation) means no single legal framework can hold both readings simultaneously — a state cannot be both absolutely sovereign and conditionally sovereign. This validates forecloses over coexists_with for the reading_relations edge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'Structural locus of the kernel''s contestation.').

omega_variable(
    intervention_selectivity,
    'Is the constraint''s extraction amplified by selective invocation (Libya yes, Syria no; Kosovo yes, Chechnya no)?',
    'Compare intervention cases 2001-2024: code for UNSC authorization, regional mandate, coalition willingness, and outcome. If extractiveness correlates with great-power interest rather than atrocity severity, the coordination function is cover for selective extraction.',
    'If selectivity is structural, the constraint''s effective extraction for targeted_states is higher than base ε suggests (great-power targets face lower intervention probability). This would push classification toward snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_selectivity, empirical, 'Whether selective enforcement makes the constraint a snare for some targets.').

omega_variable(
    post_intervention_accountability,
    'Does the constraint include a genuine post-intervention obligation (reconstruction, justice, stable governance) or does extraction end at regime removal?',
    'Track post-intervention outcomes (Kosovo, Libya, Ivory Coast, Mali) against R2P''s third pillar (responsibility to rebuild). Measure gap between mandate language and resource commitment.',
    'If pillar three is consistently unfunded/unfulfilled, the theater_ratio is understated — the coordination story (protection) collapses after the extraction event (intervention). This would increase theater_ratio toward piton territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_intervention_accountability, empirical, 'Whether the constraint''s coordination function persists post-extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(r2p_tr_t2001, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(r2p_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(r2p_tr_t2011, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(r2p_tr_t2013, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2013, 0.32).
narrative_ontology:measurement(r2p_tr_t2024, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(r2p_be_t2001, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2001, 0.25).
narrative_ontology:measurement(r2p_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(r2p_be_t2011, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2011, 0.72).
narrative_ontology:measurement(r2p_be_t2013, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement(r2p_be_t2024, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(r2p_su_t2001, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2001, 0.35).
narrative_ontology:measurement(r2p_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(r2p_su_t2011, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2011, 0.78).
narrative_ontology:measurement(r2p_su_t2013, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2013, 0.72).
narrative_ontology:measurement(r2p_su_t2024, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__r2p_reading, 0.12).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% This constraint and sovereignty_first_reading form a constraint family decomposing the Article 2(7)/Chapter VII tension. r2p_reading has high ε (0.75), beneficiaries = persecuted_populations, victims = targeted_states/sovereignty_norm, claimed_type = tangled_rope. sovereignty_first_reading has low ε (~0.15), beneficiaries = international_order/stability, victims = none (or intervened states if unauthorized intervention occurs), claimed_type = mountain or rope. The r2p_reading's extraction degrades the sovereignty norm that the sibling treats as mountain — this is the contamination edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
