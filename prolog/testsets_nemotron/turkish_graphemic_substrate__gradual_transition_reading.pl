% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Gradual Script Transition with Dual-Literacy Period
 *   domain: political/linguistic/state_formation
 *
 * SUMMARY:
 *   The gradual transition reading of the Turkish graphemic substrate kernel
 *   instantiates a managed dual-script period (1928-1950) as a scaffold: a
 *   temporary coordination structure with a declared sunset (the 1950
 *   endpoint where Latin script becomes exclusive). The constraint
 *   coordinates intergenerational knowledge transfer by maintaining
 *   Arabic-script literacy alongside the new Latin script, reducing the
 *   generational rupture that immediate enforcement would cause. However, the
 *   transition is actively enforced by the state, extracts asymmetric costs
 *   from Arabic-script practitioners and minority communities, and its
 *   coordination function degrades over time as the sunset approaches — the
 *   theater ratio rises as dual-literacy performance increasingly serves to
 *   legitimate the completed transition rather than to sustain genuine
 *   knowledge transfer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.42).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.38).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, scaffold).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Gradual Script Transition with Dual-Literacy Period").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political/linguistic/state_formation").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:has_sunset_clause(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '88616b63-554e-4aef-9809-d670d2d1bf9a').
narrative_ontology:cs_kernel_codification('88616b63-554e-4aef-9809-d670d2d1bf9a', formalized).
narrative_ontology:cs_authority_grounding('88616b63-554e-4aef-9809-d670d2d1bf9a', extraction).
narrative_ontology:cs_interpretation_layer_present('88616b63-554e-4aef-9809-d670d2d1bf9a').
narrative_ontology:cs_reading_relation('88616b63-554e-4aef-9809-d670d2d1bf9a', turkish_graphemic_substrate__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('88616b63-554e-4aef-9809-d670d2d1bf9a', turkish_graphemic_substrate__secular_nationalist_reading, influences).
narrative_ontology:cs_axiom('88616b63-554e-4aef-9809-d670d2d1bf9a', foundational, managed_transition_preserves_epistemic_continuity).
narrative_ontology:cs_axiom_status(managed_transition_preserves_epistemic_continuity, holdable).
narrative_ontology:cs_axiom_grounding('88616b63-554e-4aef-9809-d670d2d1bf9a', managed_transition_preserves_epistemic_continuity, instrumental).
narrative_ontology:cs_axiom('88616b63-554e-4aef-9809-d670d2d1bf9a', foundational, state_has_authority_to_engineer_script_change).
narrative_ontology:cs_axiom_status(state_has_authority_to_engineer_script_change, holdable).
narrative_ontology:cs_axiom_grounding('88616b63-554e-4aef-9809-d670d2d1bf9a', state_has_authority_to_engineer_script_change, conventional).
narrative_ontology:cs_reference_frame('88616b63-554e-4aef-9809-d670d2d1bf9a', ottoman_islamic_textual_continuity).
narrative_ontology:cs_drift_state('88616b63-554e-4aef-9809-d670d2d1bf9a', post_transition_republican_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('88616b63-554e-4aef-9809-d670d2d1bf9a', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, state_modernizers).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_bridge_generation).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, educational_institutions).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, arabic_script_practitioners).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, rural_isolated_communities).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, minority_language_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, educational_institutions).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_knowledge_continuity).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, managed_modernization_over_rupture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Direct the transition policy, set the sunset timeline, allocate educational resources for dual-script instruction. Hold executive and legislative authority to enforce the transition schedule. Benefit from successful modernization measured in literacy rates and European alignment.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, state_modernizers, agenda_setter,
    institutional, generational, arbitrage, national).

% The cohort educated during the transition period who achieve literacy in both scripts. They serve as translators, teachers, and cultural mediators. Their dual literacy is a genuine coordination gain but their labor is instrumentalized by the state apparatus. Exit is constrained by their invested identity as 'bridge' figures.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_bridge_generation, beneficiary,
    organized, biographical, constrained, national).

% Receive expanded mandate, funding, and curriculum authority to teach dual literacy. Bear the implementation costs of training teachers, producing materials in two scripts, and managing parallel examination systems. Their institutional survival depends on demonstrating successful transition outcomes.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, educational_institutions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, educational_institutions, payer).

% Religious scholars, calligraphers, manuscript custodians, and traditional educators whose professional identity and livelihood are fused with Arabic script. The transition renders their expertise obsolete on a state-decreed timeline. Exit is identity-locked: abandoning the script means abandoning the self-concept and communal role constituted through it. They bear extraction through professional erasure and cultural devaluation.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, arabic_script_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Communities with limited access to new educational infrastructure, where Arabic script remains the only functional literacy for religious and communal life. The transition's timeline is imposed from the center without regard for local readiness. They bear costs through exclusion from new literacy economies and loss of access to communal texts. Exit is trapped: geographic isolation and resource poverty prevent adaptation.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, rural_isolated_communities, payer,
    powerless, biographical, trapped, local).

% Kurdish, Armenian, Greek, and other minority communities whose languages were written in Arabic script or whose educational access is mediated through the state's transition apparatus. The transition accelerates assimilation by making minority literacy dependent on state-controlled dual-script infrastructure. They are excluded from agenda-setting about the transition terms that directly determine their cultural survival.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, minority_language_speakers, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, minority_language_speakers, excluded).

% Diplomatic, academic, and NGO actors monitoring the transition as a case study in script reform and cultural engineering. They document outcomes but hold no enforcement power over the Turkish state. Their analytical seat sees the full structural asymmetry between the state's coordination claims and the extraction borne by identity-locked and trapped populations.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, international_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of intergenerational knowledge transfer during a civilizational script change: how to preserve access to the entire Ottoman textual heritage (religious, legal, literary, scientific) while establishing a new literacy infrastructure aligned with European modernity. The dual-script period coordinates the handover by maintaining both systems simultaneously.
% TRANSFER_FUNCTION: Moves the cost of maintaining dual-script infrastructure (teacher training, parallel publishing, examination systems, administrative bilingualism) from the state to educational institutions and the bridge generation. Moves cultural authority from Arabic-script practitioners to state-certified dual-literacy mediators. Moves epistemic access from communities with only Arabic-script literacy to those who acquire the new Latin script.
% ABSENT_VOICES: Ottoman-era religious authorities (ulema) who were sidelined before the transition began; Kurdish and other minority intellectuals who advocated for Latin-script adaptation of their own languages rather than Turkish-only imposition; women in conservative households whose literacy access depended on Arabic-script religious education and who had no representative in the transition design.
% DISAPPEARANCE_RATIONALE: If the managed transition constraint vanished overnight, the state would face a binary choice: immediate compulsory Latin-script enforcement (causing mass literacy rupture and loss of Ottoman textual access) or abandonment of script reform (preserving Arabic script but forgoing the modernization coordination). The educational infrastructure, publishing industry, and generational literacy plans would collapse and reorganize around one of these poles.
% FOUNDING_PROBLEM: The Ottoman Empire's textual heritage in Arabic script constituted a civilizational knowledge base that would be severed by immediate script change, while the new Republic's modernization project required a script aligned with European scientific and administrative standards. The founding problem was how to achieve the latter without destroying access to the former.
% FOUNDING_PROBLEM_CORROBORATION: State modernizers (Atatürk's circle) attest the problem was real and the transition solved it, citing rising literacy rates and European integration. Arabic-script practitioners and minority communities attest the problem was manufactured by the same state that destroyed the Ottoman institutional order, and that the transition was a tool of cultural homogenization. Independent historians (e.g., Geoffrey Lewis, Niyazi Berkes) document both the genuine coordination challenge and the extractionary implementation.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__gradual_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__gradual_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__gradual_transition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(turkish_graphemic_substrate__gradual_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__gradual_transition_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).
:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects the asymmetric cost distribution: the state and bridge generation gain coordination value, while identity-locked practitioners and trapped rural/minority communities bear professional erasure and cultural exclusion. Suppression (0.38) is moderate — the constraint does not rely on brute coercion but on structural exclusion from the new literacy economy and state control of educational infrastructure. Theater ratio (0.22) captures the growing performative element: by the 1940s, dual-script instruction increasingly ritualizes the transition rather than substantively preserving Ottoman textual access. Accessibility collapse (0.35) is partial — alternatives (private Arabic-script education, minority-language publishing) persist but are marginalized. Resistance (0.58) is significant, reflecting sustained opposition from religious establishments, minority communities, and rural populations.
 *
 * PERSPECTIVAL GAP:
 *   From the state modernizer seat, the constraint appears as a successful scaffold: coordination achieved, sunset executed, modernization completed. From the Arabic-script practitioner seat, it is a snare: professional identity destroyed on a state timeline, no alternative livelihood, suppression through credential revocation and cultural devaluation. From the rural/minority seat, it is a tangled rope with broken coordination: the promised knowledge transfer never reaches them, only the extraction of their textual access. The engine computes this seat divergence from the structural data — the claimed type (scaffold) reflects the authoring seat's structural reading, not a consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   State modernizers (institutional, arbitrage exit) are structural beneficiaries: they set the agenda, control the timeline, and collect the modernization gains. The bridge generation (organized, constrained exit) benefits from dual literacy but is instrumentalized — their labor mediates the transition. Educational institutions (organized, constrained) are dual-positioned: they gain authority and resources but bear implementation costs. Arabic-script practitioners (moderate, identity-locked) are primary targets: their professional identity is fused with the script being phased out. Rural communities (powerless, trapped) bear costs through geographic and resource exclusion. Minority speakers (powerless, trapped) face accelerated assimilation. The analytical observer sees the full asymmetry: the state's coordination claim is genuine but the extraction is concentrated on those least able to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's founding problem (intergenerational knowledge continuity during script change) was structurally live at inception but became contested as the transition progressed. The sunset clause (1950) was formally met, but the mandate's resolution is contested: state modernizers declare success (literacy rates rose, European alignment achieved); practitioners and minorities declare failure (Ottoman textual access severed, cultural homogenization enforced). The scaffold did not atrophy into a piton — it completed its sunset — but the extraction patterns it established (state control of script, marginalization of Arabic-script knowledge, assimilationist pressure on minorities) persisted as structural legacies. The mandatrophy question is whether the transition's coordination function was genuine or a cover for extraction; the answer varies by seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_genuineness_vs_cover,
    'Was the dual-script period a genuine coordination mechanism for knowledge transfer, or a calculated cover for extractionary cultural engineering?',
    'Comparative analysis of state archives (educational directives, publishing records, teacher deployment) vs. community records (literacy outcomes, manuscript preservation, minority-language vitality) across the transition period. If state resources for dual-script instruction declined before the sunset while enforcement of Latin exclusivity intensified, the cover hypothesis gains support.',
    'If cover, the constraint reclassifies from scaffold toward snare/tangled_rope; the sunset clause becomes a planned extraction timeline rather than a genuine transition endpoint. If genuine, the scaffold claim holds but the extraction on identity-locked/trapped seats remains a structural injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_genuineness_vs_cover, conceptual, 'Whether the scaffold''s coordination function was genuine or instrumentalized for extraction.').

omega_variable(
    identity_lock_mechanism_arabic_practitioners,
    'Is the identity_locked exit of Arabic-script practitioners primarily professional (livelihood dependence), communal (role in religious/communal life), or epistemic (self-concept as custodians of a textual tradition)?',
    'Oral history and biographical analysis of practitioners who did vs. did not adapt to Latin script; comparison with other script-transition cases (e.g., Mongolian, Kazakh, Azerbaijani).',
    'If primarily professional, exit options improve with retraining; if communal/epistemic, the lock is structural and the extraction is deeper than livelihood loss — it is ontological. This affects the effective extraction computation for this seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_arabic_practitioners, empirical, 'The mechanism of identity lock for Arabic-script practitioners.').

omega_variable(
    minority_assimilation_intent,
    'Was the accelerated assimilation of minority-language speakers an intended feature of the transition design or an emergent consequence?',
    'Analysis of legislative debates, ministerial directives, and educational policy documents from 1928-1938 for explicit references to minority language suppression via script policy.',
    'If intended, the constraint carries a genocidal/assimilationist extraction component that reclassifies it toward snare for minority seats. If emergent, it remains a tragic but structurally distinct coordination-externality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_assimilation_intent, conceptual, 'Intent vs. emergence in minority assimilation through script transition.').

omega_variable(
    dual_literacy_epistemic_access,
    'Did the bridge generation actually achieve substantive access to the Ottoman textual heritage, or did dual literacy remain performative — decoding script without semantic/epistemic continuity?',
    'Content analysis of bridge-generation publications, translations, and educational materials: did they engage Ottoman intellectual traditions (fiqh, tasawwuf, poetry, historiography) or merely transpose surface forms?',
    'If performative, the coordination function is largely theatrical (higher theater_ratio, lower genuine coordination). If substantive, the scaffold delivered its claimed function despite asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_literacy_epistemic_access, empirical, 'Whether dual literacy achieved genuine epistemic continuity or script-level performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgs_gtr_tr_t1928, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(tgs_gtr_tr_t1932, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1932, 0.15).
narrative_ontology:measurement(tgs_gtr_tr_t1936, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1936, 0.18).
narrative_ontology:measurement(tgs_gtr_tr_t1940, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(tgs_gtr_tr_t1944, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1944, 0.22).
narrative_ontology:measurement(tgs_gtr_tr_t1947, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1947, 0.23).
narrative_ontology:measurement(tgs_gtr_tr_t1950, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1950, 0.22).

% Extraction over time
narrative_ontology:measurement(tgs_gtr_be_t1928, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1928, 0.28).
narrative_ontology:measurement(tgs_gtr_be_t1932, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1932, 0.35).
narrative_ontology:measurement(tgs_gtr_be_t1936, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1936, 0.4).
narrative_ontology:measurement(tgs_gtr_be_t1940, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1940, 0.43).
narrative_ontology:measurement(tgs_gtr_be_t1944, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1944, 0.42).
narrative_ontology:measurement(tgs_gtr_be_t1947, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1947, 0.41).
narrative_ontology:measurement(tgs_gtr_be_t1950, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1950, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tgs_gtr_su_t1928, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1928, 0.25).
narrative_ontology:measurement(tgs_gtr_su_t1932, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1932, 0.32).
narrative_ontology:measurement(tgs_gtr_su_t1936, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1936, 0.38).
narrative_ontology:measurement(tgs_gtr_su_t1940, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1940, 0.4).
narrative_ontology:measurement(tgs_gtr_su_t1944, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1944, 0.39).
narrative_ontology:measurement(tgs_gtr_su_t1947, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1947, 0.37).
narrative_ontology:measurement(tgs_gtr_su_t1950, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1950, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__gradual_transition_reading, 0.08).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_language_reform_1932).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, minority_language_policy_1924).

% DUAL FORMULATION NOTE:
% Part of the turkish_graphemic_substrate constraint family. This reading (gradual_transition_reading) was the official policy scaffold during 1928-1950. The ottoman_continuity_reading persisted as a residual oppositional constraint among religious/traditional communities. The secular_nationalist_reading became the post-1950 dominant constraint (Latin exclusivity). All three share the kernel_id turkish_graphemic_substrate but instantiate different constraints with different ε values, beneficiary/victim structures, and claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, organized, 0.35).
constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, moderate, 0.85).
constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
