% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: UN Charter Article 2(7) and Chapter VII: Sovereignty-First Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   The UN Charter Article 2(7) states: 'Nothing contained in the present
 *   Charter shall authorize the United Nations to intervene in matters which
 *   are essentially within the domestic jurisdiction of any state.' This
 *   sovereignty-first reading treats that clause as foundational law, making
 *   state consent or Chapter VII authorization (limited to inter-state
 *   aggression) the necessary gates for any military intervention. The
 *   reading benefits post-colonial and authoritarian states by barring
 *   humanitarian intervention, while extracting from populations under
 *   systematic atrocity by denying them access to external protection. This
 *   is explicitly ONE reading of a contested kernel — the sibling R2P
 *   reading, which treats sovereignty as conditional on protecting
 *   populations and opens the door to humanitarian intervention for mass
 *   atrocity, instantiates the opposite structural claim from the same
 *   Charter text. The claim/metric gap is authored intentionally: the
 *   constraint is CLAIMED as tangled_rope (coordination function for
 *   non-interference, active enforcement against humanitarian exceptions)
 *   while the metrics describe highly extractive operation (ε=0.79) with
 *   rising theater over time (the sovereignty-first reading increasingly
 *   functions as cover for atrocity protection rather than as active
 *   coordination).
 *
 * KEY AGENTS:
 *   - Post-colonial states: benefit from sovereignty-first reading as shield against neo-colonial intervention; organized power, generational time horizon
 *   - Authoritarian regimes: directly benefit from non-interference clause as legal cover for domestic atrocity; institutional power, short time horizon
 *   - Populations under domestic atrocity: powerless, identity-locked, trapped — bear the constraint's costs directly; immediate time horizon, no exit
 *   - Permanent Security Council members: set the terms of Article 2(7) interpretation through veto power and practice; institutional power, arbitrage exit
 *   - Humanitarian organizations: document atrocity, advocate for intervention, excluded from formal authority; moderate power, constrained exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.79).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.71).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "UN Charter Article 2(7) and Chapter VII: Sovereignty-First Reading").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '92ecf0ca-0e80-43c4-bb4c-9b300f1a2751').
narrative_ontology:cs_kernel_codification('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', fixed_text).
narrative_ontology:cs_authority_grounding('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', extraction).
narrative_ontology:cs_interpretation_layer_present('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751').
narrative_ontology:cs_reading_relation('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', foundational, state_sovereignty_foundational).
narrative_ontology:cs_axiom_status(state_sovereignty_foundational, holdable).
narrative_ontology:cs_axiom_grounding('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', state_sovereignty_foundational, deontological).
narrative_ontology:cs_axiom('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', foundational, intervention_requires_explicit_authorization).
narrative_ontology:cs_axiom_status(intervention_requires_explicit_authorization, holdable).
narrative_ontology:cs_axiom_grounding('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', intervention_requires_explicit_authorization, conventional).
narrative_ontology:cs_reference_frame('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', sovereignty_as_foundational_protection).
narrative_ontology:cs_drift_state('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', contemporary_humanitarian_advocacy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('92ecf0ca-0e80-43c4-bb4c-9b300f1a2751', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, persecuted_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Newly sovereign states benefit from a regime that treats internal borders as sacrosanct. The sovereignty-first reading protects them from external pressure on domestic governance. They collect insulation from international scrutiny and interference in their internal affairs, particularly when their governments face domestic opposition or engage in practices the international community would otherwise condemn.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    organized, generational, mobile, global).

% Regimes conducting mass atrocities or systematic persecution benefit directly from the sovereignty-first reading: it bars external intervention unless explicitly framed as response to inter-state aggression or authorized by Security Council (where veto-holding allies can block action). This constraint provides legal cover for internal repression and buys time through procedural barriers.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    institutional, biographical, trapped, national).

% Suffer the constraint's costs directly: their experience of genocide, ethnic cleansing, or systematic persecution is explicitly protected from external intervention by the sovereignty-first reading. They cannot exit — they are the geographic and legal subject of the regime. Their only recourse is internal revolt (which the regime can suppress) or flight as refugees (which other states are not obligated to accept). The constraint bars the international community from justifying intervention on grounds of atrocity alone.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% Identity-locked into the territory and community they face persecution within — cannot renounce their ethnic, religious, or political identity to escape. The sovereignty-first reading explicitly excludes their persecution from triggering international intervention (unless it spills across borders, triggering inter-state conflict). They bear the extraction of continued persecution while international law forbids interference.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, persecuted_minorities, payer,
    powerless, immediate, identity_locked, local).

% Permanent members have veto power over Chapter VII authorizations. Under the sovereignty-first reading, they can block interventions they deem to interfere with state sovereignty, giving them leverage over weaker states' internal affairs through the threat of intervention or the promise of protection from intervention. They set the terms of when sovereignty is treated as foundational.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Document atrocities and advocate for intervention but have no formal seat at the table of Article 2(7) and Chapter VII interpretation. Their testimony and evidence are excluded from the core legal reasoning that determines whether sovereignty bars intervention. They operate under the constraint's framework rather than participating in its authoring.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_organizations, excluded,
    moderate, biographical, constrained, global).

% Monitor whether the sovereignty-first reading blocks or permits intervention in their sphere. They have geopolitical interests in how the constraint is applied but not formal authority to revise it. Their influence comes through advocacy in the Security Council and through practice (intervention or abstention from it) that either reinforces or undermines the sovereignty-first reading.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, regional_powers, observer,
    powerful, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear international legal framework for when external military intervention is prohibited: intervention requires either explicit state consent or a Security Council authorization demonstrating inter-state aggression. This solves the coordination problem of defining when unilateral military action is legitimate, preventing a rule-of-force system where powerful states intervene anywhere at will.
% TRANSFER_FUNCTION: Moves sovereignty-as-legal-protection from states that would face intervention pressure (usually post-colonial and authoritarian states) to a global system where non-interference is the default rule. Simultaneously extracts protection from populations under domestic atrocity by barring external intervention on humanitarian grounds alone. The transfer is from persecuted populations (who lose access to international protection) to regimes (who gain immunity from humanitarian intervention).
% ABSENT_VOICES: Populations under systematic atrocity have no seat at the Security Council, in the General Assembly with meaningful veto power, or in the treaty negotiation tables that established Article 2(7). Indigenous peoples, stateless minorities, and internal political dissidents are structurally excluded from the reading's own authority structure. Humanitarian organizations document the constraint's impact but cannot reverse it. Their absence from the framework's authoring is the core structural asymmetry.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first reading disappeared and R2P or a humanitarian-intervention norm took its place, the entire landscape of when international military force is permissible would shift. Regimes currently protected by non-interference would face potential intervention; the Security Council's veto would lose much of its utility in blocking humanitarian actions; post-colonial states would lose their primary legal defense against external pressure. The distribution of de facto autonomy would reorganize sharply.
% FOUNDING_PROBLEM: Post-World War II decolonization and the creation of the UN required a framework that would prevent great powers from re-colonizing newly independent states under the guise of humanitarian concern. Article 2(7) was written to protect state sovereignty as the foundational principle, ensuring that newly sovereign nations would not be subject to external military intervention justified by internal governance concerns.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and their representatives attest the founding problem remains live: interventions framed as humanitarian threaten neo-colonial control. International legal scholars aligned with sovereignty-first doctrine support this reading. However, humanitarian organizations, R2P advocates, and populations experiencing atrocity contest the status entirely — they argue the founding problem (re-colonization risk) has been superseded by a more pressing problem (impunity for mass atrocity). The contention is not resolved by authorities outside the beneficiary set; it is the core jurisdictional dispute of the kernel.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79 at interval end) because the constraint's operation systematically protects regimes committing atrocity from external intervention, a transfer that benefits authoritarian actors and harms persecuted populations. This is extraction because the beneficiary (the regime) is not the primary subject the coordination logic would identify — the coordination story is non-interference among states; the extraction story is the use of non-interference to shield internal repression. The measurement series shows rising extractiveness over time (1945→2026: 0.61→0.79) as humanitarian norms grew in the international system, making the sovereignty-first reading increasingly extractive (it must work harder to block intervention as the pressure for intervention rises). Theater rises (0.18→0.42) because an increasing share of sovereignty-first discourse defends atrocities under the label of 'respecting state dignity' — the coordination function (preventing great-power re-colonization) has largely been solved by structural facts (no viable re-colonization mechanism), leaving theater as the operative function. Suppression plateaus around 0.71 after 2005, indicating that the enforcement infrastructure (veto power, procedural barriers) is stable rather than intensifying. The theater rise without suppression intensification suggests the constraint is increasingly vulnerable to norm challenge — it persists through institutional inertia and veto power rather than through fresh active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (post-colonial states, authoritarian regimes, permanent Security Council members) experience this reading as a legitimate protection of sovereignty-as-foundational. Their computed classification should reflect the constraint as coordination (non-interference) with asymmetric structure. The victim seats (populations under atrocity, persecuted minorities) experience it as pure extraction enforced by exclusion from the legal framework's authority structure. The engine should compute substantially different type classifications across these seats: beneficiaries see rope-to-tangled-rope; victims see snare. This divergence is the central analytical point — the same constraint looks like coordination from the protected state's position and like systematic extraction from the persecuted population's position. The scholarly consensus (R2P advocates) aligns with the victim seats' analysis, which creates a meta-level issue: the constraint is being challenged on grounds that it disguises extraction as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-colonial states and authoritarian regimes are structural beneficiaries (d near 0.0): they collect direct insulation from intervention pressure and have mobile-to-trapped exit options depending on their power level, which means the constraint subsidizes them by allowing them to avoid enforcement they would otherwise face. Populations under atrocity are structural targets (d near 1.0): the constraint extracts from them by denying access to international protection they would otherwise invoke; they are identity-locked and trapped, which amplifies extraction (they cannot exit the territory or the identity the constraint uses to target them). Permanent Security Council members are near-beneficiary (d near 0.2): they benefit from veto power leverage the constraint gives them, and they have arbitrage exit (they can always intervene unilaterally, as the constraint is only binding if they choose to respect it). Humanitarian organizations are observer-positioned (d=0.5): they are neither benefiting nor paying from the constraint's operation directly, but their exclusion from authority means they experience the asymmetry of the constraint's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty-first reading exhibits classic mandatrophy dynamics: the founding problem (preventing great-power re-colonization of post-colonial states) was substantially solved by structural facts (the high cost of re-colonization, the growth of multipolarity, the emergence of non-aligned movements). The constraint's mandate has outlived its function. What persists is not active coordination around non-interference (which would now operate as rope) but rather the use of non-interference doctrine as a shield for atrocity — this is pure extraction wrapped in coordination language. The rising theater ratio (0.18→0.42) is the key signal: the constraint increasingly functions to protect regimes rather than to prevent great-power dominance. The suppression requirement plateaus rather than declining, indicating the constraint is maintained through institutional inertia (veto power, treaty language, procedural anchoring) rather than through fresh functional necessity. A genuine mandatrophy_resolved flag should apply: the founding mandate is dead; what remains is theatrical invocation of sovereignty language to block humanitarian pressure. However, the constraint is not simple to remove because it genuinely solved a real historical problem (post-war re-colonization prevention) and removing it entirely might open the door to a different form of domination. The reading's persistence is thus partially justified (the underlying protection value remains) and partially theatrical (the current application protects atrocity rather than preventing re-colonization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neocolonialism_prevention_vs_atrocity_protection,
    'Is Article 2(7)''s non-interference clause still necessary to prevent great-power re-colonization, or has that risk been structurally eliminated by multipolarity, the UN system, and international norms?',
    'Historical counterfactual analysis: if Article 2(7) were weakened or repealed, would great powers move to re-colonize, or would multipolarity, economic interdependence, and institutional constraints prevent it?',
    'If re-colonization risk is genuinely eliminated, the sovereignty-first reading has lost its founding justification and the constraint''s persistence becomes purely extractive cover for atrocity protection. If re-colonization risk remains real, part of the extraction is a necessary cost of maintaining the protection against greater harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neocolonialism_prevention_vs_atrocity_protection, conceptual, 'Whether the founding mandate (preventing re-colonization) remains necessary or has been structurally superseded.').

omega_variable(
    internal_vs_enforced_suppression,
    'To what degree does the measured suppression (0.71) reflect active institutional enforcement (permanent member vetoes, procedural barriers) versus internalized norm adoption by states that now voluntarily refrain from humanitarian intervention?',
    'Behavioral analysis: document cases where states were tempted to intervene but refrained due to article 2(7) constraint (active suppression) versus cases where states accepted non-intervention without needing to be stopped (internalized norm).',
    'If suppression is mostly internalized, the constraint is more stable and less dependent on veto power; if mostly enforced, the constraint is vulnerable to veto-holder defection. This affects the stability of the sovereignty-first reading''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_enforced_suppression, empirical, 'Whether suppression operates through active institutional barriers or through internalized norm acceptance.').

omega_variable(
    coordination_function_decay,
    'Is the measured rise in theater ratio (0.18→0.42) evidence that the constraint''s original coordination function (preventing great-power dominance) has atrophied, or is the theater increase an artifact of increased humanitarian advocacy against the constraint?',
    'Decompose the theater increase: (a) how much reflects genuine loss of coordination function (fewer states invoke non-interference for its original purpose), (b) how much reflects increased performance of the constraint (states defending sovereignty language more eloquently despite the underlying function remaining).',
    'If function decay dominates, the constraint should be reclassified toward piton (institutionally maintained but functionally atrophied). If performance increase dominates, the constraint remains tangled_rope but with increasing vulnerability to norm challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_decay, conceptual, 'Whether rising theater indicates functional decay or rhetorical intensification in response to norm pressure.').

omega_variable(
    kernel_coexistence_vs_foreclosure,
    'Do the sovereignty-first reading and the R2P reading genuinely coexist as live positions that could be held simultaneously by different parties, or does the sovereignty-first reading''s logic logically foreclose R2P within a single institutional framework?',
    'Legal-logical analysis: can a state or international body hold both ''sovereignty is foundational and intervention requires explicit consent'' AND ''systematic atrocity triggers international responsibility to intervene'' without internal contradiction?',
    'If they coexist, the readings are in competition but neither rules the other out — the constraint is vulnerable to norm replacement but not to logical refutation. If sovereignty-first forecloses R2P, the reading is logically stronger but empirically weaker (the world increasingly rejects its logic). The engine should compute reading_relations as ''coexists_with'' based on this analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_coexistence_vs_foreclosure, conceptual, 'Whether the two readings logically foreclose each other or can coexist as live positions.').

omega_variable(
    veto_as_feature_vs_veto_as_artifact,
    'Is the permanent Security Council veto a feature of the sovereignty-first reading (part of the mechanism that enforces non-interference), or an artifact of Cold War power dynamics that now sustains the reading beyond its functional lifetime?',
    'Historical analysis of the veto''s role in blocking humanitarian interventions and protecting atrocity-committing states; comparison to a counterfactual system where Chapter VII required supermajority rather than consensus minus veto.',
    'If veto is a feature, the sovereignty-first reading is structurally tied to institutional design and cannot be changed without redesigning the UN. If veto is an artifact, the reading is more vulnerable to institutional reform that removes the veto mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_as_feature_vs_veto_as_artifact, conceptual, 'Whether the veto mechanism is essential to the sovereignty-first reading or an accidental supporting structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(arti_tr_t1966, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1966, 0.24).
narrative_ontology:measurement(arti_tr_t1989, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1989, 0.33).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(arti_tr_t2026, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.61).
narrative_ontology:measurement(arti_be_t1966, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1966, 0.68).
narrative_ontology:measurement(arti_be_t1989, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1989, 0.73).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(arti_be_t2026, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2026, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.54).
narrative_ontology:measurement(arti_su_t1966, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1966, 0.61).
narrative_ontology:measurement(arti_su_t1989, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1989, 0.67).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(arti_su_t2026, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.18).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).

% DUAL FORMULATION NOTE:
% This constraint and article_2_7_chapter_vii_tension__r2p_reading are two readings of the same contested kernel: the UN Charter's balance between Article 2(7) (non-interference) and Chapter VII (Security Council authorization). They have different ε values (sovereignty-first: 0.79; R2P: lower, ~0.55), different beneficiary/victim structures, and different classifications. The sovereignty-first reading treats Article 2(7) as foundational and interprets Chapter VII narrowly (inter-state aggression only). The R2P reading treats sovereignty as conditional on protecting populations and interprets Chapter VII broadly to permit humanitarian authorization. Neither reading forecloses the other — they coexist as live positions in international law and politics. The sovereignty-first reading influences the R2P reading by creating the doctrinal pressure that R2P must overcome; the R2P reading influences sovereignty-first by generating normative pressure that causes sovereignty-first to deepen its assertions. This is a constraint family where the two stories should be read in relation to each other: the gap between their ε values and type classifications reveals the structural stakes of the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, powerless, 0.92).
constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
