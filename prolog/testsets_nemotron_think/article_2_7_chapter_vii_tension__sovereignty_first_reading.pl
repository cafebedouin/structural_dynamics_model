% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Sovereignty-First Reading of UN Charter Article 2(7) / Chapter VII
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint story models the sovereignty-first reading of the UN
 *   Charter's Article 2(7) (domestic jurisdiction) and Chapter VII
 *   (enforcement action). The reading holds that state sovereignty is the
 *   foundational ordering principle of international law; intervention in
 *   domestic affairs requires either explicit consent of the target state or
 *   Chapter VII authorization, which the reading interprets as limited to
 *   inter-state aggression. This reading is institutionalized in UNSC
 *   practice through the P5 veto and in ICJ jurisprudence (e.g., Nicaragua v.
 *   USA, Kosovo Advisory Opinion dissenting opinions). The sibling reading
 *   (r2p_reading) argues sovereignty is conditional on protecting populations
 *   — the Responsibility to Protect (R2P) doctrine. Both readings draw from
 *   the same Charter text but instantiate different constraints with
 *   different beneficiary/victim structures and different ε values. This
 *   story authors ONLY the sovereignty-first reading as a clean ε-invariant
 *   constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.72).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Sovereignty-First Reading of UN Charter Article 2(7) / Chapter VII").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '104b05cd-48f5-4c11-afe3-73193ea34625').
narrative_ontology:cs_kernel_codification('104b05cd-48f5-4c11-afe3-73193ea34625', formalized).
narrative_ontology:cs_authority_grounding('104b05cd-48f5-4c11-afe3-73193ea34625', lineage).
narrative_ontology:cs_interpretation_layer_present('104b05cd-48f5-4c11-afe3-73193ea34625').
narrative_ontology:cs_reading_relation('104b05cd-48f5-4c11-afe3-73193ea34625', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('104b05cd-48f5-4c11-afe3-73193ea34625', foundational, non_intervention_as_customary_law).
narrative_ontology:cs_axiom_status(non_intervention_as_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('104b05cd-48f5-4c11-afe3-73193ea34625', non_intervention_as_customary_law, conventional).
narrative_ontology:cs_axiom('104b05cd-48f5-4c11-afe3-73193ea34625', foundational, chapter_vii_limited_to_interstate_aggression).
narrative_ontology:cs_axiom_status(chapter_vii_limited_to_interstate_aggression, holdable).
narrative_ontology:cs_axiom_grounding('104b05cd-48f5-4c11-afe3-73193ea34625', chapter_vii_limited_to_interstate_aggression, conventional).
narrative_ontology:cs_reference_frame('104b05cd-48f5-4c11-afe3-73193ea34625', westphalian_sovereignty_order).
narrative_ontology:cs_drift_state('104b05cd-48f5-4c11-afe3-73193ea34625', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('104b05cd-48f5-4c11-afe3-73193ea34625', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, non_intervention_as_customary_law).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, westphalian_sovereignty_as_ordering_principle).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, chapter_vii_limited_to_interstate_aggression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over Chapter VII authorization; they write and enforce the non-intervention rule. They can authorize intervention when it serves their interests (Libya 2011) or block it when it does not (Syria 2011-2024). They benefit from the coordination function (great-power conflict prevention) and extract impunity for allies. Their exit is arbitrage — they can change the constraint by consensus or unilateral action.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Former colonies that championed sovereignty as protection against neo-colonial intervention. They benefit from the norm shielding domestic affairs from external scrutiny. Their exit is constrained — they collectively defend the norm in the General Assembly and Non-Aligned Movement but individually cannot change the UNSC structure.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    organized, biographical, constrained, global).

% Regimes that commit systematic domestic repression (Syria, Myanmar, North Korea, etc.). They benefit directly from the veto shield — the constraint extracts survival for them at the cost of their populations. Their exit is constrained: they depend on P5 patrons for veto cover but cannot guarantee it indefinitely.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    institutional, biographical, constrained, national).

% Civilian populations facing genocide, crimes against humanity, ethnic cleansing (Rwanda 1994, Srebrenica 1995, Darfur 2003+, Syria 2011+, Myanmar 2017+, Xinjiang 2017+). They bear the full cost of non-intervention — death, displacement, torture — with no exit. They have no voice in UNSC decisions and no legal standing to compel action.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% States (Canada, Netherlands, Ghana, etc.), NGOs (ICRtoP, Human Rights Watch), UN officials (Special Advisers on R2P) advocating for the competing reading. They would object to the sovereignty-first constraint but are structurally excluded from the UNSC veto decision. Their exit is trapped — they cannot access the decision mechanism that authorizes intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, r2p_advocacy_coalition, excluded,
    organized, biographical, trapped, global).

% Academics and jurists who analyze the Charter interpretation dispute (Crawford, Koskenniemi, Chesterman, Bellamy, Welsh, etc.). They see the full structure — the kernel, both readings, the institutional dynamics — but do not collect from or pay into the constraint. Their exit is analytical: they can change frameworks but not the constraint itself.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power war by establishing a clear rule: no intervention in domestic jurisdiction without consent or UNSC authorization. Solves the coordination problem of mutual vulnerability — each great power agrees not to intervene in others' spheres in exchange for reciprocal restraint.
% TRANSFER_FUNCTION: Transfers the cost of domestic atrocity from perpetrator regimes to victim populations. The perpetrator regime retains power and survives; the population bears death, displacement, and trauma. The P5 members transfer political risk by using the veto to avoid hard choices — they pay no cost for inaction.
% ABSENT_VOICES: Populations under domestic atrocity are the primary absent voice — they have no representation in the UNSC, no vote in the General Assembly, and no standing in the ICJ. Future generations who inherit the precedent of impunity are also absent. The r2p_advocacy_coalition speaks for them but lacks decision authority.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first constraint vanished overnight, the UNSC would authorize intervention in ongoing atrocity situations (Syria, Myanmar, etc.) within weeks; regional organizations would act under Chapter VIII without waiting for UNSC approval; the ICC would gain effective enforcement; the norm of non-intervention would be replaced by a conditional sovereignty norm. The international order would rearrange around R2P implementation.
% FOUNDING_PROBLEM: Preventing great-power war by establishing sovereign equality and non-intervention as the ordering principle of international law after two world wars. The UN Charter (1945) was built to solve the problem of aggressive war between states by making domestic jurisdiction a shield against external intervention.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing inter-state war) is corroborated by UN Charter drafting history (San Francisco Conference records) and the Long Peace since 1945 — attested by international relations scholars (Mueller, Pinker, Gaddis) outside the beneficiary set. The contested status is corroborated by R2P advocates (Evans, Sahnoun, Bellamy) and UN Secretariat reports (Annan's 'In Larger Freedom') showing the problem has shifted from inter-state to intra-state atrocity. No single authority resolves the contest.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint blocks intervention that would stop domestic atrocity, transferring the cost of non-intervention to victim populations while shielding perpetrator regimes. The coordination function (preventing great-power war through non-intervention) is real but partial — the constraint coordinates at the inter-state level while extracting at the intra-state level. Theater ratio (0.55) is substantial: sovereignty rhetoric performs the coordination function while the veto mechanism actively suppresses R2P-authorized interventions (Syria 2011-2024, Myanmar 2017-present, Xinjiang). Suppression (0.72) reflects the structural veto barrier plus the internalized norm that makes humanitarian intervention politically costly even when legally arguable. Accessibility collapse (0.62) is moderate — alternatives (regional action, coalitions of the willing, ICC referral) exist but are legally contested and politically fragile. Resistance (0.58) is significant: R2P advocacy, ICC prosecutions, regional interventions (ECOWAS, AU), and the 'unwilling or unable' doctrine all contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the P5/agenda_setter seat, the constraint is genuine coordination (prevents great-power conflict, manages intervention authorization). From the beneficiary seats (post-colonial/authoritarian states), it is a shield — they experience it as coordination that benefits them. From the payer seat (atrocity populations), it is a snare — pure extraction of their survival. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Permanent Five members are agenda_setters with institutional power and arbitrage exit (they write the rules and can exit the constraint by authorizing intervention). Post-colonial states and authoritarian regimes are beneficiaries (they collect impunity for domestic repression; exit_options = constrained — they benefit from the norm but cannot easily change it). Populations under domestic atrocity are payers (powerless, trapped — they bear the full cost of non-intervention with no exit). R2P advocacy coalition is excluded (organized power but trapped — they would object but are structurally kept out of UNSC decisions). International legal scholars are observers (analytical, analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power war through non-intervention) is contested — great-power war has been prevented but the constraint now primarily shields domestic repression. The mandate has not been formally resolved; the Charter has not been amended. The constraint persists through institutional inertia and veto power, not because the founding problem is solved. This is a tangled_rope that has accumulated extraction over time (see measurements) without a sunset mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the sovereignty-first reading of Article 2(7)/Chapter VII a genuine coordination mechanism for international order, or an extractive shield for domestic repression?',
    'Track UNSC veto patterns on atrocity situations vs. inter-state aggression; measure whether sovereignty rhetoric correlates with domestic human rights records of veto-wielding states.',
    'If extractive shield dominates, the constraint reclassifies toward snare; if coordination function holds despite atrocity cases, remains tangled_rope with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the sovereignty-first reading coordinates order or extracts impunity.').

omega_variable(
    chapter_vii_scope_ambiguity,
    'Does Chapter VII ''threat to the peace'' authorization structurally include systematic domestic atrocity, or is it limited to inter-state aggression as the sovereignty-first reading claims?',
    'Analyze UNSC practice 1990-present: Resolutions 688 (Iraq/Kurds), 794 (Somalia), 1973 (Libya), 2165 (Syria cross-border) vs. vetoes on Syria, Myanmar, Xinjiang. Codify whether ''threat to peace'' has expanded in practice.',
    'If Chapter VII includes domestic atrocity, the sovereignty-first reading''s extraction is higher (active suppression of existing authorization); if limited to inter-state, extraction is lower but coordination function is narrower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chapter_vii_scope_ambiguity, conceptual, 'Scope of Chapter VII authorization — the core interpretive dispute.').

omega_variable(
    suppression_mechanism_atrocity,
    'Is the suppression of intervention in domestic atrocity cases structural (UNSC veto, procedural barriers) or internalized (states self-censor, populations accept non-intervention as norm)?',
    'Compare intervention rates in atrocity cases with vs. without P5 interest; survey state foreign policy establishments on perceived legality of humanitarian intervention; track NGO advocacy framing shifts.',
    'If internalized, the constraint''s effective suppression exceeds structural measure — the norm carries itself. If structural, reform pathways differ (veto reform vs. norm entrepreneurship).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_atrocity, empirical, 'Structural vs. internalized suppression of intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art27_sov_first_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(art27_sov_first_tr_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(art27_sov_first_tr_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement(art27_sov_first_tr_t1990, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(art27_sov_first_tr_t1999, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1999, 0.52).
narrative_ontology:measurement(art27_sov_first_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.54).
narrative_ontology:measurement(art27_sov_first_tr_t2011, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2011, 0.55).
narrative_ontology:measurement(art27_sov_first_tr_t2020, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2020, 0.55).

% Extraction over time
narrative_ontology:measurement(art27_sov_first_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(art27_sov_first_be_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(art27_sov_first_be_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(art27_sov_first_be_t1990, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(art27_sov_first_be_t1999, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1999, 0.62).
narrative_ontology:measurement(art27_sov_first_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(art27_sov_first_be_t2011, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2011, 0.67).
narrative_ontology:measurement(art27_sov_first_be_t2020, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(art27_sov_first_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(art27_sov_first_su_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(art27_sov_first_su_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(art27_sov_first_su_t1990, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(art27_sov_first_su_t1999, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1999, 0.7).
narrative_ontology:measurement(art27_sov_first_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.71).
narrative_ontology:measurement(art27_sov_first_su_t2011, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2011, 0.72).
narrative_ontology:measurement(art27_sov_first_su_t2020, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2020, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.12).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, r2p_reading).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, icc_complementarity_principle).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, regional_intervention_norms).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, veto_restraint_initiatives).

% DUAL FORMULATION NOTE:
% This constraint and r2p_reading form a constraint family decomposing the single natural-language label 'UN Charter intervention authority'. They share the kernel (Charter text) but have different ε (this reading: 0.68 extractive; r2p_reading: lower ε for populations, higher for perpetrator states), different beneficiaries/victims, and different claimed types. The sovereignty-first reading influences the r2p_reading by controlling the UNSC authorization gate — it creates structural downstream pressure without foreclosing the sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, institutional, 0.15).
constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
