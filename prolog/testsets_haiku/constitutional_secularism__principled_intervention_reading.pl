% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Principled State Intervention in Religious Affairs for Social Reform
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   In many pluralist democracies, the constitution mandates equality and
 *   protection of fundamental rights while also protecting religious freedom
 *   and community autonomy. The principled intervention reading resolves this
 *   tension by permitting state intervention in religious governance when
 *   necessary to protect weaker sections from discrimination or oppression,
 *   but only on carefully reasoned grounds (not on majoritarian preference
 *   alone). The reading claims a middle path: not strict neutrality (which
 *   can leave vulnerable groups unprotected), not reformist supremacy (which
 *   can weaponize state power against minorities), but calibrated
 *   intervention guided by fundamental rights principles. This constraint
 *   story models that specific reading as a contentious claim about where
 *   state authority legitimately extends into religious domains. The sibling
 *   readings — strict neutrality and reformist supremacy — are structurally
 *   distinct constraints with different ε values and different extraction
 *   patterns; they are NOT alternative measurements of this same constraint.
 *
 * KEY AGENTS:
 *   - constitutional_reform_authority (institutional, authoritative, sets the intervention doctrine)
 *   - marginalized_religious_minorities (powerless, trapped, benefit from state protection of fundamental rights)
 *   - majority_religious_establishment (powerful, constrained, lose self-determination over governance)
 *   - religious_autonomy_claimants (organized, constrained, argue state overreach)
 *   - liberal_reform_coalitions (organized, mobile, leverage state authority for reform objectives)
 *   - weaker_sections_internal_dissenters (powerless, trapped, excluded from debate despite stakes)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.62).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.58).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Principled State Intervention in Religious Affairs for Social Reform").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, 'a290636e-397e-4c0f-8f12-911c686f8b57').
narrative_ontology:cs_kernel_codification('a290636e-397e-4c0f-8f12-911c686f8b57', formalized).
narrative_ontology:cs_authority_grounding('a290636e-397e-4c0f-8f12-911c686f8b57', lineage).
narrative_ontology:cs_interpretation_layer_present('a290636e-397e-4c0f-8f12-911c686f8b57').
narrative_ontology:cs_reading_relation('a290636e-397e-4c0f-8f12-911c686f8b57', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('a290636e-397e-4c0f-8f12-911c686f8b57', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('a290636e-397e-4c0f-8f12-911c686f8b57', foundational, intervention_conditional_on_fundamental_rights).
narrative_ontology:cs_axiom_status(intervention_conditional_on_fundamental_rights, holdable).
narrative_ontology:cs_axiom_grounding('a290636e-397e-4c0f-8f12-911c686f8b57', intervention_conditional_on_fundamental_rights, deontological).
narrative_ontology:cs_axiom('a290636e-397e-4c0f-8f12-911c686f8b57', foundational, internal_remedies_exhaustion_presumption).
narrative_ontology:cs_axiom_status(internal_remedies_exhaustion_presumption, holdable).
narrative_ontology:cs_axiom_grounding('a290636e-397e-4c0f-8f12-911c686f8b57', internal_remedies_exhaustion_presumption, deontological).
narrative_ontology:cs_reference_frame('a290636e-397e-4c0f-8f12-911c686f8b57', equal_fundamental_rights_with_religious_autonomy_boundary).
narrative_ontology:cs_drift_state('a290636e-397e-4c0f-8f12-911c686f8b57', contemporary_majoritarian_capture_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a290636e-397e-4c0f-8f12-911c686f8b57', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, marginalized_religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reform_oriented_state_institutions).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, weaker_sections_within_communities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_autonomy_claimants).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, majority_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, decentralized_religious_governance_structures).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as TANGLED ROPE (not mountain, not pure rope, not snare in the authoring seat) because it combines genuine coordination benefits with asymmetric extraction. The coordination function is real: marginalized sections genuinely benefit from state-backed enforcement of equal rights against oppressive community norms — that solves a coordination problem (internal groups cannot overcome collective-action barriers to resist traditional hierarchy). But the extraction is asymmetric and substantial: religious autonomy holders lose self-determination authority, majority communities face unequal scrutiny, and the reform doctrine itself becomes subject to majoritarian capture. Extraction starts at 0.38 (early period: intervention cautious, boundaries contested) and rises to 0.62 (extraction stabilizes as institutional machinery hardens and creative reinterpretation expands intervention scope). Theater rises from 0.25 to 0.41: the constraint's public face emphasizes reform and protection of rights, but over time the enforcement machinery increasingly protects state authority itself (institutional turf, regulatory power, ideological vindication) relative to direct protection of weaker sections. Suppression rises from 0.35 to 0.58 because early resistance is uncoordinated; as the constraint hardens, suppression concentrates (religious autonomy claimants are delegitimized, internal dissenters are sidelined, exit becomes costlier). The measurement series shows the constraint stabilizing at t=25-40: extractiveness plateaus, theater ratio plateaus, suppression plateaus. The curve is not a classic exploitation-accumulation arc; it shows a coordination doctrine that initially served genuine reform hardening into an enforcement apparatus for state authority.
 *
 * PERSPECTIVAL GAP:
 *   A key perspectival gap exists between the institutional reform authority and religious autonomy claimants. The authority sees the constraint as principled protection of rights — a successful coordination mechanism. Autonomy claimants see the same constraint as illegitimate state expansion. From the authority's institutional seat, extractiveness might be experienced as zero (legitimate exercise of constitutional authority); from the constrained religious seat, extractiveness is high (loss of self-determination). The engine computes this gap from the structural data: power (institutional vs. organized), exit options (analytical vs. constrained), and victim status (institution is not a victim). The computed types will diverge — the institutional seat should compute as rope or mountain (beneficiary or neutral), while the organized victim seat computes as tangled rope or snare (extracted from).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are marginalized sections and reform coalitions — they gain state protection and leverage. Victims are majority religious establishment and autonomy claimants — they lose self-determination and face enforcement scrutiny. The asymmetry is stark because the constraint redistributes AUTHORITY over religious governance from decentralized communities to state institutions; this is a directional power transfer. Marginalized sections gain protection but lose cultural autonomy (identity_locked exit: they cannot become non-religious without losing identity, so they remain under constraint); this makes their effective exit 'trapped' despite the nominal benefit. Autonomy claimants lose authority without gaining protection; their exit is 'constrained' (can litigate, can lobby, cannot fully exit a national constitutional order). Reform coalitions have 'mobile' exit (can shift to different parties, different jurisdictions, different reform strategies). The directionality derivation from beneficiary/victim + exit options should yield: marginalized (beneficiary + trapped) ~ 0.4 (low d, net subsidy despite identity-lock costs); reform coalitions (beneficiary + mobile) ~ 0.2 (arbitrage-grade mobility toward state authority); majority establishment (victim + constrained) ~ 0.8 (high d, targets); autonomy claimants (victim + constrained) ~ 0.75 (high d, targets). No overrides needed; structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (religious communities exclude weaker sections, no internal remedy) is CONTESTED in status because religious autonomy claimants argue the problem has been substantially mitigated by internal reform movements and by exit options (dissenters can join different communities, establish reform congregations, seek secular spaces). The reform authority argues the problem is still LIVE because oppressive practices persist in many communities despite internal reform efforts. The disappearance verdict is WORLD_REARRANGES because if the intervention doctrine vanished, weaker sections would face intensified retaliation and loss of state protection — the world would rearrange into higher internal hierarchy and lower formal equality. This is not a case of a dead founding problem where the constraint persists as theater (classic piton). Instead, it is a case where the founding problem is genuinely contested AND the constraint has begun to serve institutional interests (state authority expansion, judicial power, regulatory turf) alongside the original reform function. The theater ratio (0.41 at endpoint) is moderate, not high — the constraint is not mostly theater, but a growing share of enforcement energy defends the institutional apparatus itself rather than protecting weaker sections. Mandatrophy is NOT declared because the founding problem is not dead; it is contested, which means the constraint remains contestable as either principled coordination or majoritarian overreach depending on how the principal question (omega_majortarian_capture_risk) resolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majortarian_capture_risk,
    'Does the principled intervention doctrine operationalize as protection for weaker sections, or as cover for majoritarian religious imposition on minorities?',
    'Pattern analysis of intervention targets over time: do courts/governments intervene equally in oppressive practices within majority and minority religions, or do they focus on minority practices while leaving majority-community oppression untouched? Comparative jurisprudence across jurisdictions.',
    'If pattern is asymmetric (majority practices rarely intervened), the constraint''s extractiveness is higher and its classification shifts toward snare (pure majoritarian coercion) with the ''reform'' framing as theatrical cover. If symmetric, the tangled-rope classification (genuine coordination + unequal extraction) is supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(majortarian_capture_risk, empirical, 'Whether state intervention protects weaker sections neutrally or weaponizes the doctrine against minorities.').

omega_variable(
    autonomy_vs_equality_framability,
    'Is the conflict between religious autonomy and constitutional equality framed as a genuine dilemma requiring principled trade-offs, or as a false dichotomy where autonomy is recast as oppression?',
    'Jurisprudential analysis: do courts acknowledge the legitimacy of religious autonomy as a competing right and articulate a principled boundary, or do they treat autonomy claims as presumptively suspect? Do they engage with the internal reformist voices within religious communities who argue for self-transformation, or only with state-backed reformers?',
    'If courts systematically delegitimize autonomy claims and sideline internal dissenters, the reading is less a principled coordination and more a majoritarian authority grab. This affects how the tangled-rope classification sits: is coordination genuine or illusory?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_equality_framability, conceptual, 'Whether the constraint frames the autonomy-equality conflict as a genuine dilemma or a false binary.').

omega_variable(
    internal_dissent_suppression,
    'Are weaker sections within communities who object to state intervention (preferring community self-determination over state-imposed reform) suppressed by state messaging, community pressure, or both?',
    'Documentary evidence of how dissenting voices are treated in public debate and legal proceedings. Do courts hear from communities about their own preferences regarding reform pace and method, or do courts hear only from state advocates and human rights organizations?',
    'If the constraint suppresses internal dissent from weaker sections themselves, the measured suppression (0.58) understates the true suppressive force — it captures suppression of religious autonomy claimants but misses suppression of weaker-section dissenters by state-backed reformers. The constraint''s structural suppressiveness is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_dissent_suppression, empirical, 'Whether state intervention suppresses dissenting voices among weaker sections themselves.').

omega_variable(
    reform_vs_coercion_boundary,
    'What is the structural boundary between principled intervention (protecting fundamental rights) and coercive imposition (using state power to enforce one reading of religion on communities with alternative readings)?',
    'Jurisprudential and philosophical analysis: where does principled intervention doctrine locate the boundary? Is it based on harm (intervention triggers if harm is established), on rights protection (intervention triggers if a fundamental right is at stake), on procedure (intervention requires community input), or on outcome (intervention is justified by equality outcomes)? Different boundaries generate different scope and extractiveness profiles.',
    'The clarity and defensibility of this boundary determines whether the constraint is a genuine attempt at principled coordination or a framework that rationalizes majoritarian overreach. Vague or outcome-based boundaries enable mission creep and transformation to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_vs_coercion_boundary, conceptual, 'The epistemic and normative criteria that distinguish principled reform from majoritarian coercion.').

omega_variable(
    kernel_reading_contest,
    'Does this principled intervention reading instantiate a coherent middle path between strict neutrality and reformist supremacy, or does it collapse under pressure toward one pole?',
    'Historical observation and comparative jurisprudence: how do actual constitutional courts operationalize this reading? Do they maintain the principled balance or drift toward reformist authority expansion or strict neutrality reversion?',
    'If the reading proves unstable (drifts to reformist pole), the constraint''s extractiveness will increase and it reclassifies toward snare. If it drifts to strict neutrality, extraction decreases and it reclassifies toward rope or mountain. The reading''s viability as a distinct constitutional position affects the constraint family''s stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the principled intervention reading is a stable constitutional position or an unstable midpoint between stronger poles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cons_tr_t5, constitutional_secularism__principled_intervention_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__principled_intervention_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__principled_intervention_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__principled_intervention_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__principled_intervention_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__principled_intervention_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__principled_intervention_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cons_be_t5, constitutional_secularism__principled_intervention_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__principled_intervention_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__principled_intervention_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__principled_intervention_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__principled_intervention_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__principled_intervention_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__principled_intervention_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t5, constitutional_secularism__principled_intervention_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__principled_intervention_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__principled_intervention_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__principled_intervention_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__principled_intervention_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__principled_intervention_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__principled_intervention_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__principled_intervention_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, gender_equality_in_religious_law).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, caste_discrimination_in_temples_and_mosques).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel constitutional_secularism. The kernel_id is constitutional_secularism; this reading_id is principled_intervention_reading. Sibling readings include strict_neutrality_reading (state maintains equal distance from all religions) and reformist_reading (state has affirmative duty to eliminate oppressive practices). These are three structurally distinct constraints with different ε values: strict neutrality operates at low extractiveness (0.20-0.35, near-mountain for some seats) because it minimizes state power; reformist operates at high extractiveness (0.75-0.85, snare-adjacent) because state authority expands to override autonomy; principled intervention (this constraint) operates at moderate-high extractiveness (0.62 at plateau) because it attempts to balance coordination and autonomy but remains subject to capture. The three readings share the same kernel (the constitutional commitment to some church-state relationship) but produce different effective constraints for the same agent seats because the legitimacy conditions for state action differ radically. All three should be authored as separate constraint stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
