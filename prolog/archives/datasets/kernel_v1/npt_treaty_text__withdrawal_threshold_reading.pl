% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold: Regime Stability Priority Reading
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the 'withdrawal threshold reading' of the
 *   NPT treaty text kernel. The reading holds that Article X withdrawal is
 *   permissible only under extraordinary circumstances to be determined by
 *   the interpreting state in consultation with the NPT regime — a high
 *   threshold framing that prioritizes regime stability over explicit
 *   sovereignty preservation. This reading stands in tension with an
 *   alternative 'sovereignty preservation reading' (sibling constraint) that
 *   treats Article X as an unconditional right to withdraw with three months'
 *   notice, subject only to reciprocal state practice and customary
 *   international law. The North Korean withdrawal (2003) created a
 *   precedent-ambiguity: the withdrawal occurred but the international
 *   community treated it as exceptional, neither foreclosing future
 *   withdrawals nor establishing clear precedent for them. This constraint
 *   models how the high-threshold interpretation functions as a tangled rope:
 *   it coordinates genuine P5 verification interests (the NPT regime's
 *   monitoring function) while extracting compliance from subordinate states
 *   by creating ambiguity around their theoretical exit rights. The measuring
 *   points track the constraint's evolution: as thresholds become more
 *   contested (Ukraine nuclear threats, Iranian nuclear posture,
 *   Japanese/South Korean rearmament debates), the theater ratio rises (more
 *   performative interpretation of 'extraordinary circumstances') and
 *   extractiveness increases (subordinate states become more aware that their
 *   withdrawal option is constrained by interpretation, not law).
 *
 * KEY AGENTS:
 *   - P5 Nuclear-Weapon States (US, Russia, China, UK, France): Institutional/arbitrage — interpret Article X to mean high threshold for withdrawal; maintain regime stability framing while preserving their own exit options; benefit from subordinate state lock-in
 *   - Subordinate NNWS (Iran, North Korea, Egypt): Moderate/constrained — technically have withdrawal rights but face constrained exit (sanctions, security dependency, diplomatic isolation); subject to extraction through interpretation ambiguity
 *   - Threshold States (Japan, South Korea, Brazil, Argentina): Organized/constrained — maintain latent weapons capability while locked into NPT by both treaty and security guarantees; benefit from high-threshold interpretation (credible exit option without visibility) and suffer from it (cannot actually exercise withdrawal without hegemonic cost)
 *   - Non-Proliferation Regime (Abstract Collective): Powerless/trapped — no organizational capacity; bears full cost of withdrawal ambiguity through verification difficulty and norm erosion; cannot organize self-defense
 *   - International Court of Justice / Treaty Bodies: Institutional/analytical — called upon to interpret Article X but have no enforcement power over P5 withdrawals; their interpretations are filtered through state practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.52).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold: Regime Stability Priority Reading").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '1deb7888-b376-43d8-aae3-e1674bae7628').
narrative_ontology:cs_kernel_codification('1deb7888-b376-43d8-aae3-e1674bae7628', fixed_text).
narrative_ontology:cs_authority_grounding('1deb7888-b376-43d8-aae3-e1674bae7628', extraction).
narrative_ontology:cs_interpretation_layer_present('1deb7888-b376-43d8-aae3-e1674bae7628').
narrative_ontology:cs_reading_relation('1deb7888-b376-43d8-aae3-e1674bae7628', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('1deb7888-b376-43d8-aae3-e1674bae7628', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('1deb7888-b376-43d8-aae3-e1674bae7628', foundational, regime_stability_justifies_withdrawal_constraints).
narrative_ontology:cs_axiom_status(regime_stability_justifies_withdrawal_constraints, holdable).
narrative_ontology:cs_axiom_grounding('1deb7888-b376-43d8-aae3-e1674bae7628', regime_stability_justifies_withdrawal_constraints, instrumental).
narrative_ontology:cs_axiom('1deb7888-b376-43d8-aae3-e1674bae7628', foundational, extraordinary_circumstances_requires_p5_adjudication).
narrative_ontology:cs_axiom_status(extraordinary_circumstances_requires_p5_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('1deb7888-b376-43d8-aae3-e1674bae7628', extraordinary_circumstances_requires_p5_adjudication, conventional).
narrative_ontology:cs_reference_frame('1deb7888-b376-43d8-aae3-e1674bae7628', sovereign_right_to_withdraw).
narrative_ontology:cs_drift_state('1deb7888-b376-43d8-aae3-e1674bae7628', contemporary_post_north_korea_precedent, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1deb7888-b376-43d8-aae3-e1674bae7628', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, hegemonic_security_guarantors).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_regime_integrity).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, subordinate_state_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The abstract NPT regime has no exit option and no organizational capacity. Premature withdrawals (North Korea precedent, potential Iranian withdrawal) degrade the regime's ability to verify compliance or coordinate non-proliferation norms. The regime bears the cost of ambiguous Article X interpretation without defending itself.
constraint_indexing:constraint_classification(npt_treaty_text__withdrawal_threshold_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Non-nuclear-weapon states face constrained exit: technically permitted to withdraw under Article X with three months' notice, but face severe diplomatic and economic penalties if they attempt withdrawal. Constrained by security dependency on NPT signatories and threat of sanctions, yet the withdrawal clause creates a theoretical exit option that functions as both safety valve and coordination crisis point.
constraint_indexing:constraint_classification(npt_treaty_text__withdrawal_threshold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% P5 nuclear-weapon states benefit from a high withdrawal threshold (regime stability framing) that they interpret as requiring extraordinary circumstances to justify withdrawal. This interpretation preserves their own withdrawal optionality while constraining others. The coordination function is genuine: P5 states collectively maintain the NPT verification regime. The asymmetry: P5 states experience the constraint as coordination and maintain de facto veto over what constitutes valid withdrawal grounds.
constraint_indexing:constraint_classification(npt_treaty_text__withdrawal_threshold_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized but constrained. These states maintain theoretical nuclear weapons options (fuel cycle capability) while bound by NPT commitments. The high-threshold interpretation of Article X benefits them structurally by maintaining credible exit option while suppressing its visibility. Benefits coordination (security umbrella stability) while extracting compliance through ambiguity.
constraint_indexing:constraint_classification(npt_treaty_text__withdrawal_threshold_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From a sovereignty-law perspective, Article X withdrawal is presented as an immutable right of sovereign states under customary international law. No state can be permanently bound; the withdrawal clause is a natural limit on treaty obligation. However, the structural data reveals beneficiaries (P5 states interpreting high thresholds) and victims (subordinate states facing constrained exit). This mountain classification naturalizes what is actually a contested institutional interpretation.
constraint_indexing:constraint_classification(npt_treaty_text__withdrawal_threshold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_treaty_text__withdrawal_threshold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_treaty_text__withdrawal_threshold_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The high-threshold interpretation extracts compliance from subordinate states by maintaining ambiguity about their exit rights. The extraction is real — states remain locked in the NPT regime not because they freely choose coordination but because the cost of exiting is prohibitively high under this interpretation. However, extractiveness is not maximal (0.72+) because some coordination genuinely occurs (P5 states do maintain verification norms, subordinate states do genuinely benefit from security guarantees and NPT legitimacy). The tangled rope is appropriate: both coordination function (regime verification) and asymmetric extraction (hegemonic control over exit interpretation) are present. Suppression (0.58): Moderate-high. Subordinate states face multiple suppression mechanisms: economic sanctions threat, security guarantee withdrawal threat, diplomatic isolation, IAEA inspection leverage, and most critically, the interpretive ambiguity itself. Suppression is not total because withdrawal remains technically permissible under Article X. Theater ratio (0.65): Moderate-high and rising. The performative element increases as 'extraordinary circumstances' becomes more contested. Each NPT Review Conference stages debates about what would justify withdrawal without resolving the threshold. P5 states perform commitment to regime while maintaining veto power. Threshold states perform compliance while maintaining latent exit options. The theater rises over the measurement interval as the North Korea precedent is invoked more frequently without being resolved as binding precedent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from identical structural facts. P5 institutions see Rope: the Article X withdrawal clause functions as genuine coordination mechanism, setting boundaries on commitment. They perceive themselves as maintaining norms collectively. Subordinate states see Snare: the Article X withdrawal clause exists only to be withheld; the interpretation regime suppresses the right without eliminating its existence, creating a false exit option. Threshold states see Tangled Rope: they benefit from both the coordination (security guarantees) and the extraction (ambiguous exit rights that function as leverage). The analytical observer risks seeing Mountain: sovereignty is an immutable right, and the withdrawal clause merely reflects this natural law. The structural data reveals this as false summit — the beneficiaries (P5 states, threshold states) are identifiable, and the interpretation is contingent on institutional power asymmetry, not on law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position relative to this specific constraint. P5 states: d ≈ 0.15 (beneficiaries with arbitrage options — they can withdraw if they choose, and their interpretation controls what counts as legitimate). Subordinate NNWS: d ≈ 0.80 (targets with constrained exit — they cannot credibly withdraw despite technical rights). Threshold states: d ≈ 0.50 (symmetric — they benefit from the high-threshold interpretation as a safety valve while being locked in by it; their latent capacity gives them slightly more leverage than pure NNWS). The regime itself: d ≈ 0.95 (maximally targeted; has no defensive agency). The engine's directionality derivation from beneficiary/victim declarations plus power level and exit options produces these values without explicit override. The perspectival gap between P5 institutions (rope classification: they perceive genuine coordination) and subordinate powerless agents (snare classification: they perceive pure extraction) is substantial — this gap reveals the extraction mechanism operating through interpretation asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the tangled-rope classification is stable across all perspectives except those where the observer has hegemonic power. The coordination function (P5 regime verification) is genuine but minor compared to the extraction function (subordinate state lock-in through interpretation control). The constraint would be purely extractive (Snare) if the coordination function were eliminated; it would be pure coordination (Rope) if the interpretation were truly symmetrical between P5 and NNWS. The current tangled state persists because P5 states have incentive to maintain both: genuine regime verification (prevents rogue acquisition) and hegemonic control (prevents threshold states from rearming). Mandatrophy is resolved: this is unambiguously tangled_rope from the structural perspective, with perspectival variance driven by power asymmetry, not by classification ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_circumstances_definition,
    'What factual or legal conditions constitute ''extraordinary circumstances'' under Article X para 1?',
    'Jurisprudential consensus from ICJ advisory opinions, state practice in withdrawal notices, IAEA reports on the North Korea precedent, treaty body interpretations. The 2005 NPT Review Conference explicitly rejected attempts to define this threshold.',
    'If narrowly defined (strict): withdrawal perceived as effectively prohibited for NNWS; regime strengthens; subordinate states remain trapped (Snare from their perspective). If broadly defined (permissive): withdrawal becomes credible threat; P5 hegemonic control weakens; subordinate states gain leverage (Rope). The entire classification hinges on this ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraordinary_circumstances_definition, conceptual, 'Definition ambiguity for ''extraordinary circumstances'' triggering valid Article X withdrawal').

omega_variable(
    north_korea_precedent_binding_force,
    'Does the North Korean withdrawal (2003) create interpretive precedent for subsequent withdrawals, or is it a sui generis exception that does not generalize?',
    'State practice post-2003: has any state cited North Korea as justification for withdrawal threat? Has any state explicitly rejected North Korea as precedent? IAEA and ICJ statements on customary law evolution. Comparison with other treaty withdrawal disputes (Genocide Convention, Anti-Torture Protocol).',
    'If North Korea creates precedent: extraction mechanism degrades (subordinate states can credibly threaten withdrawal, reducing P5 hegemonic control). If North Korea remains exception: extraction mechanism persists (precedent denial allows high-threshold interpretation to continue suppressing exit option). Current ambiguity benefits P5 states by maintaining regime stability illusion without foreclosing hegemonic control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_korea_precedent_binding_force, conceptual, 'Interpretive status of North Korean withdrawal as precedent for others').

omega_variable(
    security_guarantee_substitution_credibility,
    'Do security guarantees (US extended deterrence, NATO Article 5 equivalents) meaningfully substitute for NNWS withdrawal rights, or do they function as extraction mechanism by making withdrawal economically/politically impossible?',
    'Comparative case study: states with high security guarantee dependence vs autonomy. Japan, South Korea, Germany exit-option analysis. RAND/CSIS assessments of security guarantee credibility post-Ukraine. Correlation between security dependency and NPT lock-in.',
    'If guarantees substitute: withdrawal is genuinely optional but irrational choice (Tangled Rope justified). If guarantees function as extraction: withdrawal becomes theoretically possible but practically prohibited (Snare classification for subordinate states is appropriate). Current ambiguity allows both framings to coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_guarantee_substitution_credibility, empirical, 'Whether security guarantees substitute for or extract around NPT withdrawal rights').

omega_variable(
    regime_stability_vs_sovereignty_preservation_incommensurability,
    'Are the regime stability priority (high withdrawal threshold) and sovereignty preservation priority (low/no threshold) logically incompatible within a single interpretive framework, or can they be balanced through proportionality doctrines?',
    'International law scholarship on treaty interpretation (Vienna Convention Articles 31–32). Jurisprudence on balancing competing norms. Whether subsequent NPT Review Conferences have produced consensus on threshold reconciliation (they have not as of 2026).',
    'If incommensurable: the two readings foreclose each other; only one framework can be adopted. If reconcilable: both readings can coexist in different institutional spaces (different P5 positions, different NNWS interpretations). Current state: institutional coexistence masks logical incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_stability_vs_sovereignty_preservation_incommensurability, conceptual, 'Logical reconcilability of regime stability vs sovereignty preservation priorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_wth_theater_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(npt_wth_theater_t5, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(npt_wth_theater_t10, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(npt_wth_extract_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(npt_wth_extract_t5, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(npt_wth_extract_t10, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(npt_wth_supp_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(npt_wth_supp_t5, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(npt_wth_supp_t10, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, north_korea_precedent_regime_drift).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, threshold_state_latent_capacity_equilibrium).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel. The sibling readings (NWS reading, NNWS reading) are separate constraint stories modeling how P5 states and non-nuclear-weapon states respectively interpret the same text. The three stories form a constraint family linked by the kernel. The withdrawal-threshold reading serves as the bridging constraint between the other two readings because it specifies the institutional mechanism through which the interpretation gap is maintained: P5 interpretive authority over 'extraordinary circumstances.' Upstream constraints (north_korea_precedent_regime_drift) supply the empirical ambiguity that this reading exploits; downstream constraints (threshold_state_latent_capacity_equilibrium) model the strategic consequences of maintaining the high-threshold interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, institutional, 0.12).
constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
