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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Article 2(7) Domestic Jurisdiction Bar — Sovereignty-First Reading
 *   domain: international law/political philosophy/security studies
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-first reading of the Article
 *   2(7)/Chapter VII kernel: sovereignty is foundational, and Chapter VII
 *   authorization is read narrowly, limited to inter-state aggression rather
 *   than to systematic domestic atrocity. Under this reading the domestic
 *   jurisdiction bar functions as a genuine post-colonial protective
 *   coordination device AND as an extraction mechanism that shields incumbent
 *   regimes' internal violence from outside remedy. The 1994 inflection
 *   reflects the post-Rwanda/Srebrenica period, when the gap between the
 *   bar's stated coordination purpose and its actual shielding function
 *   became empirically undeniable to observers outside the beneficiary set,
 *   even as the doctrine itself did not change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.71).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Article 2(7) Domestic Jurisdiction Bar — Sovereignty-First Reading").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international law/political philosophy/security studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e56ea8b8-bcea-4102-b4c1-01d56be3da9f').
narrative_ontology:cs_kernel_codification('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', fixed_text).
narrative_ontology:cs_authority_grounding('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', lineage).
narrative_ontology:cs_interpretation_layer_present('e56ea8b8-bcea-4102-b4c1-01d56be3da9f').
narrative_ontology:cs_reading_relation('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', foundational, sovereignty_is_unconditional_absent_interstate_aggression).
narrative_ontology:cs_axiom_status(sovereignty_is_unconditional_absent_interstate_aggression, holdable).
narrative_ontology:cs_axiom_grounding('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', sovereignty_is_unconditional_absent_interstate_aggression, conventional).
narrative_ontology:cs_axiom('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', secondary, great_power_intervention_pretext_risk_outweighs_atrocity_remedy_gap).
narrative_ontology:cs_axiom_status(great_power_intervention_pretext_risk_outweighs_atrocity_remedy_gap, holdable).
narrative_ontology:cs_axiom_grounding('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', great_power_intervention_pretext_risk_outweighs_atrocity_remedy_gap, instrumental).
narrative_ontology:cs_reference_frame('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', westphalian_non_interference_baseline).
narrative_ontology:cs_drift_state('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', post_rwanda_srebrenica_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e56ea8b8-bcea-4102-b4c1-01d56be3da9f', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_incumbent_regimes).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_governments).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, ethnic_and_religious_minorities_at_risk).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, displaced_and_besieged_civilians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Article 2(7)'s domestic jurisdiction bar to block outside scrutiny of internal repression, framing any external concern as illegal interference with sovereignty. Retain full control over internal security forces and can suppress dissent or target minority populations without triggering intervention as long as the violence is characterized as internal and does not cross into inter-state aggression.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_incumbent_regimes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_incumbent_regimes, agenda_setter).

% Rely on the sovereignty-first reading as a hard-won post-colonial protection against renewed great-power intrusion into internal affairs, having experienced intervention historically as a pretext for domination. Benefit from the norm even when not themselves repressive, because it forecloses a legal channel that could be selectively weaponized against weaker states.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_governments, beneficiary,
    organized, generational, constrained, national).

% Control the Chapter VII authorization gate through veto power. Can selectively permit or block intervention depending on alliance interests, using the sovereignty bar as a shield for allied regimes' internal conduct while lifting it against adversaries. Their veto is the actual enforcement mechanism that keeps the bar operative.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Face mass violence, ethnic cleansing, or systematic repression from their own government or from actors it tolerates, with no legal international mechanism that can act on their behalf absent Security Council authorization tied to inter-state aggression. Flight across borders is often the only available exit, and even that is frequently blocked or lethal.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% Targeted by state or state-tolerated violence characterized as an internal matter. The sovereignty bar means their situation must escalate to genocide framed convincingly enough to force Council consensus before any authorized response becomes possible, and consensus is routinely blocked by a patron's veto.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, ethnic_and_religious_minorities_at_risk, payer,
    powerless, immediate, trapped, local).

% Live under siege or in flight from internal conflict; humanitarian corridors and protection depend on host-state consent under the sovereignty-first reading, so an uncooperative government can block aid and protection indefinitely without breaching any binding international rule as understood under this reading.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, displaced_and_besieged_civilians, payer,
    powerless, immediate, trapped, regional).

% Document atrocities and advocate for access but hold no authority to act without either state consent or a Council authorization that this reading strictly limits to inter-state aggression scenarios. Their reporting can shape the debate but cannot itself unlock intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_secretariat_and_humanitarian_agencies, excluded,
    moderate, biographical, constrained, global).

% Analyze the doctrinal boundary between domestic jurisdiction and matters properly subject to Council action, producing the interpretive record that both readings of the kernel cite as authority.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, diffuse).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable rule against externally imposed regime change or great-power intervention dressed as humanitarianism, protecting weaker and post-colonial states from a legal channel that could otherwise be selectively invoked by powerful states against disfavored governments.
% TRANSFER_FUNCTION: Moves protection from populations facing internal state violence to incumbent governments' claim of non-interference; the cost of preserving strict sovereignty is borne by civilians who have no recourse once violence is characterized as domestic, while the benefit of insulation from scrutiny accrues to the governments controlling that characterization.
% ABSENT_VOICES: The populations actually suffering atrocity are not parties to Security Council deliberations and have no standing to trigger Chapter VII themselves; humanitarian agencies that document their situation can advocate but not authorize action. The sovereignty-first reading is negotiated among states, not among the people the bar is invoked against.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first reading disappeared and R2P-style intervention triggers became the default, authoritarian and post-colonial governments would lose a load-bearing legal shield and populations under atrocity would gain a contested pathway to outside action — but permanent members would likely still gate actual intervention through veto, so whether the world 'rearranges' or merely relabels the same veto-gated outcomes is exactly what the two readings dispute.
% FOUNDING_PROBLEM: Built after WWII to prevent great powers from using humanitarian or political pretexts to justify intervention and domination of weaker states, learned from a long history of colonial and imperial intervention dressed as civilizing or stabilizing missions.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and their diplomatic historians attest the founding problem (great-power pretextual intervention) remains live and cite recent interventions as vindication. Human rights bodies, genocide scholars, and some smaller states outside the incumbent-beneficiary set attest that the bar's operation now primarily shields atrocity-committing incumbents rather than protecting against great-power domination, and that the two functions have diverged sharply since the Rwanda and Srebrenica failures.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is high (0.78) because the bar's operation systematically forecloses remedy for the population actually harmed while the cost of maintaining it is borne almost entirely by that same population — not by the states invoking it. Suppression (0.71) is high because the veto mechanism actively and continuously blocks alternative Council authorization pathways; this is not passive doctrinal silence but an enforced gate. Theater ratio (0.42) is moderate-rising: increasing Council debate, resolutions, and commissions of inquiry occur without corresponding authorization, consistent with performative engagement substituting for the bar's original protective function against great-power domination.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian incumbents and post-colonial governments sit near the beneficiary end: the bar directly subsidizes their insulation from scrutiny (d low). Permanent members hold a dual position — nominal agenda-setters whose veto arbitrage lets them apply or waive the bar selectively, which is why their exit_options is coded arbitrage rather than institutional-trapped. Populations under atrocity sit at the full-target end: trapped, powerless, immediate horizon — the structural profile the engine amplifies toward maximal effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing pretextual great-power intervention — was genuinely live in 1945 and remains partially live today (great powers still use humanitarian framing instrumentally). But the founding problem's persistence does not mean the bar's current operation still serves it: the mismatch between founding_problem_status=contested and disappearance_verdict=contested is itself diagnostic — this is not a settled zombie mandate, but a mandate whose original and current beneficiary sets have partially diverged, which is exactly the tangled_rope signature rather than a clean rope or clean snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_bar_founding_vs_current_function,
    'Does the sovereignty-first reading still serve its 1945 founding function of blocking pretextual great-power intervention, or has it been substantially captured by incumbent regimes as a shield for domestic atrocity, with the founding function now largely vestigial?',
    'Comparative case analysis of Council votes and vetoes since 1994: coding each veto or blocked resolution by whether it protected a weaker state from great-power domination versus shielded an allied incumbent''s internal repression.',
    'If the shielding function now dominates numerically and structurally, the tangled_rope classification is confirmed and the coordination component is largely residual language rather than operative function; if the great-power-restraint function still dominates, the classification should weight toward rope with extraction as a minority side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_bar_founding_vs_current_function, empirical, 'Whether the bar''s coordination function has been substantially displaced by its extraction function since founding.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the sovereignty-first reading the CORRECT interpretation of Article 2(7)''s ''essentially domestic jurisdiction'' language, or is it one contested reading among several that the Charter text itself underdetermines?',
    'This is not resolvable by further textual analysis alone — the Charter''s drafting history is itself contested, and state practice since 1945 has produced no stable consensus (contrast Kosovo, Libya, Syria, Myanmar responses). Resolution would require either a binding ICJ advisory opinion squarely on point or a Charter amendment, neither of which is imminent.',
    'If the r2p_reading is or becomes the dominant interpretation through customary practice, this constraint (sovereignty_first_reading) would itself become a minority/dissenting reading rather than the operative kernel-reading, materially changing which populations receive nominal legal protection under the standing framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the sovereignty-first reading is the settled interpretation or one contested reading among live alternatives — the committer-frame ambiguity this story is one instance of.').

omega_variable(
    veto_arbitrage_vs_institutional_capture,
    'Is permanent-member veto use under this reading better modeled as rational arbitrage over alliance interests, or as institutional capture of the Chapter VII gate by a small coalition whose interests no longer track the Charter''s founding intent?',
    'Longitudinal analysis of veto patterns against alliance structures over multiple decades and multiple permanent members, checking whether veto use tracks the member''s own strategic relationships to the target state rather than any principled reading of Article 2(7).',
    'Arbitrage framing supports treating permanent members as beneficiaries with high effective agency (as currently coded); a capture framing would suggest the classification should treat the veto mechanism itself as a separate, more extractive sub-constraint warranting its own story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_arbitrage_vs_institutional_capture, conceptual, 'Whether permanent-member veto behavior under the sovereignty bar is best modeled as arbitrage or as separate institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(arti_tr_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(arti_tr_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(arti_tr_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1994, 0.35).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(arti_tr_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(arti_be_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(arti_be_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(arti_be_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1994, 0.72).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(arti_be_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(arti_su_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(arti_su_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(arti_su_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1994, 0.65).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(arti_su_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.1).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, r2p_reading).

% DUAL FORMULATION NOTE:
% This story and r2p_reading are the two live readings of the article_2_7_chapter_vii_tension kernel, sharing the same Charter text and Security Council institution but authorizing structurally opposite responses to domestic atrocity. sovereignty_first_reading authors ε=0.78 for the standing sovereignty-bar arrangement as this reading's own lights see it (high, since the bar as this reading operationalizes it blocks remedy for atrocity victims); r2p_reading would author a different ε for the same standing arrangement viewed through its own conditional-sovereignty lens. Per the ε-invariance principle, these are not the same constraint measured two ways — they are two constraints instantiating two different readings of one contested kernel, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
