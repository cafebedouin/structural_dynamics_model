% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 as Threshold on Inherent Defensive Right (Minimum-Necessary-Force Doctrine)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the inherent-right reading of the Article 9
 *   kernel: sovereign states retain an unwritten, customary-law right to
 *   self-defense that the renunciation clause does not and cannot extinguish,
 *   so Article 9 is read as prohibiting aggressive 'war' while leaving a
 *   'minimum necessary' defensive capacity constitutionally untouched. Under
 *   this reading the text functions as a threshold test applied to force
 *   posture, not a categorical ban. This is the doctrinal foundation the
 *   Japanese government and the Cabinet Legal Bureau have used since 1954 to
 *   sustain the Self-Defense Forces and, cumulatively, to expand their
 *   permitted role. The sibling readings (strict_pacifist_reading,
 *   collective_self_defense_reading) are separate constraints with their own
 *   ε and stakeholder structures; this file does not average across them.
 *
 * KEY AGENTS:
 *   - japanese_executive_and_defense_ministry: agenda_setter, expands the threshold through interpretive opinion
 *   - self_defense_forces_personnel: organizationally dependent beneficiary/payer of doctrinal ambiguity
 *   - united_states_security_alliance_planners: institutional beneficiary of Japan's defensive capacity
 *   - pacifist_constitutional_movement: payer, textualist objection unheeded for seven decades
 *   - regional_neighbors_wary_of_remilitarization: excluded payer bearing security-dilemma costs
 *   - supreme_court_of_japan: observer, perpetual political-question abstention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.42).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.38).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 as Threshold on Inherent Defensive Right (Minimum-Necessary-Force Doctrine)").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'e5a2c471-88d0-40fb-96ec-f23350cfa7f9').
narrative_ontology:cs_kernel_codification('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', fixed_text).
narrative_ontology:cs_authority_grounding('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', extraction).
narrative_ontology:cs_interpretation_layer_present('e5a2c471-88d0-40fb-96ec-f23350cfa7f9').
narrative_ontology:cs_reading_relation('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', foundational, customary_international_law_preserves_inherent_defense_right).
narrative_ontology:cs_axiom_status(customary_international_law_preserves_inherent_defense_right, holdable).
narrative_ontology:cs_axiom_grounding('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', customary_international_law_preserves_inherent_defense_right, conventional).
narrative_ontology:cs_axiom('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', foundational, renunciation_clause_targets_aggressive_war_not_defensive_capacity).
narrative_ontology:cs_axiom_status(renunciation_clause_targets_aggressive_war_not_defensive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', renunciation_clause_targets_aggressive_war_not_defensive_capacity, conventional).
narrative_ontology:cs_axiom('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', secondary, minimum_necessary_force_is_judicially_administrable_threshold).
narrative_ontology:cs_axiom_status(minimum_necessary_force_is_judicially_administrable_threshold, holdable).
narrative_ontology:cs_axiom_grounding('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', minimum_necessary_force_is_judicially_administrable_threshold, instrumental).
narrative_ontology:cs_reference_frame('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', occupation_era_demilitarization_compromise).
narrative_ontology:cs_drift_state('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', post_2015_security_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5a2c471-88d0-40fb-96ec-f23350cfa7f9', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_executive_and_defense_ministry).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, united_states_security_alliance_planners).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, domestic_defense_industrial_base).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_constitutional_movement).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, regional_neighbors_wary_of_remilitarization).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, diet_minority_opposition_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, self_defense_forces_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, self_defense_forces_personnel).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, sovereign_state_inherent_self_defense_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, minimum_necessary_force_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers the 'minimum necessary' standard through cabinet legal bureau opinions, defense white papers, and procurement decisions. Sets the operational boundary of what counts as defensive rather than offensive capability, and has progressively expanded that boundary (e.g. long-range strike capability, collective self-defense legislation of 2015) while continuing to invoke the inherent-right reading as the stable doctrinal anchor.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_executive_and_defense_ministry, agenda_setter,
    institutional, generational, arbitrage, national).

% Serve in an organization whose legal legitimacy rests entirely on this reading of Article 9. Benefit from organizational stability and public funding, but bear the cost of permanent constitutional ambiguity about their own status — never quite a military, never quite civilian, with career and legal protections contingent on doctrine that could shift under a different reading.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, self_defense_forces_personnel, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, self_defense_forces_personnel, payer).

% Rely on Japan maintaining defensive capacity sufficient to share alliance burden without triggering treaty-text prohibitions. The inherent-right reading is the doctrinal precondition for US basing strategy, cost-sharing, and regional deterrence planning; they lobby quietly for expansive readings of 'minimum necessary.'
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, united_states_security_alliance_planners, beneficiary,
    institutional, generational, arbitrage, continental).

% Receives procurement contracts justified under the minimum-necessary-defense threshold. Each expansion of what counts as 'defensive' (missile defense, expeditionary logistics, counterstrike systems) is a direct revenue event; the doctrine's elasticity is a business asset.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, domestic_defense_industrial_base, beneficiary,
    organized, biographical, arbitrage, national).

% Civil society groups, constitutional scholars, and war-memory constituencies who read the postwar settlement as a categorical commitment. They experience each doctrinal expansion under 'minimum necessary' as an erosion of a promise they treat as foundational to postwar national identity; their objections are heard in courts and elections but have not altered the operative doctrine in seven decades.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_constitutional_movement, payer,
    moderate, generational, constrained, national).

% States with historical grievance from Japanese wartime aggression monitor SDF capability growth as a security-dilemma signal. They bear diplomatic and security-planning costs whenever Japan's 'minimum necessary' threshold rises, but have no standing inside Japan's domestic constitutional process to contest the reading directly.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, regional_neighbors_wary_of_remilitarization, payer,
    powerful, generational, constrained, regional).

% Repeatedly challenge cabinet reinterpretations of Article 9 in the legislature and in court, arguing the executive's doctrine-by-cabinet-opinion process bypasses the formal amendment procedure required by Article 96. Their objections are recorded in Diet proceedings but the ruling coalition's legislative majority has been sufficient to enact each expansion regardless.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, diet_minority_opposition_parties, excluded,
    moderate, biographical, constrained, national).

% Has repeatedly declined to rule on the substantive constitutionality of the SDF or the minimum-necessary standard, treating it as a political question. Its abstention functions as tacit ratification of whatever the political branches settle on, without ever adjudicating the doctrine on the merits.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, supreme_court_of_japan, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__inherent_right_reading, japanese_executive_and_defense_ministry).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Japan with a legally cognizable basis for organized territorial defense and alliance participation without requiring the formally difficult two-thirds Diet and national-referendum amendment process — the inherent-right doctrine lets the state coordinate defense policy through executive/cabinet legal interpretation rather than constitutional revision.
% TRANSFER_FUNCTION: Moves interpretive authority over the scope of 'defense' from the constitutional amendment process (requiring supermajority and referendum) to the executive and cabinet legal bureau, and moves fiscal and political capital toward defense procurement and alliance commitments that would otherwise require an explicit, contestable constitutional change.
% ABSENT_VOICES: Regional neighbors have no formal seat in Japan's domestic doctrinal process despite bearing security-dilemma costs from each expansion; the pacifist movement's textualist objections are aired in courts and media but the Supreme Court's persistent political-question abstention means the doctrine is never tested against the strict_pacifist_reading on the merits.
% DISAPPEARANCE_RATIONALE: If the inherent-right reading were displaced by the strict pacifist reading, the Self-Defense Forces would lack a constitutional basis and would require either disbandment or formal Article 96 amendment; US-Japan alliance planning, regional deterrence postures, and domestic procurement flows built on seven decades of this doctrine would need complete reconstruction.
% FOUNDING_PROBLEM: In the immediate postwar period, the doctrine solved the problem of how a demilitarized, occupied state facing Cold War security threats (Korean War onset, Soviet proximity) could develop territorial defense capacity without amending a constitution imposed under occupation and without appearing to repudiate its war-renunciation commitment to the region.
% FOUNDING_PROBLEM_CORROBORATION: The government and allied security establishment attest the underlying problem — credible territorial defense absent a mutual defense treaty with unconditional US commitment — remains live given regional threat environment. Independent constitutional scholars and the pacifist movement, whose interests lie outside the beneficiary set, attest that the original 1950s security rationale has been progressively supplanted by open-ended capability expansion (collective self-defense, strike capability) that exceeds any plausible 'minimum necessary' reading of the founding compromise; no neutral corroborating body outside government and its allied security planners affirms the current scope as still matching the founding problem.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) and suppression (0.38) are both moderate rather than extreme: the coordination function is real (a demilitarized, occupied state needed a workable route to territorial defense without immediate constitutional revision), but the doctrine has been used to progressively widen defensive scope (missile defense, expeditionary logistics, 2015 collective self-defense legislation) well beyond the 1950s security rationale, without ever subjecting the widening to the formal Article 96 amendment process the constitution actually specifies for such changes. Theater ratio (0.45, rising) reflects that cabinet reinterpretation increasingly substitutes for the constitutionally prescribed amendment procedure — a proxy process replacing the textually mandated one. Accessibility collapse (0.5) and resistance (0.55) sit mid-range: the strict pacifist and collective self-defense alternatives remain live, contested doctrinal positions (this is precisely why they are separate sibling stories), and the pacifist movement and opposition parties mount real, sustained resistance even though it has not altered outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive/Defense Ministry is the clearest beneficiary — it both administers and expands the threshold, arbitrage-level exit (it can reinterpret rather than be bound). The US alliance planners and defense industrial base are downstream institutional/organized beneficiaries whose access to Japanese capability growth depends on the doctrine holding. SDF personnel occupy a dual position: beneficiaries of institutional existence, but payers of chronic legal-status ambiguity, hence the secondary_role. Pacifist constitutional movement and opposition parties are payers/excluded — their textualist reading is structurally what the inherent-right reading displaces, and their exit options are constrained (electoral and judicial channels exist but have not reversed doctrine). Regional neighbors are payers at a different scope entirely (regional, not national), bearing externalized security-dilemma cost with no seat in the domestic process — an important asymmetry the tangled_rope structure captures via requires_active_enforcement (executive branch actively maintains the interpretive apparatus) plus named beneficiaries and victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible defense without amendment, under occupation-era security pressure) was time-bound to the early Cold War; the doctrine's continued operation at an expanded scope in 2024 raises exactly the founding_problem_status='contested' flag this schema is designed to surface — government-aligned actors say the problem persists (regional threats), corroborating outside voices say the doctrine now exceeds its original justification. Because disappearance_verdict is 'world_rearranges' (SDF's legal basis, alliance planning, and procurement all depend on this reading), the mismatch between a 'dead-original-purpose, alive-in-practice' status and a 'world would rearrange if removed' verdict is the corpus's signal for a capture/drift pattern worth flagging, not proof of illegitimacy on its own.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_threshold_indeterminacy,
    'Is there a principled, judicially enforceable content to ''minimum necessary for territorial defense,'' or is the threshold infinitely elastic in the hands of whichever cabinet legal bureau opinion is current?',
    'A Supreme Court ruling on the merits (rather than continued political-question abstention) that articulates a limiting principle, or comparative analysis of whether any proposed capability has ever actually been rejected under the standard.',
    'If no capability has ever been rejected under the standard in seventy years, the threshold reading collapses toward a rope with no binding function (any level of force is retroactively ''minimum necessary''), pushing the classification toward snare/tangled_rope with the pacifist reading vindicated as the more accurate description of what the text was meant to do. If the standard has real bite, the tangled_rope reading with genuine coordination function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_threshold_indeterminacy, empirical, 'Whether the minimum-necessary standard has ever actually constrained a specific capability decision.').

omega_variable(
    amendment_bypass_versus_valid_interpretation,
    'Is cumulative cabinet reinterpretation of Article 9 a legitimate exercise of ordinary constitutional interpretation, or a de facto amendment achieved without invoking Article 96''s supermajority-plus-referendum procedure?',
    'Comparative constitutional analysis of interpretive drift magnitude relative to formal amendment thresholds; examination of whether any single interpretive step, taken alone, would have passed a referendum test.',
    'If reinterpretation functions as amendment-by-other-means, the doctrine''s legitimacy rests on executive fiat rather than popular sovereignty, strengthening the case for reclassifying the whole apparatus as extractive of the amendment process''s coordination function rather than as valid ordinary interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_bypass_versus_valid_interpretation, conceptual, 'Whether doctrinal accretion substitutes for the formal amendment process the constitution specifies.').

omega_variable(
    kernel_reading_selection_evidence,
    'What structural evidence favors the inherent-right reading over the strict-pacifist or collective-self-defense readings as the operative one, apart from its having been the government''s chosen doctrine since 1954?',
    'Textual and drafting-history analysis (GHQ negotiations, Ashida Amendment debates) weighed against seven decades of unbroken governmental practice and the absence of any successful judicial or electoral reversal.',
    'If drafting history strongly favors the strict pacifist reading, the inherent-right reading''s persistence looks more like successful institutional capture of interpretive authority than genuine constitutional meaning; if practice-based legitimacy is given weight, the inherent-right reading is the settled, functioning doctrine regardless of original drafting intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Whether textual/historical evidence or settled practice should be the deciding criterion among the three sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1954, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1954, 0.3).
narrative_ontology:measurement(arti_tr_t1976, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1976, 0.35).
narrative_ontology:measurement(arti_tr_t1992, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1992, 0.38).
narrative_ontology:measurement(arti_tr_t2003, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2003, 0.4).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t1954, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1954, 0.2).
narrative_ontology:measurement(arti_be_t1976, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1976, 0.28).
narrative_ontology:measurement(arti_be_t1992, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1992, 0.33).
narrative_ontology:measurement(arti_be_t2003, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2003, 0.36).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1954, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1954, 0.25).
narrative_ontology:measurement(arti_su_t1976, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1976, 0.28).
narrative_ontology:measurement(arti_su_t1992, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1992, 0.3).
narrative_ontology:measurement(arti_su_t2003, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2003, 0.32).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__inherent_right_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, us_japan_security_treaty_burden_sharing).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the article_9_war_renunciation kernel, decomposed per the ε-invariance principle because the natural-language label 'Article 9' covers structurally distinct claims with different ε values: strict_pacifist_reading treats any armed force as categorically unconstitutional (near-zero coordination function once SDF existence is at stake, near-total extraction from the amendment process if maintained anyway); inherent_right_reading (this file) treats defensive capacity below a minimum-necessary threshold as constitutionally permitted (moderate ε, genuine but bounded coordination function); collective_self_defense_reading extends the inherent right further to ally defense (expected higher ε, since it detaches the doctrine from Japan's own territorial necessity). Each reading is linked here and should reciprocally link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
