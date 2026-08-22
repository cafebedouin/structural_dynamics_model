% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity (International Oversight Reading)
 *   domain: international_law/criminal_justice
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes 'complementarity': the ICC may
 *   only exercise jurisdiction when national courts are 'unwilling or unable'
 *   to prosecute. This constraint instantiates ONE READING of that
 *   article—the international oversight reading—which interprets
 *   complementarity as an accountability-trigger mechanism. Under this
 *   reading, 'unwilling or unable' is interpreted broadly to capture victor's
 *   justice, sham proceedings, elite immunity, and technical compliance
 *   without genuine independence. The ICC becomes guardian against impunity
 *   in failed or complicit states. The sibling reading
 *   (national_primacy_reading) interprets complementarity as a
 *   sovereignty-protection mechanism that presumes national adequacy unless
 *   proven sham. These readings generate fundamentally different ε values and
 *   beneficiary/victim structures from the same textual kernel. This story
 *   authors the international oversight reading's ε, beneficiaries, victims,
 *   and structural dynamics independently.
 *
 * KEY AGENTS:
 *   - ICC Prosecutor Office: sets admissibility standards via broad 'unwilling or unable' interpretation; controls intervention trigger
 *   - Victims in failed/complicit states: powerless, trapped—primary beneficiaries of low-threshold access to ICC forum
 *   - Accused elites in state-party governments: powerful, constrained—face ICC prosecution under broad complementarity reading
 *   - State governments (ICC parties): institutional, constrained—bear costs of cooperation demands and sovereignty challenges
 *   - Domestic courts in complicit states: moderate power, identity-locked—forced to choose between state protection and ICC admissibility standards
 *   - Non-state-party governments: excluded, arbitrage—provide safe harbor for accused elites outside ICC reach
 *   - International justice advocates: organized, mobile—support broad reading as necessary for accountability
 *   - Global South sovereignty advocates: excluded, constrained—argue reading is neo-colonial targeting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.68).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.71).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity (International Oversight Reading)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '7820f255-3e2e-48d2-87c4-a07affc26ce0').
narrative_ontology:cs_kernel_codification('7820f255-3e2e-48d2-87c4-a07affc26ce0', fixed_text).
narrative_ontology:cs_authority_grounding('7820f255-3e2e-48d2-87c4-a07affc26ce0', lineage).
narrative_ontology:cs_interpretation_layer_present('7820f255-3e2e-48d2-87c4-a07affc26ce0').
narrative_ontology:cs_reading_relation('7820f255-3e2e-48d2-87c4-a07affc26ce0', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('7820f255-3e2e-48d2-87c4-a07affc26ce0', foundational, broad_admissibility_enables_accountability).
narrative_ontology:cs_axiom_status(broad_admissibility_enables_accountability, holdable).
narrative_ontology:cs_axiom_grounding('7820f255-3e2e-48d2-87c4-a07affc26ce0', broad_admissibility_enables_accountability, deontological).
narrative_ontology:cs_axiom('7820f255-3e2e-48d2-87c4-a07affc26ce0', foundational, sham_proceedings_defeat_complementarity).
narrative_ontology:cs_axiom_status(sham_proceedings_defeat_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('7820f255-3e2e-48d2-87c4-a07affc26ce0', sham_proceedings_defeat_complementarity, empirically_contingent).
narrative_ontology:cs_reference_frame('7820f255-3e2e-48d2-87c4-a07affc26ce0', accountability_supremacy_framework).
narrative_ontology:cs_drift_state('7820f255-3e2e-48d2-87c4-a07affc26ce0', contemporary_expanded_jurisdiction, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7820f255-3e2e-48d2-87c4-a07affc26ce0', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_failed_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, icc_institutional_authority).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, accused_elites_in_complicit_states).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, state_sovereignty_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_justice_advocates).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, state_governments_party_to_icc).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, domestic_courts_in_complicit_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets complementarity as an accountability-trigger: ICC intervenes whenever domestic proceedings lack genuine independence or show signs of sham prosecution. Controls admissibility determination via broad reading of 'unwilling or unable.' Sets the agenda by expanding the definition of what counts as failed prosecution. Can threaten intervention to pressure state cooperation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_prosecutor_office, agenda_setter,
    institutional, generational, analytical, global).

% Lack domestic remedy when their governments are complicit in or indifferent to atrocities. ICC's low-threshold admissibility reading creates access to justice they would not otherwise have. Their cases become admissible at ICC when the prosecutor interprets 'unwilling or unable' broadly to capture elite immunity and victor's justice scenarios.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_failed_states, beneficiary,
    powerless, biographical, trapped, global).

% Face ICC prosecution under a low admissibility threshold that overrides their home state's protective proceedings or non-prosecution. The broad 'unwilling or unable' reading expands ICC jurisdiction to capture cases where domestic proceedings are technically initiated but lack genuine independence. Exit options are limited: fleeing to non-state-party jurisdictions, securing immunity agreements that are now contestable, or attempting to block ICC cooperation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, accused_elites_in_complicit_states, payer,
    powerful, biographical, constrained, global).

% Are bound by Rome Statute obligations to cooperate with ICC investigations and enforce warrants, even when prosecution targets their own elites or challenges their sovereignty claims. The broad complementarity reading intensifies these cooperation demands. Their option set is formal: comply with Rome obligations, withdraw from the treaty (at geopolitical cost), or obstruct cooperation (at legal/reputational cost).
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, state_governments_party_to_icc, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, state_governments_party_to_icc, excluded).

% Are not party to the Rome Statute and thus not bound by complementarity provisions, creating a safe harbor for accused elites who can secure protection or non-surrender agreements. Their exclusion from ICC jurisdiction is what the enforcement mechanisms must work around.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, non_state_party_governments, excluded,
    institutional, generational, arbitrage, global).

% Are caught between ICC admissibility demands and state political pressure. Under the broad complementarity reading, their proceedings are subject to ICC scrutiny for 'genuine independence' and 'genuine intent.' They cannot both satisfy their state's protective agenda and meet ICC admissibility standards; the constraint forces a choice that most cannot survive institutionally.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, domestic_courts_in_complicit_states, payer,
    moderate, generational, identity_locked, global).

% Support the broad complementarity reading as the only mechanism available to reach elites in states that would otherwise protect them. They argue this reading is essential for universal accountability and see ICC intervention as necessary precisely because domestic systems fail.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_justice_advocates, beneficiary,
    organized, generational, mobile, global).

% Argue the broad complementarity reading is neo-colonial: ICC targets African and non-Western elites disproportionately while Northern hemisphere actors face less scrutiny. Their objection is structural but excludes them from ICC governance; they are represented by their state governments, whose interests may diverge.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, global_south_sovereignty_advocates, excluded,
    organized, generational, constrained, global).

% May be displaced or undermined by ICC prosecution agendas under the broad complementarity reading. A truth commission that grants amnesty or focuses on non-punitive accountability can be found 'unwilling' to prosecute under the international reading, triggering ICC intervention and interrupting local peace processes.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, truth_and_reconciliation_commissions, excluded,
    moderate, biographical, constrained, national).

% Analyzes the complementarity doctrine and its interpretations. Scholars holding the international oversight reading see broad admissibility as necessary for accountability; those holding the national primacy reading see it as judicial overreach.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, observer_international_law_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, icc_prosecutor_office).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes ICC as a backstop mechanism that activates when domestic accountability mechanisms are unavailable, corrupt, or performative. Solves the collective-action problem of impunity in failed or complicit states by creating an external forum.
% TRANSFER_FUNCTION: Moves prosecutorial power from potentially compromised state judiciaries to the ICC whenever the prosecutor determines domestic proceedings lack independence or genuine intent. Transfers jurisdiction costs (state cooperation demands, loss of sovereignty claims) from the international community to the state harboring accused elites.
% ABSENT_VOICES: Non-state-party governments (which control many of the accused elites the reading would reach) are structurally excluded from ICC governance and cannot contest admissibility determinations. Global South voices arguing the reading produces neo-colonial targeting are represented only through their state governments, creating a double exclusion when those governments resist ICC intervention.
% DISAPPEARANCE_RATIONALE: If the broad complementarity reading disappeared and complementarity reverted to national-primacy terms, ICC admissibility would contract substantially. States could shield elites behind sham or partial domestic proceedings that meet only formalistic criteria. Victims in failed states would lose their primary avenue for accountability. The constraint's persistence directly enables prosecution pathways that would otherwise close.
% FOUNDING_PROBLEM: Genocides, crimes against humanity, and war crimes persist in states where government elites are perpetrators or complicit, ensuring domestic prosecution will not occur. A purely national complementarity standard permits indefinite impunity. ICC was created to fill this gap.
% FOUNDING_PROBLEM_CORROBORATION: International justice advocates and victims' representatives attest the founding problem is live and worsening: domestic prosecutions of state elites remain rare and politically blocked in most conflict-affected states. National governments and sovereignty advocates argue the founding problem has been overstated and used to justify ICC overreach beyond its mandate. Independent analyses of complementarity case law (Schabas, Stahn, International Crisis Group) document the doctrine's expansion and debate its justification.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.38 (2002, early complementarity framework) to 0.68 (2026, matured broad interpretation). This trajectory reflects the ICC's jurisprudential expansion of 'unwilling or unable' to capture elite immunity, victor's justice, and sham proceedings—moving from a high bar for intervention (genuine inability) to a low bar (lack of independence, selective prosecution, inadequate intent). Suppression requirement rises in parallel (0.45 → 0.71), tracking increased enforcement pressure needed to maintain cooperation from accused-state governments as ICC jurisdiction expands. Theater ratio rises but remains moderate (0.28 → 0.44), indicating the constraint's accountability function is real but increasingly intertwined with prosecutorial discretion and geopolitical targeting. The one-time-grid constraint ensures all three metrics are authored at every measured point (2002, 2008, 2014, 2020, 2026).
 *
 * PERSPECTIVAL GAP:
 *   The ICC prosecutor office and victim seats compute this constraint as access-enabling coordination (low d, high benefit). State governments party to ICC and accused elites compute it as extractive sovereignty violation (high d, high cost). Domestic courts are caught between: they simultaneously benefit from ICC oversight (it protects them from state pressure) and pay costs (they cannot shield elites their government demands they protect). The engine computes per-seat classifications from beneficiary/victim declarations and exit options; this structural divergence is not adjudicated by the claim but measured by the engine against the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: (1) victims_in_failed_states—powerless, trapped, biography-scale—gain access to justice they cannot reach domestically; directionality near 0.0 (full beneficiary). (2) icc_institutional_authority—institutional power, captures prosecutorial agenda-setting, no exit—directionality ~0.15 (institutional beneficiary with minor supervision constraints). Victims: (1) accused_elites_in_complicit_states—powerful, constrained exit (flee to non-parties, secure immunity)—directionality ~0.85 (full target). (2) state_sovereignty_claims—institutional, constrained by Rome obligations—directionality ~0.75 (structural target, but with some legitimate coordination residue). No directionality overrides applied; structural derivation from beneficiary/victim and exit produces coherent positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope: it solves a genuine coordination problem (accountability in failed states) while asymmetrically extracting sovereignty costs from state-party governments and prosecution costs from accused elites. Beneficiaries (victims) and payers (accused states, elites, domestic courts) are structurally distinct. Active enforcement is required: ICC must maintain prosecutorial pressure, states must be induced to cooperate, accused elites must be extradited against their home state's resistance. This rules out pure rope (no asymmetric extraction) and pure snare (coordination function is genuine, not cover). The theater ratio (0.44) is moderate, indicating some prosecutorial performance (selective targeting of weaker states, public awareness campaigns) but not primarily theatrical—the core function (reaching unreachable elites) is authentic and would collapse without enforcement. Mandatrophy is not resolved: the founding problem (elite impunity in failed states) remains contested—some argue ICC expansion has solved it, others argue it remains live and worsening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independence_threshold_ambiguity,
    'What count as sufficient ''genuine independence'' and ''genuine intent'' in a domestic prosecution to defeat ICC admissibility under the broad complementarity reading?',
    'Systematic review of complementarity jurisprudence and case outcomes; comparison of ICC admissibility determinations against prosecutor office internal guidance. Document what factual conditions trigger ''unwilling'' vs. ''unable'' findings.',
    'If the threshold is lowered to include technical non-compliance or selectivity without corruption, ICC jurisdiction expands further (extractiveness rises toward 0.8+); if threshold is held to manifest shams, extractiveness plateaus. This determines whether the reading''s expansion has limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_threshold_ambiguity, empirical, 'Whether ''genuine independence'' is defined by institutional structure or by case-outcome neutrality.').

omega_variable(
    neo_colonial_targeting_bias,
    'Does the broad complementarity reading produce structurally asymmetric targeting of Global South and non-Western elites while Northern hemisphere actors face lower scrutiny?',
    'Quantitative analysis of ICC prosecutions by geography, power level, and alliance patterns. Comparison of ICC intervention rates against state sovereignty and geopolitical alignment. Track whether symmetric crimes in Western states trigger equal admissibility review.',
    'If substantial targeting bias exists, the constraint functions as a legitimized neo-colonial extraction mechanism, re-classifying from tangled_rope toward snare. If symmetric, the reading is genuinely accountability-oriented. Bias is not necessarily fabricated—it may be structural to state-party composition and cooperation capacity—but it affects classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neo_colonial_targeting_bias, empirical, 'Whether the broad complementarity reading produces asymmetric extraction from Global South sovereignties.').

omega_variable(
    complementarity_reading_kernel_contention,
    'Which reading of Article 17 complementarity is authoritatively correct: international_oversight or national_primacy?',
    'This is not empirically resolvable—it is a question of legal interpretation and institutional power. Different authoritative bodies (ICC plenary, national constitutional courts, International Court of Justice) may rule differently. The resolution is institutional/political, not evidentiary.',
    'If national_primacy reading gains institutional authority (e.g., via ICJ advisory opinion or state coalition pressure on ICC), the entire constraint re-classifies: extractiveness drops (ICC jurisdiction shrinks), beneficiaries shift (states gain protection), victims shift (victims in failed states lose access). Both readings remain coherent; the question is which institutional seat''s interpretation prevails. This omega documents that the classification is reading-contingent, not kernel-invariant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complementarity_reading_kernel_contention, conceptual, 'The structural contestation of the complementarity kernel itself—which sibling reading will institutional power endorse.').

omega_variable(
    domestic_court_identity_lock_trajectory,
    'When domestic courts in complicit states are forced to choose between state loyalty (identity fusion with their government) and ICC admissibility standards, what is the post-exit suppression trajectory?',
    'Longitudinal studies of domestic judges who defect to ICC-aligned prosecution (leave state service, become international prosecutors, face retaliation). Track whether suppression persists after institutional exit or dissolves, indicating structural vs. internalized component.',
    'If suppression persists post-exit (internalized identity lock), the effective suppression is higher than the structural measure; courts face double-binding, and the constraint''s hold is stronger. If suppression drops after institutional exit, the constraint is primarily structural coercion. This informs whether identity-locked classification holds or whether constrained is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_court_identity_lock_trajectory, empirical, 'Whether suppression in domestic courts is structural or internalized via institutional identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.28).
narrative_ontology:measurement_basis(arti_tr_t2002, observed).
narrative_ontology:measurement(arti_tr_t2008, article_17_complementarity__international_oversight_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement_basis(arti_tr_t2008, observed).
narrative_ontology:measurement(arti_tr_t2014, article_17_complementarity__international_oversight_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement_basis(arti_tr_t2014, observed).
narrative_ontology:measurement(arti_tr_t2020, article_17_complementarity__international_oversight_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(arti_tr_t2020, observed).
narrative_ontology:measurement(arti_tr_t2026, article_17_complementarity__international_oversight_reading, theater_ratio, 2026, 0.44).
narrative_ontology:measurement_basis(arti_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.38).
narrative_ontology:measurement_basis(arti_be_t2002, observed).
narrative_ontology:measurement(arti_be_t2008, article_17_complementarity__international_oversight_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement_basis(arti_be_t2008, observed).
narrative_ontology:measurement(arti_be_t2014, article_17_complementarity__international_oversight_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement_basis(arti_be_t2014, observed).
narrative_ontology:measurement(arti_be_t2020, article_17_complementarity__international_oversight_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement_basis(arti_be_t2020, observed).
narrative_ontology:measurement(arti_be_t2026, article_17_complementarity__international_oversight_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(arti_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.45).
narrative_ontology:measurement_basis(arti_su_t2002, observed).
narrative_ontology:measurement(arti_su_t2008, article_17_complementarity__international_oversight_reading, suppression_requirement, 2008, 0.54).
narrative_ontology:measurement_basis(arti_su_t2008, observed).
narrative_ontology:measurement(arti_su_t2014, article_17_complementarity__international_oversight_reading, suppression_requirement, 2014, 0.62).
narrative_ontology:measurement_basis(arti_su_t2014, observed).
narrative_ontology:measurement(arti_su_t2020, article_17_complementarity__international_oversight_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(arti_su_t2020, observed).
narrative_ontology:measurement(arti_su_t2026, article_17_complementarity__international_oversight_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(arti_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__international_oversight_reading, 0.18).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, state_cooperation_extraction_compliance_regimes).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, universal_jurisdiction_extraterritorial_prosecution).

% DUAL FORMULATION NOTE:
% Article 17 complementarity decomposes into two structurally distinct constraints, one for each authoritative reading of the Rome Statute text. The international_oversight_reading (this story) interprets complementarity as an accountability-trigger with broad 'unwilling or unable' standards, enabling ICC intervention in failed/complicit states. The national_primacy_reading (sibling constraint) interprets complementarity as a sovereignty-protection mechanism, presuming national adequacy unless proven sham. These are not alternative measurements of one constraint—they generate different ε values (0.68 vs. ~0.35), different beneficiary/victim structures, and different institutional incentives. Both readings remain live institutional positions (no foreclosure), creating a genuine kernel contestation. The network edge signals downstream influence: if the international_oversight reading's jurisdiction expands, it pressures national governments toward universal jurisdiction mechanisms (upstream causality) and constrains their use of non-prosecution agreements (downstream influence on state_cooperation_extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
