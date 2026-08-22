% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Article 17 Complementarity — International Oversight Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the international-oversight reading of the
 *   Article 17 complementarity kernel: the ICC as the backstop against
 *   impunity, interpreting 'unwilling or unable' broadly enough to capture
 *   sham prosecutions, captured judiciaries, and victor's-justice
 *   arrangements engineered to shield elites. Under this reading, low
 *   admissibility thresholds are a feature, not a defect — they are what make
 *   the mechanism capable of piercing engineered domestic immunity. The
 *   sibling reading (national_primacy_reading, not this file) treats the same
 *   kernel text as a sovereignty-protection device with a high bar for ICC
 *   intervention; that is a structurally distinct constraint with its own ε
 *   and stakeholder set, linked via network.affects_constraints, not folded
 *   into this one.
 *
 * KEY AGENTS:
 *   - icc_prosecutorial_office: institutional agenda-setter constructing and applying the broad admissibility standard
 *   - atrocity_victims_in_complicit_states: primary beneficiaries — otherwise have no accountability pathway
 *   - targeted_state_elites_facing_sham_immunity: primary targets — lose the shield sham domestic proceedings were meant to provide
 *   - cooperating_states_bearing_referral_costs: bear diplomatic and resource costs of enabling the mechanism
 *   - great_power_non_states_parties: analytical absence — structurally exempt from the scrutiny applied to weaker states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.42).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.38).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity — International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '7c6a7a2b-3153-4936-8b29-d75ac01fe0ec').
narrative_ontology:cs_kernel_codification('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', fixed_text).
narrative_ontology:cs_authority_grounding('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', practice).
narrative_ontology:cs_interpretation_layer_present('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec').
narrative_ontology:cs_reading_relation('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', foundational, impunity_gap_requires_low_admissibility_bar).
narrative_ontology:cs_axiom_status(impunity_gap_requires_low_admissibility_bar, holdable).
narrative_ontology:cs_axiom_grounding('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', impunity_gap_requires_low_admissibility_bar, instrumental).
narrative_ontology:cs_axiom('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', foundational, sham_prosecution_defeats_sovereignty_deference).
narrative_ontology:cs_axiom_status(sham_prosecution_defeats_sovereignty_deference, holdable).
narrative_ontology:cs_axiom_grounding('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', sham_prosecution_defeats_sovereignty_deference, deontological).
narrative_ontology:cs_reference_frame('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', rome_statute_founding_compromise).
narrative_ontology:cs_drift_state('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', post_africa_bias_controversy_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7c6a7a2b-3153-4936-8b29-d75ac01fe0ec', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, atrocity_victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, icc_prosecutorial_office).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, targeted_state_elites_facing_sham_immunity).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, cooperating_states_bearing_referral_costs).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, domestic_judiciaries_delegitimized_by_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, affected_state_general_population).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, affected_state_general_population).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, no_impunity_for_mass_atrocity_doctrine).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, genuine_prosecution_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines whether domestic proceedings reflect genuine intent to prosecute or are shielding perpetrators; under this reading it construes 'unwilling or unable' broadly, opening admissibility whenever independence or genuineness is doubtful. It builds its institutional mandate and legitimacy on catching sham prosecutions and elite impunity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_prosecutorial_office, agenda_setter,
    institutional, generational, analytical, global).

% Live under a state whose own justice system is captured by, or complicit with, the perpetrators. Under a low admissibility threshold, they gain a pathway to prosecution that would otherwise never open; without ICC intervention their access to any accountability process is effectively zero.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, atrocity_victims_in_complicit_states, beneficiary,
    powerless, biographical, trapped, national).

% Document sham trials and lobby the ICC to find domestic proceedings inadequate. Their advocacy work is validated and resourced when the broad reading prevails; they benefit from an expansive admissibility doctrine that gives their evidence a forum.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, human_rights_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Have arranged or benefited from token domestic proceedings designed to shield themselves from real accountability. Under the broad reading, the ICC pierces this arrangement and asserts jurisdiction, exposing them to prosecution they believed the sovereignty shield had foreclosed.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, targeted_state_elites_facing_sham_immunity, payer,
    powerful, biographical, constrained, national).

% States that refer situations or cooperate with ICC investigations bear diplomatic, security, and resource costs — arrest cooperation, evidence-sharing, and exposure to retaliation — while gaining no direct benefit beyond the diffuse global good of enforced accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, cooperating_states_bearing_referral_costs, payer,
    institutional, generational, constrained, national).

% Even domestic courts that are not captured, but merely under-resourced, weak, or slow, can be found 'unable' under the broad reading and see their proceedings superseded. They bear the reputational cost of an international finding of inadequacy and have limited standing to contest that finding before the ICC itself.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, domestic_judiciaries_delegitimized_by_intervention, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, domestic_judiciaries_delegitimized_by_intervention, excluded).

% States outside the Rome Statute system, or with Security Council veto power, are structurally shielded from the same admissibility scrutiny applied to weaker or complicit states; they would be the loudest objectors to a symmetric application of the broad reading but are not bound by it and rarely appear before the Court.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, great_power_non_states_parties, excluded,
    powerful, civilizational, arbitrage, global).

% Benefits diffusely from deterrence of future atrocity and from a credible accountability record, but also bears the costs of international intervention — disrupted domestic political processes, sovereignty friction, and the risk that intervention is perceived as selective, undermining trust in the rule of law generally.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, affected_state_general_population, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, affected_state_general_population, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, diffuse).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a backstop accountability mechanism when the state that should prosecute atrocity crimes is unwilling (shielding perpetrators) or unable (institutionally collapsed) to do so genuinely — solving the collective-action problem of impunity that no domestic system captured by the perpetrators will solve on its own.
% TRANSFER_FUNCTION: Moves prosecutorial authority and the associated costs (investigation, cooperation, diplomatic exposure) from the territorial or national state to the ICC, and moves the risk of accountability from protected elites onto those elites, while imposing referral and cooperation costs on states that participate.
% ABSENT_VOICES: Great powers and their allies who are structurally outside the Rome Statute's practical reach are never subjected to the same scrutiny; targeted elites and their domestic courts have no meaningful forum to contest an ICC admissibility finding before it is made; weak-but-honest domestic judiciaries have no voice distinguishing them from genuinely captured ones.
% DISAPPEARANCE_RATIONALE: If the broad admissibility reading were replaced by strict deference to any domestic proceeding, sham trials engineered to block ICC jurisdiction would routinely succeed, victims in complicit or failed states would lose their only realistic accountability pathway, and the ICC's practical caseload against entrenched elites would collapse toward zero.
% FOUNDING_PROBLEM: After WWII and successive genocides where national courts were captured by or complicit with the perpetrators (or simply too devastated to function), there was no reliable mechanism to prosecute mass atrocity when the state itself was the obstacle.
% FOUNDING_PROBLEM_CORROBORATION: UN Commissions of Inquiry, independent fact-finding missions, and academic monitors of domestic 'accountability theater' trials (staged proceedings that acquit or lightly sanction protected figures) attest that sham domestic prosecutions remain a live and recurring phenomenon, corroborating the problem from outside the ICC's own institutional interest in expansive jurisdiction.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).
:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the mechanism genuinely redirects prosecutorial authority away from captured elites toward accountable process, which is coordination, not pure extraction — but it does impose real, asymmetric costs on cooperating states and on domestic judiciaries whose adequacy is contestable. Suppression is moderate (0.38) and has risen over the interval as the Office of the Prosecutor's admissibility jurisprudence hardened and state cooperation obligations under Part 9 of the Rome Statute intensified. Theater ratio (0.3) reflects genuine but incomplete institutional capacity — the ICC's caseload remains small relative to global atrocity incidence, so some of its accountability function is necessarily symbolic even under a sincere broad reading.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC and victim seats, the broad reading is the mechanism functioning exactly as intended — closing an impunity gap. From the targeted-elite and domestic-judiciary seats, the same admissibility finding looks like an externally imposed override of domestic sovereignty on a contestable factual predicate ('unwilling or unable'). The engine computes these divergent per-seat readings from the declared power/exit/scope data; this story does not average them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Atrocity victims and advocacy networks sit near the beneficiary end: they gain an accountability forum where none existed. Targeted elites and their engineered sham proceedings sit near the target end: the broad reading is designed specifically to pierce their immunity arrangement. Cooperating states and domestic judiciaries occupy an intermediate position — they are not the intended targets of the doctrine but bear its structural costs (referral burdens, delegitimization risk) as a side effect of the mechanism functioning as designed. Great powers outside effective jurisdiction are excluded rather than positioned on the beneficiary/victim axis at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (captured or destroyed domestic justice systems shielding perpetrators) remains empirically live, corroborated by independent fact-finding outside the ICC's own institutional interest — this blocks a mandatrophy finding under this reading. The tangled_rope classification (rather than pure rope) reflects that the coordination function is real AND asymmetric extraction is real simultaneously: the same admissibility mechanism that rescues victims from a captured domestic system also imposes costs on states and judiciaries whose 'inadequacy' finding may be contestable rather than clear-cut sham.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_pretextual_inadequacy_finding,
    'When the ICC finds a state ''unwilling or unable,'' is that finding reliably distinguishing genuinely captured/collapsed justice systems from merely under-resourced but sincere ones?',
    'Comparative case analysis of ICC admissibility rulings against independent judicial-capacity assessments (e.g. World Justice Project rule-of-law indices) to test whether findings track actual capture/collapse versus resource constraints or geopolitical exposure.',
    'If the broad reading systematically mislabels sincere-but-weak judiciaries as ''unable,'' the mechanism''s extraction on domestic judiciaries is higher than the coordination story credits; if the distinction holds reliably, the tangled_rope classification tilts closer to a genuine rope for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_pretextual_inadequacy_finding, empirical, 'Whether admissibility findings reliably separate captured/collapsed systems from merely weak ones.').

omega_variable(
    selective_enforcement_against_weak_states,
    'Does the broad reading''s practical application concentrate scrutiny on weaker, non-great-power states while structurally exempting powerful states and their allies from the same admissibility test?',
    'Statistical review of ICC referrals, preliminary examinations, and admissibility rulings by state power/alliance status over the full interval.',
    'If enforcement is concentrated on weak states, the mechanism''s coordination claim (uniform accountability standard) is undermined and its extraction functions more like selective victor''s justice in reverse — powerful states shielded, weak-state elites exposed — which would push the classification toward snare for the excluded great-power seat and sharpen the tangled_rope reading elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_against_weak_states, empirical, 'Whether the broad admissibility standard is applied asymmetrically by state power.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the Rome Statute text itself sufficiently determinate to settle whether ''unwilling or unable'' should be read broadly (this reading) or narrowly (national_primacy_reading), or is the choice between readings itself a policy/political decision dressed as interpretation?',
    'Analysis of the travaux préparatoires and subsequent Pre-Trial Chamber jurisprudence for internal consistency; comparison of drafting-history evidence cited by proponents of each reading.',
    'If the text is genuinely indeterminate, both readings are equally ''textually available'' and the operative reading is chosen by institutional and political actors rather than derived — which reframes this story''s classification as one contested political choice among available framings rather than the uniquely correct reading of a determinate text.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the broad/narrow reading split reflects textual indeterminacy or a disguised policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1998, article_17_complementarity__international_oversight_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.22).
narrative_ontology:measurement(arti_tr_t2008, article_17_complementarity__international_oversight_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(arti_tr_t2014, article_17_complementarity__international_oversight_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement(arti_tr_t2019, article_17_complementarity__international_oversight_reading, theater_ratio, 2019, 0.29).
narrative_ontology:measurement(arti_tr_t2024, article_17_complementarity__international_oversight_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(arti_be_t1998, article_17_complementarity__international_oversight_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.31).
narrative_ontology:measurement(arti_be_t2008, article_17_complementarity__international_oversight_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(arti_be_t2014, article_17_complementarity__international_oversight_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement(arti_be_t2019, article_17_complementarity__international_oversight_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement(arti_be_t2024, article_17_complementarity__international_oversight_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1998, article_17_complementarity__international_oversight_reading, suppression_requirement, 1998, 0.22).
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.26).
narrative_ontology:measurement(arti_su_t2008, article_17_complementarity__international_oversight_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(arti_su_t2014, article_17_complementarity__international_oversight_reading, suppression_requirement, 2014, 0.33).
narrative_ontology:measurement(arti_su_t2019, article_17_complementarity__international_oversight_reading, suppression_requirement, 2019, 0.36).
narrative_ontology:measurement(arti_su_t2024, article_17_complementarity__international_oversight_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, national_primacy_reading).

% DUAL FORMULATION NOTE:
% This story and national_primacy_reading decompose the natural-language concept 'Article 17 complementarity' into two ε-invariant constraints sharing one treaty-text kernel. This reading (international_oversight) authors ε=0.42 with a tangled_rope claim, low admissibility threshold, and a victim set including targeted elites and cooperating states. The sibling (national_primacy) authors its own ε and stakeholder set reflecting a high admissibility bar, presumptive deference to domestic courts, and a victim set weighted toward states subjected to ICC overreach. Both are linked bidirectionally via affects_constraints because a shift in ICC jurisprudence toward one reading directly degrades the practical availability of the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
