% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Hybrid Proportionality Reading of Geneva Protective Scope (Conflict-Type Tiering)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This story instantiates the hybrid_proportionality_reading of the
 *   geneva_conventions_protective_scope kernel: the position that Geneva
 *   protections properly scale by conflict classification (AP I for
 *   international armed conflict; AP II/Common Article 3 for
 *   non-international armed conflict), with proportionality analysis
 *   governing application within each tier. This is a distinct constraint
 *   from the sibling readings — state_centric_reading (protection keyed
 *   strictly to Article 4 combatant status) and universal_rights_reading (a
 *   single protective floor regardless of classification or status) — each of
 *   which is authored as its own story with its own ε, victim set, and
 *   classification. The 2001 inflection point in the temporal record reflects
 *   the post-9/11 proliferation of conflicts straddling
 *   international/non-international lines (transnational non-state actors,
 *   internationalized internal conflicts), which sharply increased the
 *   practical stakes of classification disputes and the doctrinal literature
 *   devoted to resolving them.
 *
 * KEY AGENTS:
 *   - state_militaries_with_classification_leverage: agenda_setter/beneficiary (institutional/arbitrage) — controls classification and reaps interpretive latitude
 *   - legal_advisors_and_targeting_lawyers: beneficiary (organized/mobile) — professional practice built on doctrinal indeterminacy
 *   - dominant_party_in_asymmetric_conflict: beneficiary/agenda_setter (powerful/arbitrage) — uses classification ambiguity to minimize exposure
 *   - civilians_in_ambiguously_classified_conflicts: payer (powerless/trapped) — protection depends on a classification they cannot contest
 *   - non_state_armed_group_detainees: payer (powerless/trapped) — treatment regime set unilaterally by the detaining power
 *   - weaker_party_combatants: payer (moderate/constrained) — bears proportionality judgments made without reciprocal disclosure
 *   - humanitarian_organizations_seeking_access: payer/excluded (organized/constrained) — access blocked pending classification disputes it cannot resolve
 *   - international_courts_and_tribunals: observer (institutional/analytical) — adjudicates years after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.52).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Hybrid Proportionality Reading of Geneva Protective Scope (Conflict-Type Tiering)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '6f394e8a-beb0-43ea-986a-acf87559e253').
narrative_ontology:cs_kernel_codification('6f394e8a-beb0-43ea-986a-acf87559e253', fixed_text).
narrative_ontology:cs_authority_grounding('6f394e8a-beb0-43ea-986a-acf87559e253', extraction).
narrative_ontology:cs_interpretation_layer_present('6f394e8a-beb0-43ea-986a-acf87559e253').
narrative_ontology:cs_reading_relation('6f394e8a-beb0-43ea-986a-acf87559e253', geneva_conventions_protective_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('6f394e8a-beb0-43ea-986a-acf87559e253', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('6f394e8a-beb0-43ea-986a-acf87559e253', foundational, protective_obligation_scales_with_conflict_classification).
narrative_ontology:cs_axiom_status(protective_obligation_scales_with_conflict_classification, holdable).
narrative_ontology:cs_axiom_grounding('6f394e8a-beb0-43ea-986a-acf87559e253', protective_obligation_scales_with_conflict_classification, conventional).
narrative_ontology:cs_axiom('6f394e8a-beb0-43ea-986a-acf87559e253', foundational, proportionality_analysis_is_the_governing_test_within_each_tier).
narrative_ontology:cs_axiom_status(proportionality_analysis_is_the_governing_test_within_each_tier, holdable).
narrative_ontology:cs_axiom_grounding('6f394e8a-beb0-43ea-986a-acf87559e253', proportionality_analysis_is_the_governing_test_within_each_tier, instrumental).
narrative_ontology:cs_reference_frame('6f394e8a-beb0-43ea-986a-acf87559e253', differentiated_regime_by_conflict_intensity).
narrative_ontology:cs_drift_state('6f394e8a-beb0-43ea-986a-acf87559e253', post_9_11_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f394e8a-beb0-43ea-986a-acf87559e253', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_classification_leverage).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, legal_advisors_and_targeting_lawyers).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, dominant_party_in_asymmetric_conflict).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_ambiguously_classified_conflicts).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_detainees).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_party_combatants).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, humanitarian_organizations_seeking_access).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, conflict_classification_as_legal_prerequisite).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, proportionality_as_governing_calculus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the initial characterization of a conflict as international or non-international, and thereby which protective regime (AP I's fuller protections vs. AP II/Common Article 3's thinner floor) applies. Retains legal advisors who argue classification and proportionality in whichever direction serves operational objectives. Can reclassify or contest classification as the conflict evolves, and rarely bears the cost of a wrong call.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_classification_leverage, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_classification_leverage, beneficiary).

% Builds a professional practice on the interpretive complexity of conflict classification and proportionality analysis. The ambiguity of the hybrid tiering system is the raw material of the career; clearer rules would reduce demand for this expertise. Moves between military, government, and academic postings depending on where classification disputes are most active.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, legal_advisors_and_targeting_lawyers, beneficiary,
    organized, biographical, mobile, global).

% Faces a weaker, often non-state adversary and argues for whichever conflict classification minimizes its own legal exposure while maximizing latitude for lethal action against the opposing force. Uses the doctrinal indeterminacy of the hybrid framework to justify targeting decisions after the fact through proportionality argument.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, dominant_party_in_asymmetric_conflict, beneficiary,
    powerful, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, dominant_party_in_asymmetric_conflict, agenda_setter).

% Live inside a conflict zone whose legal characterization determines what protection they are formally owed, but they have no voice in that characterization and often no reliable way to learn which regime a court or occupying force will later say applied. Casualties are frequently justified retroactively through proportionality calculus performed by the party that caused them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_ambiguously_classified_conflicts, payer,
    powerless, immediate, trapped, local).

% Captured members of non-state groups whose treatment depends entirely on whether the conflict is classified as international (triggering AP I combatant/POW-adjacent protections) or non-international (leaving only Common Article 3's minimal floor). The classification is typically made by the detaining power, creating a structural conflict of interest between captor and classifier.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_detainees, payer,
    powerless, biographical, trapped, regional).

% Fighters for non-state or under-resourced parties who cannot contest classification determinations through the same legal and diplomatic channels available to states. Bear the consequences of proportionality judgments made by an adversary with superior legal resources and no obligation to disclose its reasoning in real time.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_party_combatants, payer,
    moderate, biographical, constrained, regional).

% Needs a settled classification to know which access rights and protective mandates (ICRC visitation rights under AP I versus the thinner Common Article 3 framework) it can invoke. Classification disputes between parties routinely stall or block humanitarian access while the legal question remains unresolved, and the organizations have no authority to settle it themselves.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, humanitarian_organizations_seeking_access, payer,
    organized, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, humanitarian_organizations_seeking_access, excluded).

% Adjudicates classification and proportionality disputes after the fact, often years after the conduct in question, drawing on the same indeterminate doctrinal framework the parties exploited in real time. Its rulings shape future practice but cannot retroactively protect those already harmed.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, dominant_party_in_asymmetric_conflict).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated legal framework that lets states apply differentiated humanitarian obligations depending on the nature and intensity of a conflict, avoiding a one-size-fits-all regime that might be either unworkable in low-intensity internal conflicts or under-protective in interstate war.
% TRANSFER_FUNCTION: Moves interpretive discretion and legal risk toward the party controlling classification and targeting decisions (typically the stronger, often state, party) and moves protective certainty away from civilians, detainees, and weaker combatants who cannot contest the classification in real time.
% ABSENT_VOICES: Civilians and non-state detainees affected by classification decisions have no seat in the classification process itself; humanitarian organizations that would advocate for protective clarity are consulted only informally and cannot bind the classifying party's determination.
% DISAPPEARANCE_RATIONALE: States and their legal advisors would say the tiered structure is doctrinally necessary and its disappearance would collapse a coherent body of law built over decades; civilians, detainees, and humanitarian actors would say a unified protective floor (closer to the universal_rights_reading) would change little in principle but would remove the classification battle that currently determines, case by case, whether meaningful protection applies at all.
% FOUNDING_PROBLEM: The original Geneva framework needed a way to extend humanitarian protection to interstate war (AP I) while separately addressing internal conflicts (Common Article 3, later AP II) without requiring states to accept full combatant-status obligations toward domestic insurgents, which many states would not ratify.
% FOUNDING_PROBLEM_CORROBORATION: States and military legal establishments attest the tiered structure remains necessary to secure ratification and operational buy-in. ICRC commentary, UN human rights bodies, and independent IHL scholars outside any belligerent's chain of command attest that the classification threshold has become a primary vector for evading protective obligations rather than a neutral technical distinction, particularly in conflicts with mixed international/non-international characteristics (internationalized internal conflicts).
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the tiered structure genuinely coordinates a real problem (states would not have ratified a single undifferentiated regime binding them fully in internal conflicts) but that the classification threshold has become a primary lever for a stronger party to narrow its own obligations relative to a weaker one. Suppression (0.52) captures that weaker parties and civilians have essentially no procedural mechanism to force a classification determination or contest an adverse one in real time — the suppression is structural (no forum, no standing) rather than coercive in the direct sense. Theater ratio (0.44) and its rising trajectory reflect the growing volume of proportionality-analysis documentation, legal review processes, and post-hoc justification produced without a commensurate rise in verified protective outcomes for civilians in contested-classification conflicts — much of the apparatus has become argument-generation rather than protection-delivery. Accessibility collapse is moderate (0.4): alternative framings (state-centric, universal-rights) remain doctrinally live and contested, so alternatives have not fully foreclosed, unlike a true mountain. Resistance (0.62) is substantial: ICRC, human rights bodies, and IHL scholars actively contest the extractive use of classification ambiguity, which is precisely what sustains the sibling universal_rights_reading as a competing position.
 *
 * DIRECTIONALITY LOGIC:
 *   States with the institutional capacity to characterize and re-characterize conflicts, and the dominant party in any asymmetric conflict, sit near the beneficiary end: the ambiguity of the hybrid tiering system is a resource they can deploy, and their exit options (arbitrage — choosing among interpretive positions as convenient) reflect this. Civilians, detainees, and weaker-party combatants sit near the target end: they cannot classify the conflict themselves, cannot contest an adverse classification through any binding forum in real time, and bear the practical consequences of both the classification decision and the proportionality analysis performed unilaterally by the more powerful party. Legal advisors are a secondary beneficiary class whose professional interest is structurally aligned with maintaining rather than resolving the ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling universal humanitarian aspiration with the practical reality that states would not ratify a regime treating domestic insurgency identically to interstate war — was live in 1977 and remains partially live today (states still resist full AP I obligations toward non-state actors). But the corroboration record shows the founding problem's currency is now genuinely contested: ICRC and independent scholarship argue the classification threshold has drifted from a technical accommodation into a strategic instrument, while state legal establishments continue to defend it as doctrinally essential. This is exactly the tangled_rope signature — a real coordination function persists (state ratification of a workable, differentiated regime) alongside asymmetric extraction (classification discretion concentrated in the hands of the party best positioned to exploit it) — rather than either a pure Rope (no victims) or a pure Snare (no coordination function at all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_authority_neutrality,
    'Is the party that determines a conflict''s classification (typically the state or dominant party involved in the conflict) structurally capable of neutral classification, or does the self-interested position of the classifier make the classification determination itself a form of extraction?',
    'Comparative analysis of classification determinations made by an interested party versus determinations later made or reviewed by an international tribunal or independent fact-finding mission in the same conflict; a systematic divergence favoring the interested party''s self-interest would indicate the determination function is captured rather than neutral.',
    'If classification is structurally non-neutral, the extractiveness of this reading is understated by the current metrics, since the harm occurs upstream of the proportionality analysis, at the classification gate itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_authority_neutrality, empirical, 'Whether the classifying party can be structurally neutral or is inherently self-interested.').

omega_variable(
    proportionality_calculus_verifiability,
    'Can a proportionality determination made by a party to the conflict be meaningfully verified by an outside actor before harm occurs, or only reconstructed after the fact from incomplete information the acting party controls?',
    'Track record of contemporaneous versus post-hoc proportionality review across a sample of contested strikes or operations; if verification is reliably only possible post-hoc and using information the acting party selectively discloses, the calculus functions as a justification mechanism rather than a genuine constraint.',
    'If proportionality is effectively unverifiable ex ante, the theater_ratio for this reading is likely understated, since much of the apparatus produces after-the-fact argument rather than binding real-time restraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calculus_verifiability, empirical, 'Whether proportionality analysis constrains conduct in advance or only justifies it afterward.').

omega_variable(
    kernel_reading_selection_bias,
    'Given that this constraint documents the hybrid_proportionality_reading specifically, is the choice to treat conflict-type tiering (rather than combatant-status gating or a universal floor) as the operative reading itself a product of which parties currently dominate IHL institutional practice?',
    'Historical analysis of state ratification patterns, ICRC commentary evolution, and tribunal jurisprudence to determine whether the hybrid reading''s dominance in contemporary practice reflects genuine doctrinal consensus or reflects the interpretive preferences of states with the most influence over treaty negotiation and customary law formation.',
    'If the hybrid reading''s prevalence is substantially a function of state influence over IHL institutions rather than doctrinal necessity, this reinforces the beneficiary structure documented here (dominant states shaping the framework that then benefits them) and would strengthen the case that the state_centric_reading and this reading share a common beneficiary bias not present in the universal_rights_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether the prevalence of the hybrid reading itself reflects state institutional dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.2).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(gene_tr_t2018, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2018, 0.43).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(gene_be_t2018, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2018, 0.57).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.3).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2001, 0.44).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2010, 0.49).
narrative_ontology:measurement(gene_su_t2018, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2018, 0.51).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).

% DUAL FORMULATION NOTE:
% Three sibling readings of the geneva_conventions_protective_scope kernel: this story (hybrid_proportionality_reading), geneva_conventions_protective_scope__state_centric_reading, and geneva_conventions_protective_scope__universal_rights_reading. Each reading produces a different victim set and a different ε: the state-centric reading concentrates victimhood on unprivileged belligerents excluded from protection entirely; the universal-rights reading has the lowest ε because it removes the classification gate as a site of extraction; this hybrid reading sits between them, with ε driven by the exploitability of the classification threshold itself. The readings are linked here for contamination-propagation analysis — a tribunal ruling that narrows classification discretion under this reading would exert downstream pressure on the state-centric reading's legitimacy while reinforcing the universal-rights reading's practical case.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
