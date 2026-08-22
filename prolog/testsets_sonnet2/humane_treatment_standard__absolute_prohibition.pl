% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 — Absolute Prohibition Reading
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This story instantiates the absolute-prohibition reading of the Common
 *   Article 3 kernel: no circumstance — not battlefield necessity, not
 *   classification of the detainee as an 'unlawful combatant,' not an
 *   imminent-threat claim — permits crossing the threshold into torture or
 *   degrading treatment. This is structurally distinct from the
 *   contextual_necessity reading (which permits a security override) and the
 *   proportionality_balancing reading (which weighs dignity against security
 *   case-by-case); those are separate constraints with their own ε values,
 *   not alternative measurements of this one. Under this reading the standard
 *   functions overwhelmingly as coordination: it protects a genuinely
 *   vulnerable, powerless class (detained persons) against a powerful,
 *   institutionally organized actor (the detaining state) whose exit from the
 *   norm is deliberately foreclosed. Extraction is low because the norm does
 *   not transfer value FROM the protected class TO another party — if
 *   anything it constrains the powerful party's discretion for the protected
 *   class's benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.15).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.2).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.15).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 — Absolute Prohibition Reading").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, 'ca61f6c4-1131-45b6-82ff-855c8552307e').
narrative_ontology:cs_kernel_codification('ca61f6c4-1131-45b6-82ff-855c8552307e', fixed_text).
narrative_ontology:cs_authority_grounding('ca61f6c4-1131-45b6-82ff-855c8552307e', lineage).
narrative_ontology:cs_interpretation_layer_present('ca61f6c4-1131-45b6-82ff-855c8552307e').
narrative_ontology:cs_reading_relation('ca61f6c4-1131-45b6-82ff-855c8552307e', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('ca61f6c4-1131-45b6-82ff-855c8552307e', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('ca61f6c4-1131-45b6-82ff-855c8552307e', foundational, non_derogability_admits_no_exception).
narrative_ontology:cs_axiom_status(non_derogability_admits_no_exception, holdable).
narrative_ontology:cs_axiom_grounding('ca61f6c4-1131-45b6-82ff-855c8552307e', non_derogability_admits_no_exception, deontological).
narrative_ontology:cs_axiom('ca61f6c4-1131-45b6-82ff-855c8552307e', foundational, detainee_status_irrelevant_to_floor).
narrative_ontology:cs_axiom_status(detainee_status_irrelevant_to_floor, holdable).
narrative_ontology:cs_axiom_grounding('ca61f6c4-1131-45b6-82ff-855c8552307e', detainee_status_irrelevant_to_floor, deontological).
narrative_ontology:cs_reference_frame('ca61f6c4-1131-45b6-82ff-855c8552307e', post_wwii_non_derogable_floor).
narrative_ontology:cs_drift_state('ca61f6c4-1131-45b6-82ff-855c8552307e', post_9_11_security_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ca61f6c4-1131-45b6-82ff-855c8552307e', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detained_persons).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, captured_combatants).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, civilian_internees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, detaining_states_militaries).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, interrogators_and_detention_personnel).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_derogability_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, jus_cogens_prohibition_of_torture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held in custody by a detaining power, entirely dependent on that power's compliance for physical safety. Under this reading, they enter the full rights-holder set: no circumstance — interrogation urgency, alleged threat level, or classification dispute — can lower the floor. They have no capacity to enforce the standard themselves; enforcement depends entirely on external actors and the detaining power's own restraint.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detained_persons, beneficiary,
    powerless, immediate, trapped, global).

% Runs detention and interrogation operations and is bound by the absolute floor regardless of intelligence value it believes is at stake. Under this reading it cannot invoke a security exception; it bears the cost of that constraint whenever it believes an imminent-threat scenario would justify crossing the threshold. It sets day-to-day interrogation policy but cannot set it below the floor without violating the norm.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detaining_states_militaries, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, detaining_states_militaries, payer).

% Individuals executing detention policy who are bound personally by the absolute standard and can face individual criminal liability (command responsibility, war crimes prosecution) for crossing it, even under orders. They cannot exit the obligation by citing superior orders or perceived necessity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, interrogators_and_detention_personnel, payer,
    moderate, immediate, constrained, local).

% ICRC, UN human rights mechanisms, and NGOs monitor compliance, document violations, and press for accountability. They have no enforcement power themselves but generate the record that international and domestic courts can act on.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_monitoring_bodies, observer,
    organized, generational, analytical, global).

% Intelligence and security officials who believe enhanced interrogation techniques would yield actionable threat information are structurally excluded from having their necessity claims recognized under this reading — the absolute-prohibition framework treats their argument as categorically inadmissible rather than weighing it, no matter how the situation is framed.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, national_security_apparatus, excluded,
    powerful, biographical, constrained, national).

% Courts (ICTY, ICTR, ICC, domestic universal-jurisdiction prosecutions) apply the absolute standard in adjudicating command responsibility and individual criminal liability, treating the prohibition as non-derogable and immune to necessity or tu-quoque defenses.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, detained_persons).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, non-negotiable floor of treatment for anyone held in armed-conflict-related detention, so that no party to a conflict can justify degrading or torturous treatment by appeal to reciprocity, military necessity, or the detainee's classification — solving the collective-action problem of escalating mistreatment that would otherwise follow from each side calibrating treatment to what it believes the other side deserves or requires.
% TRANSFER_FUNCTION: Moves the power to define the threshold of acceptable treatment away from the detaining power's situational judgment and vests it in a fixed, universal minimum; the detaining power surrenders discretion it might otherwise exercise in exchange for a norm that also binds any power that later holds its own personnel.
% ABSENT_VOICES: National security officials and interrogation-policy advocates who believe some circumstances warrant enhanced techniques are excluded from the interpretive room under this reading — their necessity arguments are treated as inadmissible rather than considered and rejected on the merits, which is precisely what the two sibling readings (contextual_necessity, proportionality_balancing) exist to contest.
% DISAPPEARANCE_RATIONALE: If the absolute-prohibition reading collapsed, detaining powers would gain a defensible basis to escalate interrogation methods under claimed necessity; detainee treatment would become contingent on situational threat assessments rather than fixed; and the entire post-WWII architecture of non-derogable jus cogens norms would lose its clearest anchor point, with cascading effects on war crimes prosecutions and reciprocal treatment norms.
% FOUNDING_PROBLEM: The unregulated brutalization of detainees, hostages, and non-combatants in internal and international armed conflict — a problem made vivid by WWII-era atrocities and the recognized failure of reciprocity-based or discretionary standards to prevent escalating cruelty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by international criminal tribunals (ICTY/ICTR jurisprudence treating CA3 as customary international law binding even non-signatories), by state military legal advisors who continue to train forces on the absolute standard despite policy pressure to relax it, and by documented recidivism in conflicts where the standard was suspended (e.g., post-9/11 detention practices), which state investigators and legislative inquiries (outside the human-rights-advocacy community) themselves later characterized as harmful and counterproductive to intelligence goals.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.15, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).
:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.15) and rising only slightly over the interval, reflecting the modest and largely rhetorical costs incurred as states test the boundary (torture-memo-style reinterpretation attempts, 'enhanced interrogation' euphemism campaigns) without ever succeeding in relocating the ε of THIS reading — those attempts, when they succeed politically, instantiate the contextual_necessity reading rather than eroding this one. Accessibility collapse is high (0.8): once understood, the absolute prohibition leaves essentially no legally defensible alternative for a detaining power operating within this reading's framework. Resistance is moderate (0.55) because national-security actors persistently push back against the absoluteness of the floor, generating real friction (legal challenges, policy memos arguing exceptions, political pressure) even though the norm itself does not bend.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons are the clear structural beneficiaries: the constraint exists to protect them and imposes no cost on them. Detaining states and their personnel are payers in the sense that the absolute floor forecloses interrogation options they might otherwise prefer to use; their exit options are 'constrained' rather than 'trapped' because states can and do violate the norm (at legal and reputational cost) but cannot exit it as a matter of law. No victim group is declared because the coordination function dominates — this reading contains no group from whom the norm extracts value for another's benefit; it is asymmetric in the beneficiary's favor by design, which is the whole point of a non-derogable floor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unchecked wartime brutalization) remains live, corroborated by continuing violations and by tribunals still actively litigating command responsibility on this exact standard — this is not a vestigial rule performing symbolic function after its problem disappeared. The classification as rope (not tangled_rope) follows from the absence of a genuine victim class within this reading's own terms: security officials who cannot torture detainees are not 'extracted from' by the norm in the technical sense the framework tracks, they are simply denied a tool they wanted, which the coordination function itself requires be denied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_displacement_vs_erosion,
    'When a state adopts ''enhanced interrogation'' policy, has it exited the absolute_prohibition reading entirely (instantiating contextual_necessity instead), or has it eroded the absolute reading from within while nominally still claiming to hold it?',
    'Examine the state''s own legal justification: if it argues the technique does not meet the torture/degrading-treatment threshold (redefinition), that is erosion within this reading; if it argues necessity overrides the threshold once met, that is a jump to the contextual_necessity reading.',
    'If states systematically redefine rather than override, the absolute_prohibition reading''s ε should rise over time as redefinition tactics accumulate — this is the committer-structure question the two sibling stories exist to keep separate rather than blending into one hedged ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_displacement_vs_erosion, conceptual, 'Whether interrogation-policy departures instantiate a sibling reading or erode this one from within.').

omega_variable(
    jus_cogens_universality_contested,
    'Is the non-derogability of the CA3 floor a genuine feature of customary international law binding all states regardless of consent (a mountain-like universal), or is it a constructed norm whose apparent universality depends on enforcement asymmetry (powerful states rarely prosecuted, weak states more exposed)?',
    'Comparative analysis of prosecution patterns under universal jurisdiction and ICC referrals: if enforcement falls disproportionately on weaker states while powerful states'' violations go unprosecuted, the apparent universality is partly an artifact of power rather than pure legal structure.',
    'If enforcement asymmetry is substantial, this reading''s classification could shift toward tangled_rope at the enforcement layer even while the norm itself remains coordination-structured — a divergence between the norm''s design and its application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jus_cogens_universality_contested, empirical, 'Whether the absolute prohibition''s apparent universality reflects genuine jus cogens status or enforcement-power asymmetry.').

omega_variable(
    detainee_agency_absence,
    'Does the complete absence of a victim group in this reading obscure a genuine but diffuse cost borne by the detaining state''s broader population (e.g., forgone intelligence, security risk), making the true structure closer to tangled_rope than rope?',
    'Empirical intelligence-community assessment of whether absolute compliance with CA3 measurably reduces actionable intelligence yield compared to enhanced techniques — the disputed empirical claim underlying the contextual_necessity reading''s justification.',
    'If forgone security value is real and substantial, a case exists for reading the general public as an unnamed payer group, which would push this reading toward tangled_rope; if the security-value claim is empirically unsupported (as most declassified assessments suggest), rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(detainee_agency_absence, empirical, 'Whether the general public bears a diffuse security cost from absolute compliance that would qualify as an additional victim class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.05).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__absolute_prohibition, theater_ratio, 8, 0.06).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__absolute_prohibition, theater_ratio, 16, 0.07).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__absolute_prohibition, theater_ratio, 24, 0.08).
narrative_ontology:measurement(huma_tr_t32, humane_treatment_standard__absolute_prohibition, theater_ratio, 32, 0.09).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__absolute_prohibition, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__absolute_prohibition, base_extractiveness, 8, 0.09).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__absolute_prohibition, base_extractiveness, 16, 0.11).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__absolute_prohibition, base_extractiveness, 24, 0.13).
narrative_ontology:measurement(huma_be_t32, humane_treatment_standard__absolute_prohibition, base_extractiveness, 32, 0.14).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__absolute_prohibition, base_extractiveness, 40, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__absolute_prohibition, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__absolute_prohibition, suppression_requirement, 8, 0.14).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__absolute_prohibition, suppression_requirement, 16, 0.16).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__absolute_prohibition, suppression_requirement, 24, 0.18).
narrative_ontology:measurement(huma_su_t32, humane_treatment_standard__absolute_prohibition, suppression_requirement, 32, 0.19).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__absolute_prohibition, suppression_requirement, 40, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial label 'Common Article 3 standard' into structurally distinct constraints per the ε-invariance principle. absolute_prohibition (this story) authors low ε (~0.15) because it models the norm functioning as pure protective coordination with no permitted override. contextual_necessity and proportionality_balancing are separate constraints with their own stakeholder sets, victim declarations, and higher ε — they model regimes where the security exception or balancing test is actually operative, which structurally changes who bears extraction and how much. The three are linked bidirectionally via affects_constraints because each reading's dominance in practice shapes political and legal pressure on the others (e.g., successful contextual_necessity arguments in one jurisdiction increase repudiation pressure on this reading's non-derogability claim elsewhere).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
