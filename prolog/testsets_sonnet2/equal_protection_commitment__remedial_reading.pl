% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection as Anti-Subordination: Remedial Race-Consciousness Reading
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the remedial (anti-subordination) reading of the
 *   Equal Protection Clause: the guarantee is read as forbidding the
 *   perpetuation of a racial caste system and, correspondingly, as permitting
 *   race-conscious state action narrowly tailored to dismantle the effects of
 *   that subordination. This is one of three structurally distinct
 *   constitutional claims sharing the label 'equal protection' — the
 *   colorblind reading (any racial classification is forbidden) and the
 *   diversity reading (race is permitted only as one factor serving
 *   compelling educational-diversity interests) are separate constraints with
 *   their own ε values and stakeholder structures, linked here via
 *   network.affects_constraints. Under this reading specifically, the
 *   beneficiary/victim structure inverts relative to the colorblind reading:
 *   state actors implementing remedial programs and historically subordinated
 *   groups sit in the beneficiary position, while individuals displaced by
 *   preferential allocation enter the victim position — a structural delta
 *   that does not appear, or appears inverted, under the sibling readings.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: beneficiary of remedial access, generational time horizon, constrained exit from underlying disparity
 *   - remedial_program_administrators: institutional agenda-setters designing and defending race-conscious criteria
 *   - displaced_nonminority_applicants: payers bearing concentrated individual cost for diffuse historical repair
 *   - judiciary: institutional observer applying strict scrutiny to tailoring
 *   - civil_rights_advocacy_organizations: organized but frequently excluded from direct party status, advancing the anti-subordination theory as amici
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.44).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection as Anti-Subordination: Remedial Race-Consciousness Reading").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, 'cd4f0704-2b27-4862-98cb-34e93ccd5df6').
narrative_ontology:cs_kernel_codification('cd4f0704-2b27-4862-98cb-34e93ccd5df6', distributed).
narrative_ontology:cs_authority_grounding('cd4f0704-2b27-4862-98cb-34e93ccd5df6', distributed).
narrative_ontology:cs_reading_relation('cd4f0704-2b27-4862-98cb-34e93ccd5df6', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd4f0704-2b27-4862-98cb-34e93ccd5df6', equal_protection_commitment__diversity_reading, influences).
narrative_ontology:cs_axiom('cd4f0704-2b27-4862-98cb-34e93ccd5df6', foundational, equal_protection_targets_caste_not_classification).
narrative_ontology:cs_axiom_status(equal_protection_targets_caste_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('cd4f0704-2b27-4862-98cb-34e93ccd5df6', equal_protection_targets_caste_not_classification, deontological).
narrative_ontology:cs_axiom('cd4f0704-2b27-4862-98cb-34e93ccd5df6', secondary, narrow_tailoring_to_identified_subordination_permits_race_conscious_remedy).
narrative_ontology:cs_axiom_status(narrow_tailoring_to_identified_subordination_permits_race_conscious_remedy, holdable).
narrative_ontology:cs_axiom_grounding('cd4f0704-2b27-4862-98cb-34e93ccd5df6', narrow_tailoring_to_identified_subordination_permits_race_conscious_remedy, instrumental).
narrative_ontology:cs_reference_frame('cd4f0704-2b27-4862-98cb-34e93ccd5df6', reconstruction_era_anti_caste_purpose).
narrative_ontology:cs_drift_state('cd4f0704-2b27-4862-98cb-34e93ccd5df6', post_students_for_fair_admissions_2023, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cd4f0704-2b27-4862-98cb-34e93ccd5df6', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, remedial_program_administrators).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, displaced_nonminority_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups subjected to historical de jure and de facto caste-like subordination (segregation, exclusion, discriminatory allocation of opportunity) gain access to remedial set-asides, admissions preferences, or contracting programs designed to dismantle the residual effects of that subordination. Their exit from the underlying condition depends on the persistence of the remedial program; they cannot individually undo generational disparities in wealth, education access, or network capital by exiting the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    moderate, generational, constrained, national).

% State agencies, university admissions offices, and public contracting bodies design and enforce race-conscious remedial criteria, justifying them as narrowly tailored responses to identified caste-perpetuating patterns. They administer the classification, defend it in litigation, and bear reputational and legal risk if courts find the tailoring insufficiently precise.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, remedial_program_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Applicants who would have received the seat, contract, or position under a race-neutral allocation but are displaced by the remedial preference. They bear a concentrated, identifiable individual cost for what the arrangement treats as a diffuse historical debt; their recourse is litigation challenging the program's tailoring, which is slow, costly, and outcome-uncertain.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, displaced_nonminority_applicants, payer,
    moderate, biographical, constrained, national).

% Courts applying strict scrutiny evaluate whether a given remedial classification is narrowly tailored to a compelling interest in dismantling identified subordination, rather than a diffuse societal-discrimination rationale. Their doctrine determines how far this reading can be operationalized without collapsing into either the colorblind or diversity readings.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, judiciary, observer,
    institutional, civilizational, analytical, national).

% Advocate for the anti-subordination reading in litigation and legislative testimony, arguing that formal colorblindness perpetuates caste effects under a neutral label. They are frequently amici rather than parties, and their structural argument — that equal protection's purpose was abolishing a racial caste system, not merely regulating classifications — is often treated by courts as background theory rather than controlling doctrine.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a targeted, time-bounded reallocation of opportunity toward groups whose current disadvantage is causally traceable to state-sanctioned subordination, on the theory that formal neutrality applied to a starting position produced by that subordination simply launders the prior injury forward.
% TRANSFER_FUNCTION: Moves admission seats, contracts, and positions from applicants who would prevail under race-neutral criteria to applicants from groups the remedial program identifies as subordination-affected; the transfer is justified as repair rather than ordinary redistribution.
% ABSENT_VOICES: Individual displaced applicants rarely have organized representation comparable to the institutional and advocacy apparatus defending remedial programs; conversely, when programs are challenged, the historically subordinated beneficiaries are often represented only by the defending institution's litigation strategy, not by independent counsel voicing their stake directly.
% DISAPPEARANCE_RATIONALE: If this reading vanished, race-conscious remedial admissions and contracting programs would lose their doctrinal foundation; institutions would either abandon such programs (reverting to race-neutral allocation, changing who gets seats and contracts) or seek shelter under the narrower diversity rationale where available. Displaced applicants would gain uncontested seats; subordination-affected applicants would lose an access channel with no automatic replacement.
% FOUNDING_PROBLEM: Formal legal equality (colorblind rules) applied atop a society freshly emerged from centuries of state-enforced racial caste produced continued subordination in fact, because neutral rules operating on unequal starting positions reproduce the inequality rather than correcting it.
% FOUNDING_PROBLEM_CORROBORATION: Sociological and economic disparity data (wealth gap studies, school segregation persistence research) from outside both the advocacy organizations and the administering institutions corroborate that measurable subordination effects persist; however, colorblind-reading proponents dispute whether persistent disparity is properly attributed to ongoing caste dynamics versus other causes, and dispute whether race-conscious remedy is the correct institutional response even if the underlying disparity is real.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 — moderate-high, consistent with the expected 0.45-0.60 band — because the reading's own coordination logic (repairing caste effects) coexists with a genuine, individually-borne cost imposed on displaced applicants through the same mechanism that delivers the remedy. Suppression (0.44) reflects the active judicial and administrative apparatus required to sustain narrow tailoring against colorblind-reading challenges — this is not passive coordination but a contested doctrine requiring continuous defense in litigation. Theater ratio is low-moderate (0.22) because the remedial function is largely genuine rather than performative, though some administrative diversity-statement compliance work has drifted toward proxy documentation over time. Resistance is high (0.68) because the reading faces sustained, organized doctrinal challenge (colorblind-reading litigants, shifting judicial composition) rather than passive acceptance.
 *
 * PERSPECTIVAL GAP:
 *   From the remedial-program-administrator seat, the arrangement is corrective coordination solving a real, empirically documented subordination problem. From the displaced-nonminority-applicant seat, the identical mechanism operates as an enforced transfer imposed on an individual who did not personally participate in creating the historical injury being repaired. The engine computes these as structurally different seat outcomes from the same authored data; the divergence is exactly what the tangled_rope classification is meant to surface — genuine coordination function AND asymmetric extraction through the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups and remedial administrators are declared beneficiaries: the former receive access, the latter receive institutional legitimacy and mission fulfillment, both with lower derived directionality toward extraction. Displaced nonminority applicants are declared victims: they bear a concentrated, identifiable cost through the identical mechanism, driving their directionality toward the target end. This is the core structural delta of the remedial reading relative to its siblings — under the colorblind reading, ANY race-conscious beneficiary structure is itself the extraction; under this reading, the extraction is the displacement of individuals who would have prevailed under a race-neutral baseline, while the race-conscious allocation itself is the coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state-enforced racial caste producing persistent subordination under formal neutrality) is authored as contested-status rather than flatly live or dead, because the empirical predicate (disparity persists) is corroborated by data outside the advocacy/administrator beneficiary set, while the appropriate institutional response to that predicate remains genuinely disputed across the kernel's readings. This prevents two mislabeling errors: treating the remedial reading as pure extraction (ignoring the real, externally-corroborated subordination problem it responds to) and treating it as pure coordination with no cost (ignoring the concentrated individual burden on displaced applicants). Tangled rope is the classification that holds both facts simultaneously without collapsing one into the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_causation_ambiguity,
    'Is present-day racial disparity in the domains targeted by remedial programs causally traceable to state-enforced caste subordination specifically, or to a broader mix of causes for which race-conscious remedy is an imprecise instrument?',
    'Longitudinal causal-inference research tracing specific disparity channels (wealth transmission, school segregation persistence, redlining legacy effects) to state action versus other factors; would need to isolate the state-caste-specific component from general socioeconomic disparity.',
    'If disparity is substantially caste-traceable, the remedial reading''s coordination function is strongly grounded and the tangled_rope classification''s coordination component is robust. If disparity is diffuse and multi-causal, the remedial mechanism''s tailoring to a ''compelling interest'' weakens and the extraction component becomes harder to justify as narrowly targeted repair, pushing the classification toward snare from the displaced-applicant seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_causation_ambiguity, empirical, 'Whether present disparity is caste-specific or diffusely multi-causal.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''equal protection'' best modeled as a single kernel with three competing readings (this file''s approach), or are the readings themselves evidence that no single kernel text stably grounds all three — i.e., the constitutional text has always been genuinely indeterminate rather than housing determinate-but-contested readings?',
    'Historical and originalist-methodology analysis of the Fourteenth Amendment''s drafting record and immediate post-ratification enforcement practice, compared against how each reading''s proponents construct their textual/historical case.',
    'If a single determinate original meaning exists and one reading tracks it while the others are constructions layered on later, that reading''s cs_structure.authority_grounding should arguably shift toward lineage-with-higher-fidelity and the others toward more contested grounding. If the text is genuinely indeterminate, all three readings sit on comparably contestable footing and coexists_with is the only defensible relation among all pairs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three-reading kernel model itself, versus genuine original indeterminacy, is the right framing.').

omega_variable(
    remedial_program_sunset_question,
    'Should remedial race-conscious programs under this reading carry an implicit or explicit sunset tied to measurable disparity closure, making this reading structurally closer to a scaffold than a tangled_rope?',
    'Track whether courts and legislatures attach durational limits or disparity-indexed termination triggers to remedial programs upheld under this reading, versus treating them as open-ended.',
    'If sunset conditions become standard and enforced, the remedial reading would better fit scaffold (transitional coordination with declared endpoint) rather than tangled_rope (ongoing extraction alongside coordination). Currently no reliable sunset mechanism is authored into most remedial programs, which is why this story claims tangled_rope rather than scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_program_sunset_question, preference, 'Whether remedial programs should be sunset-bound, and whether courts are moving that direction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__remedial_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1970, equal_protection_commitment__remedial_reading, theater_ratio, 1970, 0.13).
narrative_ontology:measurement(equa_tr_t1986, equal_protection_commitment__remedial_reading, theater_ratio, 1986, 0.17).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__remedial_reading, theater_ratio, 2003, 0.19).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_commitment__remedial_reading, theater_ratio, 2013, 0.21).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_commitment__remedial_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__remedial_reading, base_extractiveness, 1954, 0.35).
narrative_ontology:measurement(equa_be_t1970, equal_protection_commitment__remedial_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(equa_be_t1986, equal_protection_commitment__remedial_reading, base_extractiveness, 1986, 0.48).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__remedial_reading, base_extractiveness, 2003, 0.5).
narrative_ontology:measurement(equa_be_t2013, equal_protection_commitment__remedial_reading, base_extractiveness, 2013, 0.51).
narrative_ontology:measurement(equa_be_t2024, equal_protection_commitment__remedial_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__remedial_reading, suppression_requirement, 1954, 0.25).
narrative_ontology:measurement(equa_su_t1970, equal_protection_commitment__remedial_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(equa_su_t1986, equal_protection_commitment__remedial_reading, suppression_requirement, 1986, 0.36).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__remedial_reading, suppression_requirement, 2003, 0.4).
narrative_ontology:measurement(equa_su_t2013, equal_protection_commitment__remedial_reading, suppression_requirement, 2013, 0.42).
narrative_ontology:measurement(equa_su_t2024, equal_protection_commitment__remedial_reading, suppression_requirement, 2024, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__remedial_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'equal protection clause' per the epsilon-invariance principle: colorblind_reading (Mountain/Rope-leaning, near-zero authorized race-consciousness, ε low, beneficiaries limited to the abstract principle of formal neutrality), diversity_reading (moderate ε, beneficiaries are educational institutions and the diversity interest, narrower victim class limited to displaced applicants in education specifically), and this remedial_reading (tangled_rope, ε 0.45-0.60, the broadest beneficiary/victim inversion because it operates across admissions, contracting, and employment, not just education). Each carries its own ε and stakeholder set; they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
