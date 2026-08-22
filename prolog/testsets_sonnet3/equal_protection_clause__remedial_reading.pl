% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial (Substantive Equality) Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This story instantiates the remedial reading of the Equal Protection
 *   Clause: the claim that the Clause not only permits but requires
 *   race-conscious governmental action where necessary to remediate the
 *   durable effects of historically state-sanctioned group subordination, in
 *   order to achieve substantive (as opposed to merely formal) equality. This
 *   reading is one of three structurally distinct constructions of the same
 *   constitutional text — the colorblind reading (forbidding all racial
 *   classification) and the diversity reading (permitting race-consciousness
 *   only for forward-looking pedagogical benefit) are separate constraint
 *   stories with their own ε values and beneficiary/victim structures, linked
 *   here via network.affects_constraints. This story's ε is authored strictly
 *   for the remedial reading's own operation: high extraction because the
 *   reading licenses a substantial, group-defined reallocation of scarce
 *   positions justified by historical rather than individual injury, which
 *   necessarily displaces individually-qualified non-beneficiaries. The
 *   extraction is not a critique smuggled in from a rival reading — it is
 *   what the remedial reading's own logic openly does: it trades formal
 *   individual equal treatment for group-historical redress, and that trade
 *   IS the mechanism, not a side effect.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_minorities: primary beneficiary group (organized/national) — the reading's normative center of gravity
 *   - displaced_nonpreferred_applicants: primary target (moderate/constrained) — bears the individual cost of group-level remedy
 *   - asian_american_applicant_cohorts: same-level lateral actor bearing disproportionate displacement despite own history of subordination — the reading's most acute internal tension
 *   - admitting_institutions: agenda-setter administering the remedial criteria
 *   - federal_courts: analytical observer who will ultimately decide the reading's constitutional durability
 *   - colorblind_reading_advocates: excluded voice, structurally external to this reading's own framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.62).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.58).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Clause — Remedial (Substantive Equality) Reading").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, 'e6954caa-95ca-4ff5-86b9-8ada01466a4d').
narrative_ontology:cs_kernel_codification('e6954caa-95ca-4ff5-86b9-8ada01466a4d', fixed_text).
narrative_ontology:cs_authority_grounding('e6954caa-95ca-4ff5-86b9-8ada01466a4d', lineage).
narrative_ontology:cs_interpretation_layer_present('e6954caa-95ca-4ff5-86b9-8ada01466a4d').
narrative_ontology:cs_reading_relation('e6954caa-95ca-4ff5-86b9-8ada01466a4d', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('e6954caa-95ca-4ff5-86b9-8ada01466a4d', equal_protection_clause__diversity_reading, influences).
narrative_ontology:cs_axiom('e6954caa-95ca-4ff5-86b9-8ada01466a4d', foundational, group_subordination_licenses_group_remedy).
narrative_ontology:cs_axiom_status(group_subordination_licenses_group_remedy, holdable).
narrative_ontology:cs_axiom_grounding('e6954caa-95ca-4ff5-86b9-8ada01466a4d', group_subordination_licenses_group_remedy, empirically_contingent).
narrative_ontology:cs_axiom('e6954caa-95ca-4ff5-86b9-8ada01466a4d', foundational, formal_equality_insufficient_for_substantive_equality).
narrative_ontology:cs_axiom_status(formal_equality_insufficient_for_substantive_equality, holdable).
narrative_ontology:cs_axiom_grounding('e6954caa-95ca-4ff5-86b9-8ada01466a4d', formal_equality_insufficient_for_substantive_equality, conventional).
narrative_ontology:cs_reference_frame('e6954caa-95ca-4ff5-86b9-8ada01466a4d', reconstruction_era_antisubordination_purpose).
narrative_ontology:cs_drift_state('e6954caa-95ca-4ff5-86b9-8ada01466a4d', post_2023_admissions_jurisprudence, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e6954caa-95ca-4ff5-86b9-8ada01466a4d', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, black_applicant_cohorts).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, indigenous_applicant_cohorts).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, displaced_nonpreferred_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, asian_american_applicant_cohorts).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, anticaste_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Group whose ancestors experienced legally codified subordination (slavery, Jim Crow, redlining, land dispossession). Under this reading, admissions and hiring bodies must weigh this history as a compelling justification for race-conscious remedy. Benefits accrue as improved access to selective institutions and positions, framed as compensatory rather than merely diversifying.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities, beneficiary,
    organized, generational, constrained, national).

% Individual applicants within the beneficiary group. They experience improved odds at selective institutions justified by group-level historical harm, but their individual circumstances vary widely — the group-level remedy does not track individual disadvantage precisely.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, black_applicant_cohorts, beneficiary,
    moderate, biographical, constrained, national).

% Small population with documented land and sovereignty dispossession. Benefits from remedial consideration but has essentially no exit from the national institutions in question — there is no alternative jurisdiction offering comparable opportunity, making the remedy's presence or absence highly consequential.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, indigenous_applicant_cohorts, beneficiary,
    powerless, generational, trapped, national).

% Individually qualified applicants who are not members of a remedially-favored group and who are denied a seat or position that would otherwise have gone to them, because the institution is allocating slots partly on the basis of group remediation. They bear the cost of a policy addressing a historical harm they personally did not cause, with no individualized showing that they benefited from it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, displaced_nonpreferred_applicants, payer,
    moderate, biographical, constrained, national).

% A group that is itself a historically discriminated-against minority (exclusion acts, internment, quotas) but which this reading does not place in the primary remedial beneficiary class for selective admissions; empirically bears a disproportionate share of the displacement cost under race-conscious remedial formulas, creating a minority-versus-minority allocation tension internal to the reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, asian_american_applicant_cohorts, payer,
    moderate, biographical, constrained, national).

% Universities, employers, and agencies that design and administer remedial race-conscious criteria, weigh their strength, and defend them in litigation. They set the operative definition of 'historical subordination' and the intensity of the remedy, and bear reputational and legal exposure but also gain legitimacy narratives from appearing to redress historical wrongs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, admitting_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicate whether a given remedial program is sufficiently tailored to an identified constitutional or statutory violation, and whether it can persist once that violation's effects are deemed remedied. Courts occupy the seat that must eventually decide whether this reading remains a live constitutional interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Legal scholars, litigants, and organized advocacy groups who hold that any race-conscious remedy of this kind is itself the constitutional violation. They are not part of the remedial reading's own operative framework — their objection is structurally external to this constraint, which is why they appear as excluded here rather than as a payer inside it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_reading_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal response to documented, government-sanctioned historical subordination by allocating scarce institutional positions partly on group membership, on the theory that formally equal treatment of unequally-positioned groups perpetuates rather than cures the subordination.
% TRANSFER_FUNCTION: Moves admission slots, hiring positions, and associated opportunity from individually-qualified applicants outside the remedial beneficiary classes to applicants within them, justified by group-level historical harm rather than individualized injury or individualized redress-entitlement.
% ABSENT_VOICES: Colorblind-reading advocates and individual displaced applicants who argue the remedy imposes present costs on persons with no individual culpability are structurally outside this reading's own framework, which treats group-level historical accounting as the relevant unit of analysis rather than individual desert.
% DISAPPEARANCE_RATIONALE: If this reading's remedial framework disappeared, admissions and hiring criteria at institutions currently operating race-conscious remedial programs would revert to whatever baseline (colorblind or diversity-rationale) constraint replaced it, immediately shifting the composition of admitted/hired cohorts and removing the legal cover institutions currently rely on to justify group-conscious allocation.
% FOUNDING_PROBLEM: Formal legal equality (post-1964/1965) left in place the accumulated effects of centuries of state-sanctioned subordination — wealth gaps, educational access gaps, residential segregation — that facially neutral, forward-looking equal treatment does not by itself undo.
% FOUNDING_PROBLEM_CORROBORATION: Social-science researchers studying persistent racial wealth and educational attainment gaps (a source outside the set of institutions that administer or benefit from the remedy) corroborate that the underlying disparities the founding problem identifies remain measurable; colorblind-reading courts and litigants corroborate that the specific legal violation the remedy targeted (de jure segregation) has been formally terminated, disputing whether present disparities alone sustain a live constitutional remedial mandate.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.62-0.70, declining modestly as remedial scope theoretically narrows toward eventual sunset) because the remedial reading's coordination mechanism is inseparable from a real, individually-borne cost: a qualified applicant outside the beneficiary class is displaced without any individualized finding that they personally benefited from historical subordination or that the remedy is narrowly tailored to their own conduct. Suppression is moderate-high and rising (0.45 to 0.58) reflecting the increasing legal and institutional infrastructure (compliance offices, litigation defense, holistic-review methodologies) required to defend race-conscious criteria against colorblind-reading challenges as doctrinal pressure mounts. Theater ratio is low but slowly rising (0.12 to 0.22), reflecting a documented pattern where institutions retain rhetorical commitment to remedial goals while operational criteria drift toward diversity-rationale proxies that are easier to defend in litigation — a genuine but partial metric substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary-group seat, this reading is coordination: a correction of an ongoing structural injustice that formal equal treatment cannot reach. From the displaced-applicant seat, the identical mechanism is extraction: bearing a cost assigned by group membership rather than individual conduct. The engine computes these as different per-seat classifications from the same structural data; this divergence is exactly the phenomenon the framework is built to register, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated minorities and their sub-cohorts are declared beneficiaries — the reading's entire justification runs through their group-level history, so directionality sits near the full-beneficiary end despite individual variation within the group. Displaced nonpreferred applicants and Asian American applicant cohorts are declared victims — they bear the allocation cost through the identical mechanism that delivers the benefit, with constrained exit (there is no alternate elite-institution pipeline that avoids the same criteria nationally). Asian American applicant cohorts present the sharpest structural tension: a group with its own documented historical subordination that this reading does not place in its primary remedial class, generating a minority-versus-minority allocation conflict that is internal to the remedial reading rather than an artifact of a rival reading's critique.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification requires an honest sunset clause: the remedial reading's own internal logic holds that once the effects of historical subordination are substantially remediated, the compelling justification lapses and race-conscious criteria should sunset. The mandatrophy risk is that institutions retain the remedial architecture (and its extraction) after the founding problem — legally enforced subordination and its direct, traceable effects — has substantially receded, substituting a diffuse claim about persistent disparity for the original targeted-remediation theory. The founding_problem_status is authored 'contested' precisely because this is unresolved: whether the present-day disparities constitute the SAME founding problem in an ongoing form, or a different, less specifically remediable phenomenon that the remedial architecture has drifted into addressing without a coherent sunset theory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which of the three readings of the Equal Protection Clause (colorblind, diversity, remedial) is the constitutionally correct one, and does the remedial reading''s own logic supply a workable end-condition for when remediation is ''complete''?',
    'This is not empirically resolvable by data internal to any one reading — it depends on unsettled and evolving Supreme Court doctrine, competing theories of constitutional interpretation (originalism vs. living constitutionalism vs. anticaste theory), and contested historical and social-scientific findings about the causal persistence of historical subordination''s effects.',
    'If courts foreclose the remedial reading (as recent doctrinal trends toward the colorblind reading suggest), this constraint''s entire beneficiary/victim structure becomes constitutionally impermissible rather than merely contested, and the scaffold''s sunset becomes externally imposed rather than internally triggered by remediation completion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer-level uncertainty: which kernel reading of Equal Protection controls, and what would resolve it.').

omega_variable(
    sunset_trigger_indeterminacy,
    'What observable condition would count as ''remediation complete'' such that this scaffold''s sunset clause actually fires, rather than the constraint persisting indefinitely on a renewed diffuse-disparity justification?',
    'Would require a pre-committed, falsifiable metric (e.g., convergence of specific attainment/wealth statistics to within a defined band) adopted ex ante by the administering institutions or by courts, rather than an open-ended ''until equality is achieved'' standard.',
    'Without a falsifiable trigger, the scaffold classification is vulnerable to mandatrophy: the remedial architecture could persist as inertial extraction (piton-adjacent) long after its founding problem has substantially receded, while retaining scaffold''s structural cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_trigger_indeterminacy, empirical, 'Whether a workable, falsifiable sunset trigger exists or the sunset clause is aspirational only.').

omega_variable(
    asian_american_minority_within_minority_tension,
    'Is the disproportionate displacement cost borne by Asian American applicant cohorts a structural feature the remedial reading''s own theory can justify, or does it reveal an internal inconsistency between the reading''s group-subordination premise and its actual allocation outcomes?',
    'Disaggregated admissions data across institutions operating remedial (vs. diversity-rationale) programs, compared against the reading''s own stated criteria for which groups qualify as subordination-beneficiaries and why.',
    'If the displacement is disproportionate relative to any coherent historical-subordination ranking, it suggests the operative mechanism has drifted from the remedial theory toward a proxy (e.g., simple representation targets) that the remedial reading''s own axioms would not endorse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asian_american_minority_within_minority_tension, empirical, 'Whether observed allocation outcomes are consistent with the remedial reading''s stated theory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__remedial_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(equa_tr_t10, equal_protection_clause__remedial_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__remedial_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(equa_tr_t30, equal_protection_clause__remedial_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__remedial_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(equa_tr_t50, equal_protection_clause__remedial_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(equa_tr_t60, equal_protection_clause__remedial_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__remedial_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(equa_be_t10, equal_protection_clause__remedial_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__remedial_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(equa_be_t30, equal_protection_clause__remedial_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__remedial_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(equa_be_t50, equal_protection_clause__remedial_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(equa_be_t60, equal_protection_clause__remedial_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__remedial_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(equa_su_t10, equal_protection_clause__remedial_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__remedial_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(equa_su_t30, equal_protection_clause__remedial_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__remedial_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(equa_su_t50, equal_protection_clause__remedial_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(equa_su_t60, equal_protection_clause__remedial_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language label 'the Equal Protection Clause's treatment of race' per the ε-invariance principle. colorblind_reading (near-zero ε, formal-equality mandate, likely mountain-or-rope from its own lights), diversity_reading (moderate ε, forward-looking pedagogical justification, likely tangled_rope), and remedial_reading (this file — high ε, backward-looking historical-subordination justification, scaffold with contested sunset) share the same constitutional text but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε values. They are linked via network.affects_constraints rather than merged, per the BGS decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
