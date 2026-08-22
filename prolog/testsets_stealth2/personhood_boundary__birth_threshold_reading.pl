% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Birth Threshold Reading of the Personhood Boundary
 *   domain: moral philosophy / legal ethics / commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the personhood_boundary kernel:
 *   the birth-threshold reading, under which moral and legal standing attach
 *   unconditionally at live birth and no further test - fitness, capacity, or
 *   potential - may revoke or grade them. The standing arrangement under
 *   contest is the modern personhood regime built on that line: homicide
 *   statutes covering every born person, birth registration conferring the
 *   standing documentarily, and child-protection law enforcing the duty side.
 *   The reading's distinctive structural feature is its unconditional grant:
 *   children with disabilities, who would be the first casualties of the
 *   sibling readings, sit fully inside the protected class. The colloquial
 *   question 'when does personhood begin' decomposes into three structurally
 *   distinct constraints (this reading and the two siblings); each is
 *   authored separately with its own epsilon, linked through
 *   network.affects_constraints. Claim and metrics are independent authored
 *   facts: the claim is rope; the metrics describe the arrangement's actual
 *   operation, including its eugenics-era wobble and its enforcement-heavy
 *   maintenance. KEY AGENTS (by structural relationship): - newborn_infants:
 *   protected class entire (powerless/trapped) - standing arrives at first
 *   breath and cannot be forfeited - children_with_disabilities: decisive
 *   differential beneficiary (powerless/trapped) - the unconditional grant is
 *   what separates this reading from its siblings - parents_and_families:
 *   beneficiary with payer residue (moderate/constrained) - gained certain
 *   protection, surrendered historical life-death discretion -
 *   perinatal_medical_professionals: payer with beneficiary residue
 *   (organized/constrained) - operationalize the line at delivery -
 *   state_legislatures_and_courts: agenda setter (institutional/constrained)
 *   - define the statutory line; briefly captured by fitness readings, then
 *   reverted - international_human_rights_bodies: analytical observer
 *   (institutional/analytical) - outside corroboration -
 *   capacity_theorist_bioethicists: excluded voice (moderate/mobile) - hold
 *   the sibling readings outside the legal settlement
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.09).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.7).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.09).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Birth Threshold Reading of the Personhood Boundary").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral philosophy / legal ethics / commitment systems").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'faff5c69-0e3e-47c2-b14a-7e1263648c3a').
narrative_ontology:cs_kernel_codification('faff5c69-0e3e-47c2-b14a-7e1263648c3a', formalized).
narrative_ontology:cs_authority_grounding('faff5c69-0e3e-47c2-b14a-7e1263648c3a', lineage).
narrative_ontology:cs_interpretation_layer_present('faff5c69-0e3e-47c2-b14a-7e1263648c3a').
narrative_ontology:cs_reading_relation('faff5c69-0e3e-47c2-b14a-7e1263648c3a', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('faff5c69-0e3e-47c2-b14a-7e1263648c3a', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('faff5c69-0e3e-47c2-b14a-7e1263648c3a', foundational, unconditional_moral_standing_for_all_born_humans).
narrative_ontology:cs_axiom_status(unconditional_moral_standing_for_all_born_humans, holdable).
narrative_ontology:cs_axiom_grounding('faff5c69-0e3e-47c2-b14a-7e1263648c3a', unconditional_moral_standing_for_all_born_humans, deontological).
narrative_ontology:cs_axiom('faff5c69-0e3e-47c2-b14a-7e1263648c3a', secondary, bright_line_preferred_to_graded_assessment).
narrative_ontology:cs_axiom_status(bright_line_preferred_to_graded_assessment, holdable).
narrative_ontology:cs_axiom_grounding('faff5c69-0e3e-47c2-b14a-7e1263648c3a', bright_line_preferred_to_graded_assessment, instrumental).
narrative_ontology:cs_reference_frame('faff5c69-0e3e-47c2-b14a-7e1263648c3a', unconditional_birth_moral_equality).
narrative_ontology:cs_drift_state('faff5c69-0e3e-47c2-b14a-7e1263648c3a', post_human_rights_consolidation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('faff5c69-0e3e-47c2-b14a-7e1263648c3a', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, newborn_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, children_with_disabilities).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, parents_and_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, perinatal_medical_professionals).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, parents_and_families).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, perinatal_medical_professionals).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, born_alive_rule).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, equal_dignity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are inside the protected class from the moment of live birth: homicide law covers them, birth registration documents them, and care duties attach to everyone responsible for them. They contribute nothing to the arrangement's operation and can do nothing to forfeit their place in it; every element of their protection is delivered by other parties' conduct. Their position is total dependence: they cannot leave, appeal, or substitute anything for the protection they receive.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, newborn_infants, beneficiary,
    powerless, immediate, trapped, global).

% Live their entire lives inside the protected class on the same unconditional terms as everyone else born. The grant to them involves no assessment of capacity, prognosis, or cost; their standing does not depend on anyone's judgment of their abilities. They are the class whose treatment differs most sharply depending on which reading of the boundary governs, and they have no voice in that contest.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, children_with_disabilities, beneficiary,
    powerless, biographical, trapped, global).

% Receive legally guaranteed protection for their infants and children, backed by prosecution of anyone who violates it. In exchange they surrendered the discretion over infant life that parents exercised in most earlier societies, and they carry affirmative duties: registration, care obligations, cooperation with child-protection authorities. They cannot contract out of the arrangement or waive it on a child's behalf.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, parents_and_families, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, parents_and_families, payer).

% Work at the exact point where the line is drawn. Delivery-room teams make resuscitation and documentation decisions that determine whether the protections attach, and they carry criminal and malpractice exposure when borderline decisions go wrong. They benefit from the line's clarity - a single observable event replaces case-by-case judgment - while absorbing its marginal decision costs.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, perinatal_medical_professionals, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, perinatal_medical_professionals, beneficiary).

% Define the statutory line - born-alive rules, homicide definitions, registration requirements - and adjudicate its edge cases. Every clarification of the line extends their prosecutorial and administrative jurisdiction. During the eugenics decades parts of this seat enacted fitness-conditioned policies before postwar repudiation restored the unconditional line; the seat cannot now abandon enforcement without collapsing the arrangement it administers.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, state_legislatures_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Monitor state compliance through treaty reporting, review country submissions, and publish findings on infant and child welfare. They hold no enforcement force of their own; their function is documentation and corroboration from outside any single state's institutions. Their archives supply the outside attestation for the arrangement's founding problem and its status.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Argue in journals and university seminars that standing should track demonstrated capacities or potential rather than birth, and that the unconditional line dodges questions they consider live. No legislature has adopted their position for born humans; their influence is confined to academic discourse and occasional bioethics consultation. They can publish, travel, and organize freely - their exclusion is from the legal settlement, not from speech.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, capacity_theorist_bioethicists, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a single administrable bright line separating the moral-legal community from the not-yet: anyone can know who counts without case-by-case assessment of worth, and homicide law, birth registration, and child protection all key off one observable event. It solves the collective-action problem of protecting helpless members without requiring per-child judgments that caregivers or states would inevitably disagree about.
% TRANSFER_FUNCTION: Moves security of person to every born human, universally and unconditionally, funded by diffuse public enforcement cost; simultaneously moves life-and-death discretion over infants out of private and state hands and into the public rule. Nothing of value is transferred to any concentrated recipient.
% ABSENT_VOICES: Capacity theorists and holders of the sibling readings are outside the legal settlement - they would re-open the boundary with graded standing and are represented nowhere in statute. Historically, eugenic boards spoke for exclusion and were defeated. The infants themselves cannot speak at all; their silence is total and permanent, which is precisely why the bright line substitutes a rule for their missing voice.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, fitness and potential tests would rush into the vacuum; neonaticide and abandonment would return wherever caregivers judged infants burdensome; disability communities would face immediate graded-standing proposals; homicide law would fragment into contested categories. Nearly every institution touching infancy would reorganize.
% FOUNDING_PROBLEM: Infant survival hung on caregiver discretion: exposure, infanticide, and selective neglect were ordinary across most of recorded history, and communities needed a rule that removed infant survival from case-by-case judgment.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: criminal prosecutions of neonaticide and child-homicide statistics attest the problem's residual liveness; UNICEF and demographic reporting document continuing infant vulnerability; disability-rights organizations attest the stakes remain real for their members. Capacity theorists corroborate that a contest exists, though they dispute the solution rather than the problem. No source outside the beneficiary set attests that the problem is fully dead.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.09, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).
:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.09 at interval end) because the arrangement's costs - surrendered discretion, enforcement expenditure, marginal decision burden at the line - are small and near-universally shared, while the protection delivered is existential for the protected class. Suppression is high (0.70) and rising across the series: the norm is maintained by criminal law, mandatory reporting, and child-protection machinery, and its persistence has always depended on actively prosecuting violations rather than on voluntary compliance; the mid-interval dip (0.36 at 1940) marks the eugenics-era erosion of enforcement will, an external shock rather than a cycle. Theater peaks mid-century (0.28 at 1960) when declaratory instruments outran enforcement machinery, then falls to 0.08 as implementation matured. Accessibility collapse is moderate-high (0.62): within any framework that accepts the unconditional grant, excluding a born human becomes contradictory, but the sibling readings persist as live social positions, so alternatives collapse inside the frame rather than across society. Resistance is low-moderate (0.25): behavioral resistance collapsed after 1945; what remains is argumentative. All three series share one time grid (1900-2020 at twenty-year steps) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different classifications from identical structural data. From the newborn's position the arrangement is indistinguishable from the sky - it is the condition of having standing at all, experienced by a party with no capacity to perceive it. From the parents' position it is a trade: discretion surrendered, certainty gained. From the courts' position it is an administered line that occasionally generates tragic edge cases. From the capacity theorist's position it is an arbitrary dogma that forecloses questions they regard as live. The engine computes these divergences from power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: newborn_infants and children_with_disabilities sit at the full-beneficiary end (d near 0) - the arrangement subsidizes them existentially and they are held inside it by helplessness rather than by design. parents_and_families derive low-to-mid d: net beneficiaries carrying real payer residue (discretion surrendered, enforcement obligations borne). perinatal_medical_professionals derive mid d: they pay the boundary's marginal decision costs and collect its clarity. state_legislatures_and_courts derive low-mid d: they administer the line and collect jurisdiction, not rents - the arrangement's gains do not accrue to them as extraction. No victims are declared because this reading has none in the structural sense: no group bears asymmetric extraction through the arrangement; its costs are the diffuse price of the coordination itself. Accordingly gain_flow is authored as 'diffuse' - an affirmative finding after checking every named seat, not a default - and fixing_cost as 'prohibitive': dismantling the arrangement would require reversing the deepest consensus in modern law against the interest of every protected seat, for the benefit of none.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - infant survival hanging on caregiver discretion - regenerates with every birth, so the arrangement cannot outlive its function the way a completed program can; mandatrophy is not resolved. The analysis guards against two misreadings. First, the mid-century theater peak and the eugenics-era suppression dip could be misread as decay toward inertial persistence; the series shows the opposite - theater fell and enforcement hardened as the arrangement consolidated, and the prohibitive-fixing/diffuse-gain combination reflects a load-bearing structure, not an abandoned shell, because the function (theater 0.08) is demonstrably alive. Second, the low extraction could be misread as the arrangement being uncontested; the kernel contest is live precisely because the sibling readings would redraw the protected class, and the omega variables carry that contest rather than letting the rope classification bury it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the birth threshold the correct placement of the personhood boundary, or do the sibling readings (fitness-contingent, potential-based) better track moral standing?',
    'Public-reason convergence, legislative choice, and long-run institutional performance across jurisdictions adopting different placements; no decisive empirical test exists, so resolution tracks which reading retains the ability to organize stable law.',
    'Adopting a sibling reading would remove disabled born children from the protected class, authorize graded homicide categories, and convert this arrangement into one with identifiable victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the personhood_boundary kernel should govern.').

omega_variable(
    bright_line_margin_cost,
    'Does the bright line''s arbitrariness at the margin (minutes on either side of delivery) impose real decision and liability costs that the headline extraction figure understates?',
    'Compare malpractice rates, prosecution patterns, and family outcomes in jurisdictions operating birth-based versus gestational-age-based rules.',
    'If margin costs are large, effective extraction at the clinician and family seats is higher than the scalar suggests; if negligible, the near-pure coordination reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bright_line_margin_cost, empirical, 'Hidden cost of the bright line''s arbitrariness at the delivery margin.').

omega_variable(
    selective_enforcement_history,
    'Has the arrangement''s promise of universal standing been delivered universally, or does formal adoption coexist with selective denial (enslaved children historically, institutionalized disabled infants during the eugenics era, residual neonaticide today)?',
    'Archival demographic reconstruction and contemporary prosecution and mortality data disaggregated by race, disability, and income.',
    'Persistent selectivity raises the extraction actually borne by marginalized born humans above the headline figure and would support reading those populations'' experience as hybrid coordination-extraction rather than pure protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_history, empirical, 'Whether universal standing is universal in fact.').

omega_variable(
    fitness_reading_recurrence,
    'Can fitness-contingent reasoning recapture state policy as it did between roughly 1900 and 1945, converting the excluded-class dynamics back into active exclusion?',
    'Monitor legislation and clinical guidelines that condition neonatal treatment or legal protection on disability status or projected capacity.',
    'Recurrence would flip the arrangement''s operation for the targeted class from protection to exposure, dating a type transition in the period following this interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_reading_recurrence, empirical, 'Risk of eugenics-era fitness readings recapturing policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1900, personhood_boundary__birth_threshold_reading, theater_ratio, 1900, 0.14).
narrative_ontology:measurement(pers_tr_t1920, personhood_boundary__birth_threshold_reading, theater_ratio, 1920, 0.16).
narrative_ontology:measurement(pers_tr_t1940, personhood_boundary__birth_threshold_reading, theater_ratio, 1940, 0.24).
narrative_ontology:measurement(pers_tr_t1960, personhood_boundary__birth_threshold_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement(pers_tr_t1980, personhood_boundary__birth_threshold_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(pers_tr_t2000, personhood_boundary__birth_threshold_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(pers_tr_t2020, personhood_boundary__birth_threshold_reading, theater_ratio, 2020, 0.08).

% Extraction over time
narrative_ontology:measurement(pers_be_t1900, personhood_boundary__birth_threshold_reading, base_extractiveness, 1900, 0.16).
narrative_ontology:measurement(pers_be_t1920, personhood_boundary__birth_threshold_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement(pers_be_t1940, personhood_boundary__birth_threshold_reading, base_extractiveness, 1940, 0.17).
narrative_ontology:measurement(pers_be_t1960, personhood_boundary__birth_threshold_reading, base_extractiveness, 1960, 0.13).
narrative_ontology:measurement(pers_be_t1980, personhood_boundary__birth_threshold_reading, base_extractiveness, 1980, 0.11).
narrative_ontology:measurement(pers_be_t2000, personhood_boundary__birth_threshold_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(pers_be_t2020, personhood_boundary__birth_threshold_reading, base_extractiveness, 2020, 0.09).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1900, personhood_boundary__birth_threshold_reading, suppression_requirement, 1900, 0.44).
narrative_ontology:measurement(pers_su_t1920, personhood_boundary__birth_threshold_reading, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(pers_su_t1940, personhood_boundary__birth_threshold_reading, suppression_requirement, 1940, 0.36).
narrative_ontology:measurement(pers_su_t1960, personhood_boundary__birth_threshold_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement(pers_su_t1980, personhood_boundary__birth_threshold_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(pers_su_t2000, personhood_boundary__birth_threshold_reading, suppression_requirement, 2000, 0.66).
narrative_ontology:measurement(pers_su_t2020, personhood_boundary__birth_threshold_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'when does personhood begin' conflates three structurally distinct constraints. This file authors the birth-threshold reading alone: low extraction, no victims, universal unconditional protection. The fitness-contingent and potential-based readings instantiate different constraints with different victim sets (excludable born infants) and materially higher extraction; they are authored separately and linked here. The birth-threshold reading is the consolidated settlement and sits upstream: its legal entrenchment shapes the legitimacy and resource conditions under which the sibling readings survive at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
