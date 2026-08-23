% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Birth-Threshold Personhood Boundary (Categorical Reading)
 *   domain: moral philosophy / historical ethics / commitment systems
 *
 * SUMMARY:
 *   This file instantiates the birth-threshold reading of the
 *   personhood_boundary kernel: moral and legal standing attach to every
 *   human being at live birth, killing any born human is homicide, and no
 *   state holds authority to declare a born human a non-person. Its
 *   distinctive structural feature is categorical inclusion — the class that
 *   sibling readings would expose to evaluation (born infants failing fitness
 *   or rational-potential tests) lies wholly inside protection, so no
 *   excluded-newborn victim class exists under this reading. The epsilon
 *   referent is the standing birth-threshold arrangement itself, assessed by
 *   this reading's own lights, which yields a low value: the arrangement
 *   protects rather than transfers, its costs falling mainly on families of
 *   gravely impaired newborns and on public care budgets. Claimed type and
 *   metrics are authored independently: rope is asserted from the structural
 *   belief that this is genuine coordination with net-benefit participation
 *   and unsuppressed alternatives, while the metrics describe the arrangement
 *   as it actually operates, including its concentrated residual costs.
 *   Family linkage: personhood_boundary__fitness_contingent_reading and
 *   personhood_boundary__potential_based_reading.
 *
 * KEY AGENTS:
 *   - all_born_humans: universal beneficiary (powerless/trapped) — holds categorical standing it cannot resign or be stripped of
 *   - vulnerable_newborns: primary protected class (powerless/immediate/trapped) — homicide-grade protection from birth regardless of condition
 *   - severely_disabled_born_individuals: acute beneficiary (powerless/biographical/trapped) — standing never evaluated, dependent on the categorical rule
 *   - parents_of_severely_impaired_newborns: primary payer (moderate/constrained) — bears concentrated care and option-loss costs
 *   - state_legal_authorities: agenda_setter (institutional/generational/arbitrage) — writes, adjudicates, and enforces the line; holds formal power to move it
 *   - neonatal_care_professionals: dual-positioned beneficiary/payer (organized/mobile) — applies the rule at the bedside; clear standards gained, moral burden borne
 *   - bioethics_dissidents: excluded challenger (moderate/global/analytical) — publishes fitness- and potential-based alternatives with no lawmaking seat
 *   - human_rights_institutions: observer (institutional/generational/analytical) — monitors adherence and extends the norm through treaty and guideline channels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.16).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.38).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.09).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.09).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Birth-Threshold Personhood Boundary (Categorical Reading)").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral philosophy / historical ethics / commitment systems").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, '6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc').
narrative_ontology:cs_kernel_codification('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', formalized).
narrative_ontology:cs_authority_grounding('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', distributed).
narrative_ontology:cs_reading_relation('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', foundational, all_born_humans_possess_moral_standing).
narrative_ontology:cs_axiom_status(all_born_humans_possess_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', all_born_humans_possess_moral_standing, deontological).
narrative_ontology:cs_axiom('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', foundational, no_state_authority_to_exclude_born_persons).
narrative_ontology:cs_axiom_status(no_state_authority_to_exclude_born_persons, holdable).
narrative_ontology:cs_axiom_grounding('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', no_state_authority_to_exclude_born_persons, deontological).
narrative_ontology:cs_reference_frame('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', categorical_birth_inclusion_frame).
narrative_ontology:cs_drift_state('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', contemporary_bioethics_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6a56b775-04d5-4c04-ad0f-cffd7ee3c9cc', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, all_born_humans).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, vulnerable_newborns).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, severely_disabled_born_individuals).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, parents_of_severely_impaired_newborns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, neonatal_care_professionals).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, neonatal_care_professionals).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, bright_line_personhood_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, equal_moral_status_of_all_born_humans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every human being from the moment of live birth falls under unconditional legal and moral protection: killing any born human is homicide, and no authority may declare a born human a non-person. Members span every capacity level and every degree of social power, but the standing attaches by category rather than by merit, and no member can resign from it or be expelled from it.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, all_born_humans, beneficiary,
    powerless, generational, trapped, global).

% Newborns are the most dependent humans in existence: they cannot speak, flee, advocate, or bargain, and their survival depends entirely on other people's restraint and care. The arrangement guarantees their lives homicide-grade protection regardless of health, wantedness, or parental circumstances, and their interests reach the threshold-setting process only through adult proxies.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, vulnerable_newborns, beneficiary,
    powerless, immediate, trapped, global).

% Individuals with profound cognitive impairment would fail any demonstrated-fitness or rational-potential test; under this arrangement their standing never comes up for evaluation at all. They depend on the categorical rule because they cannot demonstrate, negotiate, or defend their own qualification for protection, and their lifetime care is publicly and familially funded.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, severely_disabled_born_individuals, beneficiary,
    powerless, biographical, trapped, global).

% Families receiving a catastrophic neonatal diagnosis face lifetimes of intensive caregiving under rules that leave no lawful path to active ending of an impaired infant's life and narrow paths even to withdrawal of life-sustaining treatment beyond ordinary medical-futility judgments. Relocation to more permissive jurisdictions exists but is rare, costly, and legally fraught. These families bear the sharpest day-to-day costs of the categorical rule while sharing in its general benefits.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, parents_of_severely_impaired_newborns, payer,
    moderate, biographical, constrained, national).

% Legislatures write the birth line into vital-statistics and homicide statutes; courts adjudicate edge cases such as live-birth criteria and perinatal boundaries; prosecutors enforce. They hold the formal power to redraw or abandon the threshold — exactly the change the sibling readings propose — and refrain under constitutional, professional, and cultural pressure that has kept the line fixed for generations. They collect order and legitimacy from the rule's stability while financing its enforcement.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, state_legal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Physicians and nurses in neonatal units apply the rule at the bedside: every delivered infant is a patient with full claims to treatment. Uniform standards spare them case-by-case status judgments, but in grave-prognosis cases they carry the sustained moral weight of maintaining lives the categorical rule gives them no helpful way to shorten, and attrition out of neonatal specialties is a recognized response. Professional bodies lobby on protocol questions without controlling the threshold itself.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, neonatal_care_professionals, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, neonatal_care_professionals, payer).

% Academic philosophers and bioethicists argue that birth is the wrong line — that demonstrated fitness, sentience, or rational potential should govern instead — publishing direct challenges to the categorical premise and proposals for regulated neonatal decision-making. They hold publication venues and disciplinary standing but no seat in the lawmaking process that fixes the threshold, and their proposals have never been enacted in any major jurisdiction.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, bioethics_dissidents, excluded,
    moderate, biographical, analytical, global).

% Treaty bodies, United Nations agencies, and medical associations monitor adherence: they investigate infanticide and abandonment, press states toward universal birth registration and protection, and document violations where enforcement lags. They extend the norm's reach through conventions and guidelines without administering any particular state's statute.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, human_rights_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single publicly observable event — live birth — at which complete legal and moral protection attaches to a human being, replacing continuous case-by-case assessment of capacity, fitness, or potential with one administrable criterion that every participant can verify without expert adjudication.
% TRANSFER_FUNCTION: Moves unconditional protection toward every born human and imposes matching restraint obligations on everyone else; concentrates decision and care costs on the families of gravely impaired newborns and on public budgets; removes newborn life-and-death adjudication from parental and state discretion.
% ABSENT_VOICES: Parents of newborns with catastrophic prognoses who would choose comfort-only care or neonatal euthanasia have no seat in threshold-setting; historical practitioners of infanticide and exposure are absent by design; academic defenders of fitness- and potential-based criteria publish but hold no lawmaking seat; the newborns themselves cannot speak and enter only through proxies whose incentives diverge from theirs.
% DISAPPEARANCE_RATIONALE: If the categorical birth threshold vanished overnight, personhood would become an adjudicated property: homicide law would fragment into fitness- and potential-assessment regimes, states would recover authority to declare classes of newborns non-persons, protection of impaired infants would track evaluators' judgments, and the medical, legal, and familial division of labor built on the bright line would reorganize around case-by-case standing review.
% FOUNDING_PROBLEM: Arbitrary killing and exclusion of newborns: recorded societies practiced infanticide and exposure of unwanted, illegitimate, female, or imperfect infants, and assigned fathers or states discretionary power over whether a newborn's life counted. The arrangement was built to place every born human permanently beyond that discretion.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the arrangement's benefit structure by historical demography (foundling-institution mortality records documenting exposure-driven death rates), classical legal sources recording lawful paternal exposure rights, and contemporary human-rights and criminal reporting documenting infanticide where the norm is weakly enforced. No corroborating source sits inside the benefiting parties — the historians and monitors attest the persistence of the founding problem, not the merits of this arrangement.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low (0.16) because the arrangement transfers almost nothing out of its participants: protection is the product, and the residual costs — concentrated family care burdens, public care spending, foreclosed neonatal end-of-life options — are obligations of the arrangement, not receipts collected by any seat. Suppression is 0.38: criminal enforcement of the homicide line is real and the prohibition on exclusion declarations is absolute, but the competing readings remain fully live in academic and public discourse, so the arrangement suppresses actions, not alternatives. Theater ratio is low (0.09): the function is performed constantly and verifiably in every delivery room and courtroom. Accessibility collapse is 0.42 — accepting the categorical premise collapses the practical alternatives (any regime permitting newborn exclusion) but leaves the conceptual siblings intact. Resistance is 0.35: a sustained, serious philosophical challenge exists (it drives the axiom_overriding drift state below) while organized political resistance is marginal. Coordination type is identity_coordination: the arrangement's dominant function is boundary maintenance — coordinating membership claims in the moral community against evolving criteria — and the FNL gaming risk is checked rather than assumed away: the identity framing here delivers genuine membership clarity, and the measured excess extraction over the type floor stays small. Suppression is authored as a raw structural property; the engine alone scales effective extractiveness by directionality and scope. All three metric series share one time grid ({0, 20, 40, 60, 80, 100}, mapping approximately onto 1925–2025): extractiveness creeps upward as intensive-care technology extends impaired-infant survival and shifts costs onto families; theater stays flat and low; suppression_requirement rises through the century's codification ratchet (postwar repudiation of exclusionary programs, universal birth-registration campaigns, child-rights instruments), modeling enforcement machinery that matured and hardened — the one dynamic this story deliberately tracks temporally.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute sharply different classifications from identical structural data. From the parents-of-impaired-infants position the arrangement is experienced as foreclosure — a rule that removes their hardest options and hands the difference to no one — and their constrained exit pushes their effective burden toward the full-target end. From the beneficiary positions the same rule is experienced as unconditional security, most acutely by those (newborns, the cognitively impaired) who could hold no negotiating position under any alternative reading. The agenda-setter seat experiences the arrangement as legitimate order it administers at real fiscal cost, and the excluded dissident seat experiences it as a settled dogma it may attack but never vote on. The engine computes these divergences from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The three beneficiary declarations drive the protected seats toward the subsidy end: all_born_humans receives categorical protection diffusely, and the two acute subclasses receive it without contributing anything — their d sits near the beneficiary pole, amplified toward it further by trapped exit (they cannot arbitrage their status). parents_of_severely_impaired_newborns is the declared victim class and carries high d: concentrated, inescapable cost with constrained exit. state_legal_authorities sits near-symmetric — they collect order and legitimacy from the rule's stability while paying enforcement costs, and their arbitrage-grade control over the threshold keeps them from target-position exposure. No directionality overrides are authored: the derivation from beneficiary/victim declarations plus exit options reproduces these relationships without correction, and an override keyed only to power atoms would misapply a single correction across heterogeneous agents sharing an atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: newborn dependence is permanent, and the discretion the arrangement removes regenerates in every generation and every jurisdiction with weak enforcement — so no obsolescence flag is warranted, and the live founding_problem_status paired with a world_rearranges disappearance verdict is the aligned configuration the mismatch consumer expects (no zombie flag). Classifying this as rope guards against the symmetrical errors: reading the arrangement as pure extraction (which would erase the categorical protection that is its entire output and misread families' concentrated costs as collected rents — no seat collects them, hence gain_flow is diffuse) or as a natural fact (which would erase its constructed, historically contested character — emerges_naturally is false, and the threshold-placement omega documents the residue of ambiguity). The forward-looking risk this story flags is degradation rather than decay: care-burden concentration is the mechanism by which a broadly beneficial coordination arrangement could acquire the asymmetric cost structure of a hybrid form, and the concentration omega plus the slowly rising extractiveness series exist to catch that transition if it arrives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'How would the victim set and computed classification change if this kernel were instantiated as the fitness-contingent or potential-based reading instead of this birth-threshold reading?',
    'Generate the two sibling stories (personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading) and compare computed per-seat types and victim sets against this file; the declared structural delta predicts that newborns failing fitness or potential tests move from the protected set into an identifiable victim class.',
    'Under the sibling readings, currently-protected newborns become exclusion targets and the arrangement plausibly computes as snare or tangled_rope rather than rope; this file''s rope classification holds only within the birth reading''s categorical premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Reading-indexed classification: the same kernel yields structurally different constraints per reading.').

omega_variable(
    birth_threshold_naturalness,
    'Is the moral salience of birth a discovered feature of human development or an administrable convention selected for observability?',
    'Cross-cultural comparison of candidate thresholds (quickening, viability, birth, naming rites) testing whether any placement tracks a genuine discontinuity in morally relevant capacities rather than administrative convenience and detectability.',
    'If purely conventional, the arrangement is a maintained coordination choice with no natural-limit character; if the threshold tracks a real developmental discontinuity, parts of the structure acquire mountain-like features, changing how certification treats its immutability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(birth_threshold_naturalness, conceptual, 'Natural law versus constructed convention ambiguity at the heart of the threshold''s placement.').

omega_variable(
    nicu_withdrawal_drift,
    'Does neonatal end-of-life practice (withdrawal of intensive treatment in gravely impaired newborns) diverge systematically from the categorical rule such that a de facto quality-of-life threshold already operates?',
    'Comparative audit of neonatal intensive-care withdrawal decisions against homicide-law application across jurisdictions with differing protocols, including regulated neonatal-euthanasia frameworks, testing whether formal and bedside lines coincide.',
    'Systematic divergence would raise theater_ratio above its authored low value and signal practice_drift within this reading''s own codification — the categorical rule persisting formally while a conditional rule operates clinically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nicu_withdrawal_drift, empirical, 'Formal-rule versus bedside-practice gap at the margin of the categorical threshold.').

omega_variable(
    care_burden_concentration,
    'Are the costs of the categorical rule concentrating on a narrow class — families of severely impaired children — as neonatal medicine extends survival?',
    'Longitudinal caregiver-burden and lifetime-care-cost studies correlated with neonatal intensive-care technology diffusion across decades.',
    'Growing concentration on a constrained minority is the classic degradation vector by which broadly beneficial coordination acquires asymmetric cost-bearing; it would raise measured extractiveness for the payer seat and pressure the arrangement toward hybrid coordination-and-cost dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_burden_concentration, empirical, 'Concentration of the arrangement''s residual costs as the technology frontier extends impaired-infant survival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__birth_threshold_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(pers_tr_t20, observed).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__birth_threshold_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement_basis(pers_tr_t40, observed).
narrative_ontology:measurement(pers_tr_t60, personhood_boundary__birth_threshold_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement_basis(pers_tr_t60, observed).
narrative_ontology:measurement(pers_tr_t80, personhood_boundary__birth_threshold_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement_basis(pers_tr_t80, observed).
narrative_ontology:measurement(pers_tr_t100, personhood_boundary__birth_threshold_reading, theater_ratio, 100, 0.09).
narrative_ontology:measurement_basis(pers_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__birth_threshold_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement_basis(pers_be_t20, observed).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__birth_threshold_reading, base_extractiveness, 40, 0.13).
narrative_ontology:measurement_basis(pers_be_t40, observed).
narrative_ontology:measurement(pers_be_t60, personhood_boundary__birth_threshold_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement_basis(pers_be_t60, observed).
narrative_ontology:measurement(pers_be_t80, personhood_boundary__birth_threshold_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement_basis(pers_be_t80, observed).
narrative_ontology:measurement(pers_be_t100, personhood_boundary__birth_threshold_reading, base_extractiveness, 100, 0.16).
narrative_ontology:measurement_basis(pers_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__birth_threshold_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement_basis(pers_su_t20, observed).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__birth_threshold_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement_basis(pers_su_t40, observed).
narrative_ontology:measurement(pers_su_t60, personhood_boundary__birth_threshold_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement_basis(pers_su_t60, observed).
narrative_ontology:measurement(pers_su_t80, personhood_boundary__birth_threshold_reading, suppression_requirement, 80, 0.36).
narrative_ontology:measurement_basis(pers_su_t80, observed).
narrative_ontology:measurement(pers_su_t100, personhood_boundary__birth_threshold_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement_basis(pers_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the personhood boundary' covers three structurally distinct claims with different victim sets and different epsilon values, authored as three files linked by affects_constraints. This file (birth-threshold reading) is the codified baseline — categorical inclusion of all born humans, no excluded-newborn victim class, low extraction. The fitness-contingent and potential-based siblings are narrower: each introduces an evaluative criterion that excludes some born newborns, creating identifiable victims among exactly the population this file protects, and each therefore carries substantially higher extraction. Upstream/downstream structure runs from this reading to the siblings in argumentative terms — sibling proposals cite the same statutes and clinical practices this reading administers while attacking its categorical premise — but the logical relations are foreclosure (see cs_structure.reading_relations): no single framework can hold that every born human has unconditional standing together with any criterion that strips standing from born humans.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
