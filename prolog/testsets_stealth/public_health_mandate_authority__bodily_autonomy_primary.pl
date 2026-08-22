% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate Authority — Bodily Autonomy Primary Reading
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   The standing arrangement under contest is the public health mandate
 *   regime: school-entry immunization statutes, emergency-order mandates,
 *   employer and healthcare-worker requirements, an exemption-administration
 *   bureaucracy, and an enforcement ladder running from conditional exclusion
 *   to termination or disenrollment. This file instantiates ONE reading of
 *   the contested kernel public_health_mandate_authority — the
 *   bodily_autonomy_primary reading — under which that arrangement is a
 *   categorical violation of bodily sovereignty: no collective benefit,
 *   however real, enters the justification calculus for non-consensual
 *   medical intervention. Epsilon is authored for the standing mandate
 *   arrangement as this reading assesses it (high), never for the arrangement
 *   this reading would prefer; the reading-indexed value sits over a fixed
 *   referent. Constraint-family note: the colloquial label 'public health
 *   mandate' decomposes into three readings with different victim sets and
 *   different epsilon values — this file, public_health_primary (obligation
 *   to protect the vulnerable commons), and proportionality_reading
 *   (sliding-scale legitimacy). The siblings are separate stories linked via
 *   network.affects_constraints; their epsilon values differ because their
 *   assessments of the same referent differ, not because the referent
 *   differs. Claim/metric independence holds: claimed_type is authored from
 *   this reading's structural verdict (snare — persistence through coercion
 *   with the collective-benefit story doing legitimizing work it cannot, on
 *   this reading, legitimately do), while the metrics describe the
 *   arrangement's actual operation; the engine computes per-seat
 *   classifications from the structural data and owns any divergence.
 *
 * KEY AGENTS:
 *   - - unvaccinated_mandate_subjects: Primary target (moderate/constrained) — bears coerced intervention or exclusion; declared victim
 *   - - conscientious_objectors: Identity-locked target subset (moderate/identity_locked) — refusal constitutive of religious/philosophical identity; declared victim
 *   - - public_health_agencies: Agenda setter and institutional collector (institutional/arbitrage) — drafts, administers, enforces; collects compliance and authority
 *   - - mandate_enforcing_employers: Delegated enforcer with dual position (powerful/constrained) — administers the sharp edge, collects liability shift
 *   - - immunocompromised_patients: Protective-externality beneficiary (powerless/trapped) — receives coverage buffer, bears no coercion; EXCLUDED from victim set per this reading
 *   - - public_health_primary_advocates: Aligned beneficiary seat (organized/mobile) — collects the externality, bears zero compulsion; zero-extraction seat per the expected structural delta
 *   - - exemption_denied_applicants: Excluded voice (powerless/constrained) — objection category removed from the decision table
 *   - - courts_constitutional_reviewers: Analytical observer (institutional/analytical) — adjudicates the perimeter from outside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.86).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.86).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate Authority — Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '24084bc4-10cc-4e88-aefd-f0d163636810').
narrative_ontology:cs_kernel_codification('24084bc4-10cc-4e88-aefd-f0d163636810', formalized).
narrative_ontology:cs_authority_grounding('24084bc4-10cc-4e88-aefd-f0d163636810', lineage).
narrative_ontology:cs_interpretation_layer_present('24084bc4-10cc-4e88-aefd-f0d163636810').
narrative_ontology:cs_reading_relation('24084bc4-10cc-4e88-aefd-f0d163636810', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('24084bc4-10cc-4e88-aefd-f0d163636810', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('24084bc4-10cc-4e88-aefd-f0d163636810', foundational, bodily_inviolability_without_consent).
narrative_ontology:cs_axiom_status(bodily_inviolability_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('24084bc4-10cc-4e88-aefd-f0d163636810', bodily_inviolability_without_consent, deontological).
narrative_ontology:cs_axiom('24084bc4-10cc-4e88-aefd-f0d163636810', secondary, collective_benefit_never_trumps_bodily_integrity).
narrative_ontology:cs_axiom_status(collective_benefit_never_trumps_bodily_integrity, holdable).
narrative_ontology:cs_axiom_grounding('24084bc4-10cc-4e88-aefd-f0d163636810', collective_benefit_never_trumps_bodily_integrity, deontological).
narrative_ontology:cs_reference_frame('24084bc4-10cc-4e88-aefd-f0d163636810', inviolable_bodily_sovereignty).
narrative_ontology:cs_drift_state('24084bc4-10cc-4e88-aefd-f0d163636810', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('24084bc4-10cc-4e88-aefd-f0d163636810', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, mandate_enforcing_employers).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_mandate_subjects).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, conscientious_objectors).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, state_police_power_doctrine).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, jacobson_deference_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declined a mandated vaccination and now faces the enforcement ladder: conditional exclusion from workplaces, campuses, and classrooms until compliance, with termination or disenrollment as the terminal step. Some hold documented medical hesitations, some general distrust, some no objection beyond timing preference. Exit looks like giving up the job or enrollment, relocating to a jurisdiction without the requirement, or consenting under pressure — each path costing the most to those with the least savings and mobility.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_mandate_subjects, payer,
    moderate, biographical, constrained, national).

% A subset of refusers whose refusal is bound to religious or philosophical identity: accepting the injection under threat reads to them as betraying a constitutive commitment, not as a prudential tradeoff. They absorb the same exclusions as other refusers but experience compliance as unavailable in a stronger sense — the price of staying whole is losing the job, and some pay it. If the identity frame ever broke — a doctrinal reinterpretation permitting the shot — their practical options would widen overnight.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, conscientious_objectors, payer,
    moderate, biographical, identity_locked, national).

% Draft the order, set the coverage target, define the exemption categories, and run the enforcement referral pipeline through employers and schools. Collect compliance statistics, expanded administrative authority, and budget justification with each mandate cycle. Their officials are not themselves subject to the coercive edge — they can resign, rotate, or reclassify duties without undergoing the intervention they impose. Leaving the arrangement costs the institution nothing; the mandate is its instrument, not its enclosure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Hospitals, school systems, and large firms execute dismissal and exclusion on the state's behalf and for their own liability bookkeeping: a vaccinated roster shifts infection risk, insurance exposure, and negligence arguments off their balance sheet. They did not design the requirement but administer its sharpest edge, and they lobby both for extension (predictable staffing) and for carve-outs (shortages). Refusing to enforce invites regulatory penalty; enforcing invites wrongful-termination suits — so they stay inside the arrangement either way.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, mandate_enforcing_employers, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, mandate_enforcing_employers, beneficiary).

% Cannot mount a full response to vaccination themselves and rely on surrounding coverage for a protective buffer they cannot purchase individually. Every point of community uptake is protection delivered to them. They bear no injection they did not choose and, in this arrangement's terms, owe no reciprocal bodily duty — the protection arrives whether or not they endorse the method. Their position is immobile: the vulnerability travels with the body.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Epidemiologists, bioethicists, and civic organizations who argue the mandate is an obligation owed to the vulnerable commons. Nearly all are vaccinated by choice, so the enforcement machinery never touches them; they collect the same protective externality as everyone else while bearing none of the compulsion. Their funding, publication venues, and standing ride on the arrangement's continuation, but they can and do move attention across outbreaks and causes — the arrangement is their cause, not their condition.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, beneficiary,
    organized, biographical, mobile, national).

% Filed for religious or philosophical exemptions in jurisdictions that answered by eliminating the category outright. Their applications are not pending; the door they knocked on was removed from the wall. They retain standing to sue and little else: no hearing to address, no administrator with discretion to appeal to, no seat in the rulemaking that deleted their option. Their objection circulates in comment threads and protest lines rather than in any decision room.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, exemption_denied_applicants, excluded,
    powerless, biographical, constrained, national).

% Review mandate challenges under precedents that defer to state police powers in emergencies while carving out growing religious-liberty and bodily-integrity exceptions at the margins. They hear evidence from every other seat, issue rulings that narrow or widen the enforcement perimeter, and stand structurally outside the arrangement they adjudicate — no compliance flows to or from them.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, courts_constitutional_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Compels uniform immunization uptake toward coverage thresholds that voluntary consent historically plateaus below, and centralizes the decision to accept medical risk instead of leaving it to each individual — solving the free-rider problem in herd protection by removing the rider's option.
% TRANSFER_FUNCTION: Moves decision-rights over one's own body from individuals to public health authorities and their delegated enforcers (employers, schools); moves compliance from the mandated to the agencies' coverage targets, and where compliance is refused, moves employment and educational access away from the refuser.
% ABSENT_VOICES: Exemption-denied applicants are the clearest absent voice: their objection category was deleted from the decision table rather than weighed in it. More broadly, the unvaccinated sit inside the enforcement perimeter but outside agenda-setting — mandate design happens in agency rulemaking and legislative chambers where their refusal is processed as a compliance problem, never as a veto.
% DISAPPEARANCE_RATIONALE: If the mandate regime vanished overnight, schools and employers would rewrite admission and hiring conditions within weeks, agencies would lose their principal coverage instrument and the administrative apparatus built around it, exemption bureaucracies would dissolve, and an active litigation docket would clear — the voluntary-immunity equilibrium, school attendance law, and occupational health rules would all reorganize around the absence.
% FOUNDING_PROBLEM: Recurrent epidemic disease plus the free-rider problem in immunity: voluntary uptake plateaus below herd thresholds, exposing those who cannot be vaccinated and straining care infrastructure. Mandates were built to close the coverage gap that persuasion alone repeatedly failed to close.
% FOUNDING_PROBLEM_CORROBORATION: Historical epidemiology outside the mandate apparatus corroborates that the founding problem was real — voluntary uptake did plateau below thresholds. Contemporary bioethics and public-comment records outside the benefiting parties attest the problem recurs with novel pathogens while disputing that its liveness licenses non-consensual intervention. No source outside the benefiting parties attests that the problem's persistence settles the legitimacy question; that inference is drawn only inside the arrangement's own beneficiary set.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.86: from this reading's seat the arrangement compels bodily submission or strips livelihood and education — near the top of the scale, and the temporal series shows it climbed there during the 2020-2024 mandate wave (0.44 to a 0.92 peak before partial relaxation to 0.86 as emergency orders expired but school-entry, healthcare, and scattered employer mandates persisted and in places codified). Suppression 0.72: the enforcement machinery is real and hardened into standing structures; alternatives (medical exemptions, relocation, remote work, homeschooling) persist but are costly and jurisdiction-dependent, hence accessibility_collapse 0.55 rather than higher. Theater 0.18: enforcement is overwhelmingly functional; a modest performative layer (compliance signaling, public credential displays) swelled at peak and receded. Resistance 0.78: sustained litigation, protest, and state-level statutory prohibitions on mandate authority — the arrangement is actively fought, not passively absorbed. The suppression_requirement series is authored because enforcement-capacity change IS the traced dynamic here: rapid build-up (0.28 to 0.84), then partial relaxation that nonetheless leaves suppression above its pre-wave baseline (0.72) — normalization of the enforcement apparatus, not its dismantling. All three series share one seven-point grid. Seat divergence: payer seats (unvaccinated, objectors) compute heavy effective burden amplified by constrained and identity-locked exit; beneficiary seats compute near-zero or negative; the public_health_primary_advocates seat computes zero extraction (beneficiary position plus mobile exit, no coercion borne) exactly as the expected structural delta specifies; immunocompromised_patients are declared beneficiaries only — their trapped exit does not push them toward the target range because they bear no coercion cost, and this reading's delta explicitly excludes them from the victim set. Suppression here is predominantly structural (livelihood and access barriers), with a minor internalized component among objectors whose communities treat compliance as defection.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same structure and experience four different arrangements. From the agency seat it is obligation-administration: a coverage instrument with an exemption valve. From the unvaccinated subject's seat it is a bodily ultimatum backed by destitution. From the employer's seat it is liability management with a regulatory whip behind it. From the immunocompromised patient's seat it is a lifeline arriving without any demand on their body. The engine computes a classification per seat from power, exit, and directional position; the authored snare claim records this reading's verdict and does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations (unvaccinated_mandate_subjects, conscientious_objectors) drive d toward the full-target end, amplified by constrained exit for the general refuser population and identity_locked exit for objectors, whose compliance price includes betraying constitutive commitments. Beneficiary declarations drive d toward the subsidized end: agencies (who also set the agenda and face zero institutional cost from the arrangement), enforcing employers (who collect the liability shift), immunocompromised patients (protective externality, no coercion borne), and public_health_primary_advocates (externality collected, compulsion never touched them — the derivation lands this seat at effectively zero extraction, matching the declared structural delta). No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms already produce the intended per-seat relationships, including the deliberate inclusion of the advocates' zero-extraction seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview returns founding_problem_status 'contested' against disappearance_verdict 'world_rearranges' — no dead-problem mismatch flag fires, correctly: the arrangement's function is vigorously disputed, not atrophied, and theater_ratio stays low, so no piton signature is available. This blocks two opposite errors. First, it prevents mislabeling a functioning, actively enforced arrangement as inertial performance — the enforcement ladder does real work on real bodies. Second, it prevents this reading's categorical claim from erasing the recorded coordination function: the metrics keep the genuine coverage-coordination visible even though the reading holds it categorically insufficient to justify the means, which is precisely the discipline that stops a rights-based verdict from quietly rewriting the descriptive record. The mandatrophy question here is not whether the mandate outlived its purpose but whether its purpose can ever authorize its method — a normative contest the classification records without resolving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (bodily_autonomy_primary) of the kernel public_health_mandate_authority; how would the classification change if instantiated under a sibling reading?',
    'Comparative analysis across the three sibling stories (public_health_primary, proportionality_reading) using identical structural data with reading-swapped beneficiary/victim declarations: track victim-set membership, epsilon, and computed type per reading.',
    'Under public_health_primary the unvaccinated leave the victim set, the immunocompromised become the protected center, and epsilon falls toward the coordination floor; under proportionality_reading victim-set membership and epsilon become scale-dependent. The snare verdict is a property of THIS reading''s seat, not of the arrangement independent of all readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: reading-relative classification of a contested kernel.').

omega_variable(
    intervention_scope_boundary,
    'Where does ''non-consensual medical intervention'' begin for this reading — invasive intervention (injection) only, or any bodily imposition including testing, quarantine, and masking requirements?',
    'Doctrinal analysis of the reading''s own litigation posture and philosophical statements: which impositions its advocates challenge as categorical violations versus accept as regulable conduct.',
    'A broad scope pulls testing and quarantine regimes into the victim-generating structure and raises effective epsilon further; a narrow scope confines the categorical claim to injections and lowers the measured footprint of the arrangement this reading condemns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_scope_boundary, conceptual, 'Boundary of the categorical principle within the reading''s own commitments.').

omega_variable(
    coerced_consent_validity,
    'Is consent obtained under threat of job loss or educational exclusion valid consent, such that compliant-but-unwilling subjects fall outside the coerced set?',
    'Survey and interview data on why compliant subjects complied, cross-referenced with bioethical analyses of conditional consent under ultimatum.',
    'If ultimatum-consent counts as consent, the extracted set shrinks to actual refusers and epsilon drops materially; if it does not, the compliant-under-threat majority joins the victim set and the arrangement''s extraction is far larger than refusal statistics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coerced_consent_validity, empirical, 'Whether the compliant-under-pressure population belongs inside or outside the coerced set.').

omega_variable(
    voluntary_compliance_floor,
    'What share of the population would comply absent any enforcement — and is the coercive edge therefore extracting from a small residual or from a large unwilling margin?',
    'Jurisdictional natural experiments: uptake in otherwise-similar regions with and without mandates, controlling for outbreak salience and access.',
    'A high voluntary floor means the enforcement apparatus primarily threatens a small minority (sharper but narrower extraction); a low floor means it compels the many (broader extraction, and the coordination story carries more of the load — moving the structural reading toward hybrid territory even on this reading''s own account).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_floor, empirical, 'Size of the population the coercive edge actually acts upon.').

omega_variable(
    immunocompromised_dual_position,
    'Is the exclusion of immunocompromised patients from the victim set stable, given that some members of the group publicly reject the protection-as-justification frame and oppose mandates on autonomy grounds?',
    'Position data from immunocompromised advocacy organizations and patient surveys distinguishing protection-preference from method-endorsement within the group.',
    'If the group''s self-understanding splits durably, the beneficiary declaration becomes unstable — a subgroup would carry both protective externality and coercion harm, complicating the directionality derivation and partially reopening the victim set this reading''s delta closes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_dual_position, conceptual, 'Stability of the beneficiary assignment for the group the mandate most protects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phma_bap_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(phma_bap_tr_t0, observed).
narrative_ontology:measurement(phma_bap_tr_t8, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(phma_bap_tr_t8, observed).
narrative_ontology:measurement(phma_bap_tr_t16, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 16, 0.16).
narrative_ontology:measurement_basis(phma_bap_tr_t16, observed).
narrative_ontology:measurement(phma_bap_tr_t24, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(phma_bap_tr_t24, observed).
narrative_ontology:measurement(phma_bap_tr_t32, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(phma_bap_tr_t32, observed).
narrative_ontology:measurement(phma_bap_tr_t40, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(phma_bap_tr_t40, observed).
narrative_ontology:measurement(phma_bap_tr_t48, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 48, 0.18).
narrative_ontology:measurement_basis(phma_bap_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(phma_bap_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(phma_bap_be_t0, observed).
narrative_ontology:measurement(phma_bap_be_t8, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(phma_bap_be_t8, observed).
narrative_ontology:measurement(phma_bap_be_t16, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 16, 0.78).
narrative_ontology:measurement_basis(phma_bap_be_t16, observed).
narrative_ontology:measurement(phma_bap_be_t24, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 24, 0.88).
narrative_ontology:measurement_basis(phma_bap_be_t24, observed).
narrative_ontology:measurement(phma_bap_be_t32, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 32, 0.92).
narrative_ontology:measurement_basis(phma_bap_be_t32, observed).
narrative_ontology:measurement(phma_bap_be_t40, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 40, 0.84).
narrative_ontology:measurement_basis(phma_bap_be_t40, observed).
narrative_ontology:measurement(phma_bap_be_t48, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 48, 0.86).
narrative_ontology:measurement_basis(phma_bap_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(phma_bap_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(phma_bap_su_t0, observed).
narrative_ontology:measurement(phma_bap_su_t8, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(phma_bap_su_t8, observed).
narrative_ontology:measurement(phma_bap_su_t16, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(phma_bap_su_t16, observed).
narrative_ontology:measurement(phma_bap_su_t24, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 24, 0.8).
narrative_ontology:measurement_basis(phma_bap_su_t24, observed).
narrative_ontology:measurement(phma_bap_su_t32, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 32, 0.84).
narrative_ontology:measurement_basis(phma_bap_su_t32, observed).
narrative_ontology:measurement(phma_bap_su_t40, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(phma_bap_su_t40, observed).
narrative_ontology:measurement(phma_bap_su_t48, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 48, 0.72).
narrative_ontology:measurement_basis(phma_bap_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'public health mandate' into three readings of one kernel (public_health_mandate_authority), per the epsilon-invariance principle: the label conflates structurally distinct claims with different victim sets and different epsilon values. This file instantiates bodily_autonomy_primary (categorical prohibition; unvaccinated in the victim set, immunocompromised excluded from it, zero extraction on public-health-primary advocates). public_health_primary inverts the victim structure (obligation to the vulnerable commons; epsilon near the coordination floor). proportionality_reading makes both victim-set membership and epsilon outputs of a sliding scale. Each story carries its own stable epsilon over the same standing arrangement; the edges here record the family linkage, and the upstream lineage precedent (jacobson_deference_precedent, vindicated by the arrangement's operation) is cited as evidence by the sibling readings' opponents and proponents alike.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
