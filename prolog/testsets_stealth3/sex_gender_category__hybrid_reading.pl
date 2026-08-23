% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Medical Gatekeeping Model of Sex/Gender Category Membership (Hybrid Reading)
 *   domain: social ontology/legal classification
 *
 * SUMMARY:
 *   The colloquial question 'what determines sex/gender category membership?'
 *   decomposes, per the epsilon-invariance principle, into three structurally
 *   distinct constraints sharing one kernel (sex_gender_category): a
 *   birth-immutability reading, a self-identification reading, and this
 *   file's hybrid reading — membership determined by the combination of
 *   biological facts and medically certified social transition. This story
 *   instantiates ONLY the hybrid reading as it actually operates: diagnostic
 *   manuals and standards of care define a qualifying pathway; specialized
 *   clinics administer assessments and issue the letters that legal
 *   recognition systems require; statutes tie corrected documents to clinical
 *   sign-off. Its epsilon referent is the standing gatekeeping arrangement
 *   itself, assessed by the hybrid reading's own lights — which concede real
 *   costs (high gatekeeping burdens, exclusion of those who do not or cannot
 *   transition, authority concentrated in medical institutions) alongside a
 *   real administrative function. The sibling readings author different
 *   epsilon over different arrangements and live in separate files linked
 *   through network.affects_constraints. Claim and metrics are independent
 *   authored facts: claimed_type records the structural reading (a genuine
 *   coordination function carrying asymmetric extraction under active
 *   enforcement); the metric values record descriptive operation, untuned to
 *   any predicted engine output. KEY AGENTS (by structural relationship): -
 *   psychiatric_and_endocrine_professional_bodies: Agenda-setting beneficiary
 *   (institutional/identity_locked) — authors the criteria and derives
 *   professional standing from administering them -
 *   gender_clinic_practitioners: Agenda-setting beneficiary
 *   (institutional/constrained) — runs the assessments, collects the fees,
 *   issues the decisive letters -
 *   legally_recognized_transitioned_individuals: Beneficiary/payer
 *   (moderate/identity_locked) — holds recognition purchased through the gate
 *   - non_transitioning_trans_individuals: Primary target (powerless/trapped)
 *   — bears total exclusion from recognition -
 *   trans_people_denied_or_delayed_transition_access: Target
 *   (powerless/trapped) — bears queue, cost, and discretion burdens inside
 *   the pathway - sex_segregated_institution_operators: Beneficiary
 *   (institutional/constrained) — consumes the certificate as an
 *   administrable placement criterion - cis_category_stakeholders: Incidental
 *   beneficiary (organized/mobile) — inherits a stable category system at low
 *   personal cost - demedicalization_advocates: Excluded voice
 *   (organized/constrained) — contests the premise from outside the
 *   criteria-writing rooms - human_rights_treaty_bodies: Analytical observer
 *   (institutional/analytical) — reviews and condemns, enforces nothing
 *   directly
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.64).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.65).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Medical Gatekeeping Model of Sex/Gender Category Membership (Hybrid Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social ontology/legal classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'db23bfcd-1dfe-49a5-bd96-ca4929a734f7').
narrative_ontology:cs_kernel_codification('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', formalized).
narrative_ontology:cs_authority_grounding('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', expertise).
narrative_ontology:cs_interpretation_layer_present('db23bfcd-1dfe-49a5-bd96-ca4929a734f7').
narrative_ontology:cs_reading_relation('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_axiom('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', foundational, category_membership_is_clinically_certifiable).
narrative_ontology:cs_axiom_status(category_membership_is_clinically_certifiable, holdable).
narrative_ontology:cs_axiom_grounding('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', category_membership_is_clinically_certifiable, instrumental).
narrative_ontology:cs_axiom('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', foundational, transition_adequacy_is_clinically_assessable).
narrative_ontology:cs_axiom_status(transition_adequacy_is_clinically_assessable, holdable).
narrative_ontology:cs_axiom_grounding('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', transition_adequacy_is_clinically_assessable, empirically_contingent).
narrative_ontology:cs_reference_frame('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', medical_gatekeeping_regime).
narrative_ontology:cs_drift_state('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', post_depathologization_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('db23bfcd-1dfe-49a5-bd96-ca4929a734f7', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, psychiatric_and_endocrine_professional_bodies).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, gender_clinic_practitioners).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, legally_recognized_transitioned_individuals).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, sex_segregated_institution_operators).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cis_category_stakeholders).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_people_denied_or_delayed_transition_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, legally_recognized_transitioned_individuals).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, clinical_gatekeeping_doctrine).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, diagnosable_transsexualism_premise).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, regret_prevention_rationale).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and periodically revise the diagnostic manuals and standards of care that define who qualifies for gender-affirming treatment and legal recognition. Their committees decide what counts as a qualifying diagnosis, how long assessment or lived experience must last, and which interventions are prerequisites. Revision happens through internal committee processes; outsiders can lobby but cannot set the criteria. The organizations' journals, conferences, and training pipelines are built around administering these criteria, and successive editions have renamed and reframed the diagnoses without surrendering the certifying role.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, psychiatric_and_endocrine_professional_bodies, agenda_setter,
    institutional, generational, identity_locked, global).

% Psychiatrists, psychologists, endocrinologists, and surgeons in specialized services. They conduct the assessments, write the referral letters legal systems require, prescribe hormones, and perform surgeries. Assessment slots, therapy hours, and lifelong hormone monitoring generate service income, and their expert testimony carries weight in courts and tribunals. An individual clinician could retrain into another specialty, but referral networks, publications, and seniority are tied to these services.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gender_clinic_practitioners, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, gender_clinic_practitioners, beneficiary).

% Trans people who completed the required assessments and interventions and now hold corrected documents. They gained legal recognition, and with it safer travel, employment, and facility access. The price was years of assessment, disclosure of intimate history to strangers, and procedures timed to the service's schedule rather than their own. Their recognized status is now woven into their records, relationships, and self-understanding, and there is no parallel route to revisit that status outside the same medical channel.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legally_recognized_transitioned_individuals, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, legally_recognized_transitioned_individuals, payer).

% Trans people who live according to their gender but have not undergone — or refuse — the prescribed medical pathway, whether from cost, health contraindications, age, or rejection of the diagnostic frame. Under the recognition rules they remain registered in their birth category: documents mismatch presentation, and access to sexed facilities, shelters, and sports turns on institutional discretion case by case. There is no application they can file that the current rules would accept.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, trapped, national).

% Trans people who accept the medical pathway and are trying to complete it but are stuck: multi-year waiting lists, geographic gaps in provision, gatekeeper disagreement about readiness, or funding refusals. They are inside the rules but cannot reach the finish line, and each year of delay extends the period in which they carry the costs of transition-seeking without the protections recognition confers.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_people_denied_or_delayed_transition_access, payer,
    powerless, biographical, trapped, national).

% Run prisons, shelters, changing facilities, and sports competitions. They need a workable, defensible rule for placing people in sexed spaces and categories; the medical certificate hands them one that locates the decision in clinical authority rather than their own discretion. Adopting a different criterion on their own initiative would expose them to litigation and political attack from both directions, so they follow the certification system even where its edge cases strain daily operations.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, sex_segregated_institution_operators, beneficiary,
    institutional, generational, constrained, national).

% The broad population whose documents, facilities, and competitions are organized around the existing two-category system. They encounter the recognition rules rarely and at low personal cost, and the certification requirement keeps the pace of category change slow and individually traceable, which many read as orderly administration. Their daily arrangements would survive most reform variants, giving them little reason to organize around the question either way.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cis_category_stakeholders, beneficiary,
    organized, generational, mobile, national).

% Trans-led organizations, allied lawyers, and some clinicians campaigning for recognition based on self-declaration and for moving transition care to informed-consent models. They testify to consultations and litigate, but hold no seat on the committees that write the diagnostic criteria or the standards of care, and several jurisdictions they have won retain the medical requirement for documents issued elsewhere.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, demedicalization_advocates, excluded,
    organized, biographical, constrained, global).

% Regional and international human rights mechanisms that review states' recognition laws, publish findings on compulsory interventions and diagnostic prerequisites, and issue recommendations. They examine the system from outside any single national implementation and can condemn practices, but they enforce nothing directly.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, human_rights_treaty_bodies, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, gender_clinic_practitioners).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an administratively verifiable criterion for assigning people to legal and institutional sex/gender categories: a standardized clinical assessment plus a prescribed sequence of interventions yields a certificate that registries, facilities, and sports bodies can check, resolving cases that birth biology leaves ambiguous without requiring institutions to adjudicate self-reported identity.
% TRANSFER_FUNCTION: Moves decision authority over category membership from individuals to medical institutions; moves assessment fees, therapy hours, disclosure of intimate history, and procedurally timed interventions from trans individuals to the clinical system; returns corrected documents, legal recognition, and protected access to those who complete the pathway.
% ABSENT_VOICES: Non-transitioning trans individuals and demedicalization advocates hold no seat on the committees that write diagnostic criteria or standards of care; intersex people, whose category assignment is governed by the same medical apparatus, are likewise outside the room. All three would contest the premise that legitimate membership requires clinical certification, and their objection is currently recorded only in consultations they can attend but cannot vote in.
% DISAPPEARANCE_RATIONALE: Recognition registries, prison and shelter placement rules, sports eligibility, and thousands of in-progress assessments would lose their operating criterion overnight; jurisdictions would scramble toward either birth-document rules or self-declaration, mid-pathway applicants would be stranded between regimes, and the clinical services built around the pathway would lose their referral role.
% FOUNDING_PROBLEM: Mid-twentieth-century clinicians faced patients requesting cross-sex hormones, surgery, and legal sex change, and institutions had no defensible rule for granting any of it: who genuinely required transition, how to guard against regret and malpractice, and on what evidence a registry could amend a legal category. The gate was built to answer 'who may change category, decided by whom, on what evidence.'
% FOUNDING_PROBLEM_CORROBORATION: Historical case-series literature from the first gender clinics attests the founding problem as then formulated. Contemporary corroboration is split and partly adverse: outcome research and informed-consent practice attest that the pathologizing core of the original problem has been superseded, while some clinical and safeguard literature attests a narrower live residue concerning capacity and regret-risk assessment. No corroborating source outside the benefiting parties attests the current gate as still solving its founding problem in its original form; the strongest external attestations are the human-rights reports documenting the gate's costs.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.64: the pathway prices access in years of assessment, repeated disclosure of intimate history, and procedurally timed irreversible interventions, while excluding outright those who cannot or will not meet it; the hybrid reading's own lights concede these costs. Suppression 0.65: inside gate jurisdictions recognition is legally unavailable by any other route, and documentary mismatch carries facility, employment, and travel exposure; the component is predominantly structural, with an internalized residue routed to the omega variable. Theater 0.30: as demand outran capacity, the diagnostic interview became pro forma for most applicants and the operative filter migrated to queue position and cost — the performative share grew steadily without displacing the real filtering done by scarcity. Accessibility_collapse 0.55: alternatives (self-declaration jurisdictions, social transition without papers, informed-consent care) persist and are visible, but inside a gate jurisdiction the specific alternative 'recognition without medical transition' collapses once the rules are understood. Resistance 0.60: sustained demedicalization campaigning, litigation, and jurisdictional shopping, with the two payer seats demonstrating real coalition capacity through trans-led organizations. Temporal series share one grid (t=0..70, decade steps): extractiveness rose through the pathologization era, peaked under statutory ratchets (sterilization and divorce requirements attached to recognition), eased as depathologization and self-declaration reforms landed, then firmed again as backlash jurisdictions hardened criteria — a drift-with-reversal rather than an oscillation cycle, so no intermittent-reinforcement mechanism is alleged. Suppression_requirement traces the enforcement build-up (statutory embedding of the medical requirement) and then plateaus: the enforcement picture is mature and stable in the late period, which is why that series flattens instead of tracking the extractiveness rebound.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. The two agenda-setting medical seats sit inside a coordination structure they staff and draw income and standing from: from their position the arrangement is a clinical discipline with quality controls. The two powerless payer seats sit outside the finish line — one permanently (no route they can take), one temporarily (queue, cost, gatekeeper discretion) — and from their positions the same structure operates as denial with paperwork. The recognized cohort computes mixed: it holds a status purchased at high cost and biographically entangled with the gate, which is why gate survivors sometimes defend the apparatus that taxed them. Inter-institutionally, the criteria-writing bodies (globally scoped, organizationally fused with the gate) experience revision pressure as existential, the service-delivery clinics (revenue-tied, individually exit-capable) experience it as market risk, and the treaty bodies (analytical, enforcement-free) experience it as reportable fact. Same-level lateral divergence: the two powerless payer seats differ only in their relationship to the pathway — acceptance versus refusal — which the exit atom captures identically as trapped but the situations show as different lives; the two organized seats (general document-holders versus demedicalization advocates) differ in stake, not power. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The criteria-writing bodies and clinic practitioners combine agenda power with collected fees and conferred standing, placing them near the beneficiary pole; segregated-facility operators and the broad document-holding public are incidental beneficiaries with low exposure and mobile or constrained positions. The two victim groups are powerless and trapped — no accepted alternative route exists inside the regime — placing them near the full-target pole. One directionality override corrects the derivation for the recognized cohort: declared beneficiary with identity-locked exit, the naive chain would read near-pure subsidy, but that cohort's position nets sunk gate costs, procedurally timed interventions, and continuing conditionality of status against a real recognition gain; d=0.38 encodes that mixed position. Excluded advocates and the treaty observer feed consensus-provenance and analysis respectively, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate splits. The regret-prevention and pathologization rationale is substantially dead — outcome literature and peer jurisdictions achieve comparable results without it — while an administrability residue (institutions wanting a checkable criterion for sexed placements and documents) remains live. Reading the arrangement as pure extraction would erase the administrability function that registries, prisons, shelters, and sports bodies genuinely consume; reading it as pure coordination would erase the documented exclusion of non-transitioning individuals and the fee-and-authority flows into the clinical sector. The tangled-rope claim holds both halves apart so neither mislabeling passes. On the R5 mismatch consumer: founding_problem_status=contested pairs with disappearance_verdict=world_rearranges, so no zombie flag fires — the arrangement still rearranges the world, but no longer wholly for the reason it was built.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the hybrid reading of the sex_gender_category kernel; what structural features — victim set, beneficiary set, enforcement profile — would change under the biology_reading or identity_reading siblings?',
    'Compare the compiled sibling stories: biology_reading (membership fixed at birth) removes the transition pathway and converts transition-seekers into the primary victim class; identity_reading (self-identification) removes the certification gate, dissolving the medical beneficiary set and shifting contested cases onto safeguard-limited institutions.',
    'Classification is reading-indexed: the same colloquial debate computes as structurally different constraints under different readings; cross-reading comparison must join on kernel_id, never on the colloquial label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    regret_screening_necessity,
    'Does clinical screening causally produce the low regret rates cited in the gate''s favor, or would outcomes be equivalent under informed-consent access — making the assessment apparatus overhead rather than function?',
    'Outcome comparison across jurisdictions and eras with different screening intensities, controlling for cohort effects; natural experiment from established informed-consent services.',
    'If screening is not causal, the gate''s coordination justification collapses toward pure rent and effective extraction on the payer seats rises sharply; if causal, part of the measured cost is the price of the safeguard itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regret_screening_necessity, empirical, 'Whether the gate''s screening function is causally load-bearing or ceremonial.').

omega_variable(
    internalized_gatekeeping_performance,
    'How much of the suppression experienced by trans individuals under the certification regime is structural (denial of recognition, documentary exposure) versus internalized (self-policing of narrative and presentation to satisfy diagnostic expectations)?',
    'Post-regime trajectory: compare distress and narrative-conformity behavior between cohorts recognized under the gate and cohorts recognized under self-declaration regimes; if performance patterns persist after the gate is removed, the internalized component is substantial.',
    'If internalized, effective suppression exceeds the structural measure and persists across reform; the payer seats carry the regime with them after exit, and structural reform alone undercounts the harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_gatekeeping_performance, empirical, 'Structural versus internalized suppression mechanism split for the interpersonal-facing face of the gate.').

omega_variable(
    expertise_vs_extraction_authority,
    'Is the medical profession''s authority over category membership grounded in transferable clinical competence, or in control of the gate itself — such that demedicalization threatens the authority rather than the science?',
    'Track professional-body behavior under demedicalization proposals: whether opposition tracks safety evidence or tracks loss of gate functions (referral-letter monopolies, assessment mandates); compare bodies in jurisdictions that already removed gate functions.',
    'If authority is gate-contingent, the expertise framing is cover and the extraction component is larger than this reading''s own assessment concedes; if competence-grounded, the gate survives reform as a voluntary clinical service rather than a compulsory checkpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_vs_extraction_authority, empirical, 'Whether professional authority over the kernel is competence-based or rent-preserving.').

omega_variable(
    exclusion_vs_cost_extraction_split,
    'Does the measured extraction concentrate in the total exclusion of non-transitioning trans individuals, or in the costs imposed on those pursuing the pathway — and do the two payer seats compute the same constraint?',
    'Per-seat classification output: compare computed type and effective extraction for the two payer seats; sharp divergence indicates the seats experience structurally different arrangements despite sharing the certification requirement.',
    'If the seats diverge, seat-level analysis should treat them as distinct experiential classes within one constraint; if they converge, the victim set is homogeneous and coalition analysis applies directly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_vs_cost_extraction_split, conceptual, 'Seat-level divergence within the victim set of a single epsilon-invariant constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(sex__tr_t0, observed).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(sex__tr_t10, observed).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__hybrid_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(sex__tr_t20, observed).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__hybrid_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(sex__tr_t30, observed).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__hybrid_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement_basis(sex__tr_t40, observed).
narrative_ontology:measurement(sex__tr_t50, sex_gender_category__hybrid_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(sex__tr_t50, observed).
narrative_ontology:measurement(sex__tr_t60, sex_gender_category__hybrid_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(sex__tr_t60, observed).
narrative_ontology:measurement(sex__tr_t70, sex_gender_category__hybrid_reading, theater_ratio, 70, 0.3).
narrative_ontology:measurement_basis(sex__tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(sex__be_t0, observed).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(sex__be_t10, observed).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__hybrid_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(sex__be_t20, observed).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__hybrid_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(sex__be_t30, observed).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__hybrid_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(sex__be_t40, observed).
narrative_ontology:measurement(sex__be_t50, sex_gender_category__hybrid_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(sex__be_t50, observed).
narrative_ontology:measurement(sex__be_t60, sex_gender_category__hybrid_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement_basis(sex__be_t60, observed).
narrative_ontology:measurement(sex__be_t70, sex_gender_category__hybrid_reading, base_extractiveness, 70, 0.64).
narrative_ontology:measurement_basis(sex__be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(sex__su_t0, observed).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__hybrid_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(sex__su_t10, observed).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__hybrid_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(sex__su_t20, observed).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__hybrid_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(sex__su_t30, observed).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__hybrid_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(sex__su_t40, observed).
narrative_ontology:measurement(sex__su_t50, sex_gender_category__hybrid_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement_basis(sex__su_t50, observed).
narrative_ontology:measurement(sex__su_t60, sex_gender_category__hybrid_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement_basis(sex__su_t60, observed).
narrative_ontology:measurement(sex__su_t70, sex_gender_category__hybrid_reading, suppression_requirement, 70, 0.65).
narrative_ontology:measurement_basis(sex__su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'sex/gender category determination' decomposes into three epsilon-distinct stories over one kernel. biology_reading authors epsilon for a birth-immutability arrangement (its casualties are those who transition or need to); identity_reading authors epsilon for a self-declaration arrangement (its contested costs land on safeguard-limited institutions); this hybrid_reading authors epsilon ~0.64 for the medical-gatekeeping arrangement, whose victim set partially overlaps both siblings' — the excluded non-transitioning individual is invisible to the biology reading's frame and central to this one, while the burdened transition-seeker is this reading's admitted cost and the biology reading's target of denial. Upstream/downstream: the diagnostic-manual apparatus upstream of this reading supplies the 'verifiability' argument both siblings argue against; each file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, moderate, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
