% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender-Identity Criterion for the 'Woman' Category (Self-Identification Reading)
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This story authors the gender-identity reading of the woman_category
 *   kernel: the rule that category membership follows avowed identity
 *   regardless of assigned sex, as instantiated in self-ID gender-recognition
 *   law, identity documents, sports eligibility, and admission to single-sex
 *   services. The epsilon referent is the standing arrangement under contest
 *   — the identity-based membership rule as it actually operates — assessed
 *   by this reading's own lights: the reading endorses the criterion, yet
 *   acknowledges that in sport and intimate spaces the rule imposes real,
 *   concentrated costs on birth-sex-defined women, which is why epsilon is
 *   moderate-high rather than near-zero. The claim/metric gap is deliberate
 *   and independent: claimed_type is tangled_rope because the arrangement
 *   possesses a genuine, substantial coordination function (gatekeeping-free
 *   recognition) alongside asymmetric extraction requiring active
 *   enforcement; the metrics describe that mixed operation without being
 *   tuned to any predicted engine output. Per the family discipline, the
 *   sibling readings (sex_biology_reading, intersex_accommodation_reading)
 *   are separate constraint files with their own epsilon and victim sets;
 *   this file neither imports their content nor hedges across them.
 *
 * KEY AGENTS:
 *   - - transgender_women: Primary beneficiary (moderate/identity_locked) — receive recognition, documents, access, and anti-discrimination standing
 *   - - gender_nonconforming_people: Secondary beneficiary (moderate/constrained) — shielded from anatomical gatekeeping
 *   - - female_athletes: Primary target in the sports domain (organized/constrained) — compete for finite slots under the opened criterion
 *   - - women_in_sex_segregated_facilities: Primary target in the intimate-spaces domain (powerless/trapped) — housed and served under identity-based admission
 *   - - gender_critical_feminist_organisations: Target via enforcement's speech and association costs (organized/identity_locked)
 *   - - legislators_and_ministers: Agenda setter (institutional/mobile) — enact, amend, reverse
 *   - - courts_and_equalities_regulators: Agenda setter (institutional/constrained) — adjudicate which criterion operates
 *   - - sports_governing_bodies: Agenda setter (institutional/constrained) — set eligibility under cross-pressure
 *   - - single_sex_service_providers: Operational agenda setter with payer residue (institutional/constrained) — administer and absorb friction
 *   - - survivors_of_male_violence: Excluded voice (powerless/trapped) — dependent on the contested services, absent from the deliberations
 *   - - bioethics_and_legal_scholars: Analytical observer (analytical/analytical) — maps the structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.6).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.68).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender-Identity Criterion for the 'Woman' Category (Self-Identification Reading)").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, 'df596e10-1cab-4149-952c-19cb58e570e3').
narrative_ontology:cs_kernel_codification('df596e10-1cab-4149-952c-19cb58e570e3', distributed).
narrative_ontology:cs_authority_grounding('df596e10-1cab-4149-952c-19cb58e570e3', distributed).
narrative_ontology:cs_reading_relation('df596e10-1cab-4149-952c-19cb58e570e3', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('df596e10-1cab-4149-952c-19cb58e570e3', woman_category__intersex_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('df596e10-1cab-4149-952c-19cb58e570e3', foundational, self_declared_identity_is_authoritative_for_category_membership).
narrative_ontology:cs_axiom_status(self_declared_identity_is_authoritative_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('df596e10-1cab-4149-952c-19cb58e570e3', self_declared_identity_is_authoritative_for_category_membership, deontological).
narrative_ontology:cs_axiom('df596e10-1cab-4149-952c-19cb58e570e3', secondary, anatomical_gatekeeping_of_category_membership_is_discriminatory).
narrative_ontology:cs_axiom_status(anatomical_gatekeeping_of_category_membership_is_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('df596e10-1cab-4149-952c-19cb58e570e3', anatomical_gatekeeping_of_category_membership_is_discriminatory, deontological).
narrative_ontology:cs_reference_frame('df596e10-1cab-4149-952c-19cb58e570e3', self_identification_membership_framework).
narrative_ontology:cs_drift_state('df596e10-1cab-4149-952c-19cb58e570e3', contemporary_policy_backlash, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('df596e10-1cab-4149-952c-19cb58e570e3', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_nonconforming_people).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_athletes).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, women_in_sex_segregated_facilities).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, gender_critical_feminist_organisations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, single_sex_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under legal and social categories assigned at birth that do not match their identity. Under this arrangement they obtain gender-recognition certificates, passports, and records matching their identity without psychiatric diagnosis or surgical prerequisite, gain admission to women's services and spaces consistent with that recognition, and hold standing in anti-discrimination proceedings as women. Their stake in the criterion is constitutive: reverting to birth-sex categorization is precisely the outcome the arrangement exists to end, so exit from the arrangement is not a meaningful option from where they stand.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, global).

% People whose presentation or identity does not fit binary expectations benefit from a membership criterion that requires no anatomical conformity and no medical certification. They gain document flexibility and reduced scrutiny in everyday administrative encounters. Their direct stake is smaller than that of transgender women, but the same criterion shields them from gatekeeping they would otherwise face.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_nonconforming_people, beneficiary,
    moderate, biographical, constrained, global).

% Compete for places, podium finishes, records, scholarships, and livelihoods inside a female category whose admission criterion, under this arrangement, is identity rather than birth sex. Where eligibility opens, they compete for finite slots against athletes who experienced male puberty. Their alternatives are accepting the criterion, moving to open categories where these exist, or leaving competitive sport; none preserves the category they trained within. Collective voice exists through athlete associations but has limited purchase over federation rules set under litigation and sponsorship pressure.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_athletes, payer,
    organized, biographical, constrained, global).

% Women in prisons, refuges, hospital wards, and changing rooms are housed or served under policies that admit anyone who identifies as a woman. Prisoners cannot leave the estate; refuge users arrive at a moment of crisis with few alternatives. Their recourse runs through complaint channels administered by the same institutions applying the admission policy, and their circumstances permit little collective organization.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, women_in_sex_segregated_facilities, payer,
    powerless, immediate, trapped, national).

% Campaign for sex-based provisions and the birth-sex criterion. Under this arrangement their public position carries professional and platform costs: affiliated individuals report employment consequences, campaign charities face funder and regulator scrutiny, and major institutions treat their framing as beyond acceptable speech. Their commitment is constitutive of their political identity, so absorbing these costs and continuing is the only course available to them; abandoning the commitment would dissolve the organizations themselves.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_critical_feminist_organisations, payer,
    organized, generational, identity_locked, global).

% Enact or amend gender-recognition and equality law, issue or withdraw guidance on single-sex exceptions, and respond to coalition pressure from both directions. They can change statutory criteria and have done so in opposite directions across jurisdictions and across the interval; electoral cycles discipline how far and how fast they move.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, legislators_and_ministers, agenda_setter,
    institutional, biographical, mobile, national).

% Interpret whether 'woman' in statutes means self-identified gender or birth sex, adjudicate eligibility and service-admission disputes, and publish guidance that binds service providers. They do not legislate anew, but their rulings decide which criterion operates in practice; reversal pressure reaches them as new cases, appeals, and revised statutory instruction.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, courts_and_equalities_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Set and periodically revise eligibility rules for female categories under simultaneous litigation risk, sponsor pressure, athlete objection, and unresolved science on retained advantage. Whichever criterion they adopt draws challenge from the constituency it excludes; their discretion is real but bounded by courts, umbrella federations, and national legislation.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Prison services, refuge networks, hospitals, and leisure operators apply admission and housing rules under funding conditions and equality obligations. They absorb the day-to-day friction of the arrangement: complaints from service users on both sides, staff training burdens, incident management, and legal exposure wherever guidance is ambiguous. They administer the criterion more than they chose it.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, single_sex_service_providers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, single_sex_service_providers, payer).

% Women whose need for female-only crisis services stems directly from violence by men. Consultations on admission criteria reach them through advocacy intermediaries or not at all; they are rarely seated in the deliberations that set the rules for the services they depend on at the moment of greatest vulnerability.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, survivors_of_male_violence, excluded,
    powerless, biographical, trapped, national).

% Analyze the category question across law, ethics, and biology: what makes a criterion for 'woman' coherent, what turns on membership in sporting, custodial, and intimate settings, and how competing criteria allocate costs across populations. They collect nothing from the arrangement and bear none of its costs; they map its structure.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, bioethics_and_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single administrable criterion for who counts as a woman across documents, services, and eligibility rules: membership follows avowed identity, so recognition requires no psychiatric diagnosis, no anatomical inspection, and no gatekeeping panel. It solves the problem that birth-sex criteria misclassify transgender people and subject them to intrusive verification for ordinary administrative life.
% TRANSFER_FUNCTION: Moves recognition goods — legal status, document accuracy, service access, category eligibility, and the standing attached to being counted as a woman — to people whose identity differs from their assigned sex. The corresponding costs fall on those for whom the previous boundary had value: competitive places in female sport, the exclusivity of certain intimate spaces, and the institutional simplicity of an anatomy-checked criterion.
% ABSENT_VOICES: Survivors of male violence dependent on single-sex crisis services enter policy debates only through intermediaries; detained women are consulted, where at all, through the institutions detaining them; grassroots athletes without unions or legal representation are absent from eligibility rule-making; in several jurisdictions the criterion was settled by court ruling or ministerial guidance without any legislative hearing for affected women.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, gender-recognition regimes, passport and record systems, sports eligibility rules, prison housing policy, and service admission rules would each revert to the last birth-sex criterion they used; hundreds of thousands of recognized statuses would be invalidated; litigation would restart immediately in every domain. The arrangement is load-bearing across administrative, sporting, and custodial systems.
% FOUNDING_PROBLEM: Transgender people were legally and socially classified by birth sex: identity documents mismatched lived identity, changing legal sex required psychiatric diagnosis and often sterilization or surgery, and services, sport, and record-keeping used anatomy-based checks that excluded them from the category matching their lives.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: clinical and psychiatric literature documents the harms of forced categorization mismatch and of gatekeeping regimes; national statistical offices record the administrative burden of document mismatch; Council of Europe and UN treaty-body reporting documents discrimination against transgender people independently of beneficiary advocacy. Caveat stated plainly: the strongest corroborating institutions now also administer or endorse the arrangement, so their independence is partial and declining over the interval.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.60 reflects the reading's own accounting: the recognition core (documents, legal status) is low-cost and genuinely coordinative, but the contested applications concentrate real asymmetric costs — competitive slots are zero-sum, intimate-space boundaries carry felt safety and privacy costs, and enforcement increasingly reaches speech and association — pulling the blended value to moderate-high. Suppression 0.68 is a raw structural property, unscaled by power or scope: the rival criterion is not criminalized anywhere, but it is penalized through employment consequences, funding scrutiny, regulatory guidance, and institutional speech norms, and the suppression series shows the enforcement machinery maturing over the interval. Theater 0.25: awareness signaling and institutional statement-making are real but a minority of activity; document changes and eligibility rulings are functional. Accessibility collapse 0.5: within adopting institutions the alternatives collapse almost entirely, but across jurisdictions the birth-sex criterion persists and has recently regained ground, so collapse is partial and uneven. Resistance 0.7: organized, litigated, and legislatively effective in several jurisdictions. The measurement series run on one shared time grid (2012–2026, seven points, all three metrics at every point); 2026 values are authored projections of the current state, marked projected. The identity_coordination declaration carries the standard gaming alert: identity framing is a common cover for extraction, and here the framing is genuine at the criterion level while the extraction risk lives in the applications — which is exactly the split the omegas trace.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the transgender_women seat the arrangement is liberating coordination: it removes gatekeeping they experienced as misclassification, and their identity-lock means no policy swing registers as exit. From the female_athletes and women_in_sex_segregated_facilities seats the same structure operates as uncompensated transfer of scarce goods — slots, safety, exclusivity — with constrained or trapped exit amplifying effective extraction. From the gender_critical_feminist_organisations seat the structure extracts through enforced silence as much as through allocation. Agenda-setter seats (legislators, courts, federations, providers) experience administration and cross-pressure rather than either benefit or extraction, with providers carrying a payer residue of compliance friction. Coalition potential for the powerless seats is real but thin: detained women cannot organize, and refuge users are transient, so the trapped seats compute the highest effective extraction with the least capacity to act on it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: transgender_women sit near the full-beneficiary end (the arrangement subsidizes them by design, and identity-lock anchors them there); gender_nonconforming_people sit slightly higher with weaker stakes. Declared victims map to high directionality, amplified by exit: women_in_sex_segregated_facilities (powerless, trapped) sit nearest the full-target end; female_athletes (organized but constrained) sit high with somewhat more leverage; gender_critical_feminist_organisations (organized, identity_locked) sit high through the enforcement channel even though their material grievance is positional rather than allocative. Agenda setters derive mixed or fallback directionality: legislators are mobile enough to sit near symmetric; courts and federations are constrained administrators near symmetric; single_sex_service_providers tilt slightly toward target through their payer residue. Scope effects: the arrangement operates nationally but the normative contest and elite sport are global, so verification difficulty and extraction amplification rise at the global-scoped seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical misreadings. Reading the arrangement as pure rope — pure liberation with no losers — erases the documented collision in sport and intimate spaces and converts a live distributive conflict into a settled moral fact. Reading it as pure snare — a cover story, as its fiercest critics allege — erases the genuine coordination function (gatekeeping-free recognition, document accuracy, safety for a persecuted minority) that motivated the founding problem and still does work every day. Tangled rope holds both: real coordination, real asymmetric extraction, active enforcement required to hold the structure against resistance from the paying seats. On obsolescence: the founding problem (trans exclusion under birth-sex classification) is still live and independently corroborated, so this is not a mandatrophy case — the arrangement has not outlived its function; the open question is whether its function and its extraction can be separated by domain, which is what the epsilon_domain_gradient_decomposition omega tests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positionality,
    'This constraint is one reading of the woman_category kernel (reading: gender_identity_reading). What would change structurally if a sibling reading (sex_biology_reading, intersex_accommodation_reading) governed instead?',
    'Author the sibling stories as separate files and compare computed classifications: victim sets, epsilon, and per-seat types should shift substantially (under the biology reading the excluded set centers on transgender women; under this reading the cost-bearing set centers on birth-sex-defined women in high-stakes domains).',
    'Every classification output of this story is conditional on the reading; the sibling files instantiate different constraints with different epsilon and victim sets, not alternative measurements of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positionality, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings are other constraints.').

omega_variable(
    disagreement_location_criterion_vs_application,
    'Is the disagreement between readings located in the membership criterion itself (whose avowal or anatomy is authoritative) or primarily in the high-stakes application domains (sport, custody, intimate spaces) where the criterion''s costs concentrate?',
    'Compare sibling-reading classifications domain by domain: if the readings converge in low-stakes administrative domains and diverge only in sport and intimate spaces, the live contest is over application, not the criterion.',
    'If the disagreement is mostly applicational, domain-partitioned hybrid arrangements become stable equilibria and this reading''s foreclosure relations soften in practice; if criterial, no partition stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_criterion_vs_application, conceptual, 'Where the kernel contest is actually located: criterion level or application level.').

omega_variable(
    epsilon_domain_gradient_decomposition,
    'The reading authors a single epsilon (0.60), but identity-document applications carry low extraction while sports-eligibility and intimate-space applications carry high extraction. Are these one constraint or a family requiring decomposition?',
    'Per-domain epsilon assessment: if document-policy epsilon sits below ~0.3 while sports/custody epsilon sits above ~0.7, decompose into per-domain stories sharing the criterion and linked via network.affects_constraints.',
    'A single-story classification blends a near-rope recognition core with a high-extraction periphery; decomposition would let each domain classify on its own structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_domain_gradient_decomposition, empirical, 'Whether the domain gradient in extraction costs forces decomposition of this reading into per-domain constraint stories.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of the rival reading structural (employment consequences, charity-funding scrutiny, regulatory guidance treating the rival framing as unprotected) or internalized (self-censorship, preference falsification among institutions and individuals who privately doubt the criterion)?',
    'Post-deregulation trajectory: in jurisdictions that reversed or loosened enforcement (post-2025 rulings and executive actions), track whether dissenting expression surges. A surge indicates a large internalized component carried during the enforcement era; flat expression indicates the suppression was mostly structural and lifted with the sanctions.',
    'If largely internalized, the constraint''s durable suppressive force exceeds the structural measure and persists after formal reversal; if structural, reversal restores the pre-constraint expressive equilibrium quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the suppression enforcing this reading.').

omega_variable(
    dual_identity_lock_stability,
    'Both principal contending seats are identity-locked: transgender women are locked into the category-membership claim itself, and gender-critical feminists are locked into the rival commitment that constitutes their feminism. Does dual identity-lock make the contest irresolvable by evidence or experience?',
    'Track position migration under sustained counter-evidence and counter-experience across a cohort window: measurable conversion in either seat would indicate the locks are softer than declared.',
    'If both locks hold, per-seat classifications stay stable across policy swings and the contest resolves only by jurisdictional sorting rather than persuasion; classification outputs become regime-indexed rather than evidence-indexed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_identity_lock_stability, conceptual, 'Whether mirrored identity-lock on both sides freezes the structural relationship.').

omega_variable(
    competitive_advantage_magnitude,
    'What performance advantage, if any, do transgender women retain in female categories after hormone therapy, by sport and level of competition?',
    'Controlled longitudinal studies of retained anthropometric and performance markers post-transition, disaggregated by sport, puberty exposure, and competitive level.',
    'Determines whether the sports-domain costs of this reading are concentrated in rare elite cases or diffuse across the category, and therefore where the reading''s highest-extraction application actually binds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_advantage_magnitude, empirical, 'Empirical magnitude of retained advantage governing the sports-application cost profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 2012, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2012, woman_category__gender_identity_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement_basis(woma_tr_t2012, observed).
narrative_ontology:measurement(woma_tr_t2015, woman_category__gender_identity_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement_basis(woma_tr_t2015, observed).
narrative_ontology:measurement(woma_tr_t2018, woman_category__gender_identity_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement_basis(woma_tr_t2018, observed).
narrative_ontology:measurement(woma_tr_t2020, woman_category__gender_identity_reading, theater_ratio, 2020, 0.17).
narrative_ontology:measurement_basis(woma_tr_t2020, observed).
narrative_ontology:measurement(woma_tr_t2022, woman_category__gender_identity_reading, theater_ratio, 2022, 0.2).
narrative_ontology:measurement_basis(woma_tr_t2022, observed).
narrative_ontology:measurement(woma_tr_t2024, woman_category__gender_identity_reading, theater_ratio, 2024, 0.23).
narrative_ontology:measurement_basis(woma_tr_t2024, observed).
narrative_ontology:measurement(woma_tr_t2026, woman_category__gender_identity_reading, theater_ratio, 2026, 0.25).
narrative_ontology:measurement_basis(woma_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(woma_be_t2012, woman_category__gender_identity_reading, base_extractiveness, 2012, 0.34).
narrative_ontology:measurement_basis(woma_be_t2012, observed).
narrative_ontology:measurement(woma_be_t2015, woman_category__gender_identity_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement_basis(woma_be_t2015, observed).
narrative_ontology:measurement(woma_be_t2018, woman_category__gender_identity_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement_basis(woma_be_t2018, observed).
narrative_ontology:measurement(woma_be_t2020, woman_category__gender_identity_reading, base_extractiveness, 2020, 0.49).
narrative_ontology:measurement_basis(woma_be_t2020, observed).
narrative_ontology:measurement(woma_be_t2022, woman_category__gender_identity_reading, base_extractiveness, 2022, 0.53).
narrative_ontology:measurement_basis(woma_be_t2022, observed).
narrative_ontology:measurement(woma_be_t2024, woman_category__gender_identity_reading, base_extractiveness, 2024, 0.57).
narrative_ontology:measurement_basis(woma_be_t2024, observed).
narrative_ontology:measurement(woma_be_t2026, woman_category__gender_identity_reading, base_extractiveness, 2026, 0.6).
narrative_ontology:measurement_basis(woma_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2012, woman_category__gender_identity_reading, suppression_requirement, 2012, 0.28).
narrative_ontology:measurement_basis(woma_su_t2012, observed).
narrative_ontology:measurement(woma_su_t2015, woman_category__gender_identity_reading, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement_basis(woma_su_t2015, observed).
narrative_ontology:measurement(woma_su_t2018, woman_category__gender_identity_reading, suppression_requirement, 2018, 0.44).
narrative_ontology:measurement_basis(woma_su_t2018, observed).
narrative_ontology:measurement(woma_su_t2020, woman_category__gender_identity_reading, suppression_requirement, 2020, 0.51).
narrative_ontology:measurement_basis(woma_su_t2020, observed).
narrative_ontology:measurement(woma_su_t2022, woman_category__gender_identity_reading, suppression_requirement, 2022, 0.58).
narrative_ontology:measurement_basis(woma_su_t2022, observed).
narrative_ontology:measurement(woma_su_t2024, woman_category__gender_identity_reading, suppression_requirement, 2024, 0.64).
narrative_ontology:measurement_basis(woma_su_t2024, observed).
narrative_ontology:measurement(woma_su_t2026, woman_category__gender_identity_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(woma_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, womens_sport_eligibility_rules).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, single_sex_service_admission_rules).

% DUAL FORMULATION NOTE:
% The colloquial label 'who counts as a woman' decomposes into at least three structurally distinct constraints — the identity criterion (this file), the biology criterion, and the intersex-accommodation criterion — each with its own epsilon, victim set, and classification; they are separate stories linked by network edges, not one constraint with a measurement parameter. This file instantiates only the gender-identity reading and does not average across readings. Downstream dependents (sport eligibility, service admission) inherit the criterion and carry the highest-stakes collisions; the sibling links run bidirectionally in practice because each reading's legal advances change the operating environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
