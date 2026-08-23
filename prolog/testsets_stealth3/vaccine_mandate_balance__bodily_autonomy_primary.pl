% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: State Medical Compulsion Mandate (Bodily-Autonomy-Primary Reading)
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The standing arrangement under evaluation is the state medical-compulsion
 *   regime: statutes, agency orders, and institutional policies that compel
 *   vaccination — or exclude the non-compliant from employment, schooling,
 *   travel, and public benefits — wherever public-health authorities judge
 *   voluntary coverage insufficient to protect the population. Assessed from
 *   the bodily-autonomy-primary seat, the arrangement coordinates a genuine
 *   collective good: it aligns individual immunization decisions with a
 *   coverage threshold that voluntary systems under-produce, and the
 *   protection this delivers to people who cannot be vaccinated effectively
 *   is real. It also takes something from an identifiable minority:
 *   individuals vaccinated over their objection, or excluded from ordinary
 *   economic and civic life for refusing, surrender decision authority over
 *   their own bodies to the state's coverage targets. Extraction is high
 *   because the mechanism is compulsion applied to bodily integrity — the
 *   domain where this reading holds consent categorical — and suppression is
 *   high because the regime's persistence requires actively maintaining
 *   penalties and closing exits rather than resting on participant
 *   preference. This story is the bodily_autonomy_primary reading of the
 *   vaccine_mandate_balance kernel; the sibling readings are separate
 *   constraint files linked through network.affects_constraints, and the
 *   committer structure — which reading this is, what the siblings would
 *   change, where the disagreement sits — is carried in the omega variables
 *   and the kernel_context note, never reconciled inside this constraint. KEY
 *   AGENTS (by structural relationship): - unvaccinated_coerced_individuals:
 *   primary target (powerless/trapped) — bears fines, exclusion, and
 *   non-consensual intervention - conscientious_objectors: secondary target
 *   (moderate/identity_locked) — identity-bound refusal priced by exclusion -
 *   public_health_authorities: primary beneficiary and agenda-setter
 *   (institutional/constrained) — collects compliance, sets coverage targets
 *   - immunocompromised_high_risk_individuals: protected beneficiary
 *   (moderate/trapped) — receives reduced exposure; residual risk borne as
 *   accepted risk - vaccine_manufacturers: incidental beneficiary
 *   (institutional/arbitrage) — guaranteed demand, insulated from liability -
 *   mandate_administering_institutions: enforcing beneficiary
 *   (organized/constrained) — local enforcement, risk reduction, shifted
 *   political cost - voluntary_compliers: near-costless beneficiary
 *   (moderate/mobile) — free-rides on assured coverage -
 *   constitutional_courts: analytical observer (institutional/analytical) —
 *   defines the legal envelope
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.8).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "State Medical Compulsion Mandate (Bodily-Autonomy-Primary Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '36413bef-e12f-42a9-8684-250450e2164b').
narrative_ontology:cs_kernel_codification('36413bef-e12f-42a9-8684-250450e2164b', formalized).
narrative_ontology:cs_authority_grounding('36413bef-e12f-42a9-8684-250450e2164b', lineage).
narrative_ontology:cs_interpretation_layer_present('36413bef-e12f-42a9-8684-250450e2164b').
narrative_ontology:cs_reading_relation('36413bef-e12f-42a9-8684-250450e2164b', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('36413bef-e12f-42a9-8684-250450e2164b', vaccine_mandate_balance__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('36413bef-e12f-42a9-8684-250450e2164b', foundational, bodily_integrity_requires_affirmative_consent).
narrative_ontology:cs_axiom_status(bodily_integrity_requires_affirmative_consent, holdable).
narrative_ontology:cs_axiom_grounding('36413bef-e12f-42a9-8684-250450e2164b', bodily_integrity_requires_affirmative_consent, deontological).
narrative_ontology:cs_axiom('36413bef-e12f-42a9-8684-250450e2164b', foundational, collective_benefit_cannot_override_consent).
narrative_ontology:cs_axiom_status(collective_benefit_cannot_override_consent, holdable).
narrative_ontology:cs_axiom_grounding('36413bef-e12f-42a9-8684-250450e2164b', collective_benefit_cannot_override_consent, deontological).
narrative_ontology:cs_reference_frame('36413bef-e12f-42a9-8684-250450e2164b', absolute_bodily_self_ownership).
narrative_ontology:cs_drift_state('36413bef-e12f-42a9-8684-250450e2164b', contemporary_mandate_expansion_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('36413bef-e12f-42a9-8684-250450e2164b', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_high_risk_individuals).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_manufacturers).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, mandate_administering_institutions).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, voluntary_compliers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, conscientious_objectors).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, herd_immunity_coverage_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design immunization policy and set coverage targets; issue mandate orders where voluntary uptake falls short, define exemption categories, and direct enforcement through health departments. Collect compliance as coverage statistics and reduced outbreak burden. Bear litigation risk, political backlash, and the administrative cost of running exemption hearings. They cannot walk away from the disease threat their mission is charged with, so abandoning enforcement is not a live option.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Decline vaccination and face the enforcement apparatus: fines in some jurisdictions, termination from employment, exclusion of children from school, denial of entry to venues and borders. The alternative to compliance is forfeiting participation in ordinary economic and civic life, and once the injection is given the intervention cannot be undone. They have no vote in the coverage targets their refusal is measured against.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Refuse on religious or philosophical conviction that the body may not be invaded without consent. Compliance would violate commitments that constitute their identity and community standing, so paying the exclusion price is not experienced as a choice among options. They organize exemption litigation and mutual aid, and their convictions pass across generations.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, conscientious_objectors, payer,
    moderate, generational, identity_locked, national).

% Cannot be vaccinated effectively or face elevated risk from the diseases themselves; they depend on the surrounding population's immunity for protection. Mandates deliver them reduced exposure at no cost to their own bodies. Their residual exposure risk under a consent-respecting regime is carried as accepted risk inherent to a liberty order, not as a harm imposed on them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_high_risk_individuals, beneficiary,
    moderate, biographical, trapped, national).

% Sell vaccines into a market where mandates guarantee demand and liability regimes (indemnification funds, preemption statutes) insulate them from adverse-event claims. They neither set nor enforce the mandates. Their exit is easy: global markets and diversified portfolios mean any single jurisdiction's policy shift costs them little.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Hospitals, universities, employers, and school systems that condition attendance or employment on vaccination. They gain reduced transmission risk and liability cover, and the state absorbs the political cost of compulsion. They bear administrative burden, exemption processing, and some staff attrition. They enforce the policy locally but did not write it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, mandate_administering_institutions, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, mandate_administering_institutions, agenda_setter).

% Would vaccinate anyway; the mandate changes little for them beyond paperwork. They receive the assurance that neighbors cannot free-ride on their compliance, and they pay essentially nothing for that assurance. Exit is trivial because compliance is their default.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, voluntary_compliers, beneficiary,
    moderate, biographical, mobile, national).

% Adjudicate challenges to mandates under the governing constitutional tradition; their doctrine (reasonableness review of public-health measures, compelling-interest tests) defines the legal envelope the arrangement operates in. They collect nothing and pay nothing; they take testimony from every other seat and can redraw the envelope.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the infectious-disease free-rider problem: individuals can capture herd protection while declining vaccination and shifting risk onto others, so voluntary systems under-produce coverage exactly where the medically vulnerable face the greatest exposure. Compulsion aligns individual immunization decisions with a coverage threshold, administered through school-entry, employment-conditioned, and travel-related requirements.
% TRANSFER_FUNCTION: Moves decision authority over bodily intervention from individuals to public-health authorities; moves compliance and residual adverse-event risk onto the coerced minority; moves reduced-exposure protection to the medically vulnerable and to institutions that would otherwise bear transmission risk, and — as guaranteed demand under liability shields — revenue to vaccine manufacturers.
% ABSENT_VOICES: The coerced hold formal but powerless voice: exemption hearings with low grant rates, litigation decided under a reasonableness standard they rarely satisfy, and no seat in the agencies and legislatures where coverage targets are set. The medically vulnerable are heard directly and continuously; the conscientious objector's claim is processed as an exemption request rather than a veto. No seat representing the consent interest as categorical exists inside the mandate-design process — that absence is the structural fact this reading exists to name.
% DISAPPEARANCE_RATIONALE: If compulsion vanished overnight, coverage would fall below threshold in under-vaccinated clusters, exposure risk to the medically vulnerable would rise immediately, and hospitals, employers, and schools would re-impose private requirements to manage their own liability — employment, schooling, and travel would reorganize around private gatekeeping instead of state compulsion. This reading does not deny the rearrangement; it holds the rearrangement's cost must be carried as accepted risk by the protected, not imposed as coerced intervention on the objecting minority.
% FOUNDING_PROBLEM: Recurrent lethal epidemics and the free-rider collapse of voluntary immunization: smallpox, polio, and measles demonstrated that voluntary compliance fails to reach coverage thresholds precisely where exposure threatens those who cannot be vaccinated, and jurisdictions responded by making vaccination a condition of school entry and, later, of employment and travel.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: historical mortality records for smallpox, polio, and measles; contemporary measles-resurgence surveillance; and this reading's own concession — its claim contests the justificatory force of collective benefit, not its existence. Dissenting judges and bioethicists outside public-health agencies attest the founding problem is live while denying that it licenses compulsion; no party to the dispute, including this reading, claims the founding problem is dead.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80 at interval end) because the mandate's mechanism is compulsion applied to bodily integrity and the extracted compliance accrues to identifiable recipients — the public-health apparatus's coverage targets, manufacturers' guaranteed demand. Suppression (0.72) is authored as a raw structural property, unscaled by power or scope in the engine's computation: the regime must actively maintain penalties and exclusion to hold, and it narrows exits over time. Theater ratio (0.30) is moderate-low: enforcement is real, but a growing share of compliance activity is ritual — attestation documentation and symbolic exemption hearings with near-zero grant rates. Accessibility collapse (0.45): alternatives — relocation, private schooling, job change — persist but narrow as more institutions adopt requirements; this is not the near-total collapse of a natural law. Resistance (0.65): litigation, exemption movements, and political backlash are persistent and occasionally effective. The three measurement series share one grid (t = 0..30, indexing the modern mandate-expansion era in five-year steps) and rise together, modeling an enforcement ratchet: mandate scope expands from school entry to workplace to travel, exemptions narrow, penalties escalate. The rising suppression_requirement series is authored deliberately — the story's dynamic is enforcement-capacity growth, not merely extraction drift. Receipt: the extracted compliance demonstrably accrues to the public-health apparatus, so gain_flow names public_health_authorities; the immunocompromised and manufacturers benefit from the arrangement without receiving the extracted good itself. Fixing cost: repeal is administratively trivial for the legislatures and courts that could effect it, but the cost-to-benefit ratio for the fixer is prohibitive — outbreak blame concentrates on whoever repeals while the liberty benefit is diffuse — so fixing_cost is prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. The unvaccinated-coerced (powerless, trapped) and the conscientious objector (identity_locked) sit at the full-target end: for them the arrangement operates as bodily intervention without consent, enforced by penalty. The public-health authorities and mandate-administering institutions sit near the beneficiary end: for them the same structure is a coordination instrument they administer and draw coverage and risk reduction from. Voluntary compliers experience near-zero extraction — they would have complied anyway — while free-riding on the coercion that assures their protection; their exit is trivial precisely because the constraint does not bind them. The immunocompromised are protected beneficiaries under this reading's declarations: their residual exposure risk is carried as risk accepted under a liberty regime, not victimization — a sibling reading would declare them victims instead, and that inversion is carried in the omega variables rather than reconciled here. Coalition capacity among the coerced is weak: refusers are dispersed, individually rational to comply, and their identity-locked core is a small fraction, so nominal coalition power does not translate into leverage against the enforcement machinery.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. public_health_authorities, mandate_administering_institutions, vaccine_manufacturers, immunocompromised_high_risk_individuals, and voluntary_compliers are declared beneficiaries — the arrangement subsidizes them, placing their d near the beneficiary end and damping or inverting effective extraction at their seats. unvaccinated_coerced_individuals and conscientious_objectors are declared victims; trapped exit (the intervention is irreversible and the alternative is forfeiting ordinary life) and identity_locked exit (compliance would violate constitutive conviction) place them near the full-target end, where effective extraction is amplified. Exit structure modulates within roles: the manufacturers' arbitrage-grade exit (global markets, liability shields) places them nearest the beneficiary end despite institutional power, while the immunocompromised are trapped by their medical condition — but trapped-as-beneficiary amplifies subsidy, not extraction. Scope is national: coverage and exemption verification is institutionally mediated, so the engine's scope amplification applies modestly at the payer seats. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct structural relationship for every seat, and no power atom contains two agents the derivation cannot distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification does double work here. Against the public-health framing, it refuses to certify the mandate as pure coordination: the same structure that protects the vulnerable takes bodily decision authority from the coerced, and that taking is registered at the payer seats rather than absorbed into a collective-benefit aggregate. Against the pure-liberty framing, it refuses to certify the arrangement as pure extraction: the coordination function is genuine — the founding problem (free-rider collapse of voluntary coverage) is corroborated live from outside the beneficiary set, and this reading's own claim concedes the collective benefit is real while denying its justificatory force — so the arrangement is not a coordination cover story but a hybrid whose extraction component this reading holds illegitimate. founding_problem_status is live and the disappearance verdict is world_rearranges, so no zombie or obsolescence flag is warranted: the mandate's function has not atrophied and its maintenance is not theatrical. The mandatrophy risk in this domain is scope creep rather than obsolescence: the rising base_extractiveness series models mandates expanding beyond the founding epidemics into lower-severity contexts — extraction accumulating on a live function, the pattern the temporal drift detector should surface — rather than a dead mandate kept alive by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the bodily_autonomy_primary reading of the vaccine_mandate_balance kernel — the claim that individual consent categorically bars state-compelled medical intervention regardless of collective benefit. Which priority rule governs the kernel: this reading''s categorical consent-trump, the public_health_primary reading''s collective-supremacy-when-voluntary-compliance-fails, or the proportionality_reading''s strict-threshold balancing with robust exemptions?',
    'No empirical resolution exists — the contest is over the normative priority rule itself. Resolution would require a polity-level commitment (constitutional entrenchment of one rule, or a sustained doctrinal shift by the adjudicating courts); within this corpus the readings persist as separate constraint stories linked by network edges, each with its own stable epsilon.',
    'Adopting public_health_primary would move the immunocompromised-exposed into the victim set and the unvaccinated-coerced out of it, collapsing epsilon toward the low end; adopting proportionality_reading would make both victim sets conditional on severity, transmission, and safety thresholds. This story''s epsilon (0.80), victim set, and claimed structure are valid only under the bodily_autonomy_primary reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the vaccine_mandate_balance kernel; the disagreement is located in the priority rule between consent and collective benefit, and the sibling readings invert or condition this story''s victim set.').

omega_variable(
    mandate_coverage_counterfactual,
    'What coverage levels do compulsion regimes actually achieve that well-designed voluntary systems cannot, and how does the coverage delta vary with disease severity and vaccine risk profile?',
    'Natural experiments across jurisdictions with mandate-based versus incentive-based systems matched for vaccine type and disease; difference-in-differences on coverage and outbreak rates around mandate adoption and repeal.',
    'A large delta at low vaccine risk confirms the mandate''s coordination function is genuine but does not touch this reading''s categorical claim; a small delta would indicate the compulsion purchases little coordination, tightening the payer seats'' computed classification toward pure extraction even while the whole-structure claim stays hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_coverage_counterfactual, empirical, 'Whether compulsion buys coverage voluntary systems cannot, and at what severity and risk profile.').

omega_variable(
    coerced_compliance_composition,
    'What share of measured compliance would not have occurred without penalty or exclusion, and is the residual suppression structural (penalties, exclusion machinery) or internalized (compliance-as-civic-duty norms that persist independent of enforcement)?',
    'Revealed-preference studies around exemption-availability changes: the compliance response of the marginal refuser when exemptions are granted or removed isolates the coerced share; post-repeal attitude and behavior tracking isolates internalized components.',
    'A high coerced share confirms the victim set''s size and the enforcement-ratchet trajectory in the measurement series; a dominant internalized share would mean the suppression metric overstates active state force — the constraint would persist through converged preference, shifting the aggregate classification toward coordination even though the payer seats still compute extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coerced_compliance_composition, empirical, 'Coerced versus consensual composition of compliance; structural versus internalized suppression mechanism.').

omega_variable(
    exemption_accommodation_sufficiency,
    'Could robust exemption regimes — the accommodation this reading demands — preserve protective coverage without compelling the objecting minority, or does exemption breadth collapse the coverage threshold the mandate exists to secure?',
    'Coverage and outbreak data from jurisdictions with broad philosophical and religious exemptions versus narrow ones, controlling for baseline hesitancy and disease incidence.',
    'If robust exemptions preserve coverage, the proportionality_reading becomes implementable without violating this reading''s axiom and the practical dispute dissolves into implementation detail; if they collapse coverage, the three readings face a genuine trilemma and this story''s victim set is the stable outcome of any liberty-respecting regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exemption_accommodation_sufficiency, empirical, 'Whether robust exemptions are coverage-compatible — the practical bridge or trilemma between the readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 6, 0.2).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.23).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 18, 0.26).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.28).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 18, 0.74).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the vaccine mandate balance' covers three structurally distinct constraints generated from one kernel. This story (bodily_autonomy_primary) authors epsilon 0.80 for the standing compulsion arrangement with the unvaccinated-coerced and conscientious objectors as victims and the immunocompromised as protected beneficiaries. The sibling stories over the same standing arrangement differ: public_health_primary moves the immunocompromised-exposed into the victim set (lethal exposure from others' refusal) and the coerced out of it, collapsing epsilon; proportionality_reading makes both sets conditional on severity, transmission, and safety thresholds. The epsilon values differ because the readings differ — not because the arrangement is measured differently; each story holds one stable, epsilon-invariant classification. Family edges run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
