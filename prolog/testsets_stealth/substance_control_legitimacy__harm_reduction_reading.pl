% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm-Reduction Substance Control Regime (Medicalized, Non-Criminalizing)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   A public health authority claims jurisdiction over substance use under a
 *   duty to minimize population harm, and exercises it through medicalized
 *   management rather than user criminalization: users are channeled into
 *   treatment programs, supervised consumption sites, and disease
 *   surveillance, while supply-side enforcement against the black market
 *   persists. The regime's binding edge is its mandate machinery — treatment
 *   orders issued as conditions of avoiding detention or retaining housing,
 *   benefits, or custody, plus civil commitment of users judged dangerous to
 *   themselves. The claim/metric gap is deliberate: the constraint is CLAIMED
 *   as tangled_rope (genuine harm-reduction coordination entangled with
 *   mandate burden), and the authored metrics describe moderately extractive,
 *   actively enforced operation with a persistent black market. The engine
 *   computes per-seat classifications from the structural data; the claim is
 *   not tuned to any predicted output. KEY AGENTS (by structural
 *   relationship): - mandated_treatment_subjects: primary target
 *   (powerless/trapped) — bear mandate compliance burden -
 *   civilly_committed_users: primary target (powerless/trapped) — bear
 *   detention burden - black_market_operators: secondary target
 *   (moderate/constrained) — bear residual supply-side enforcement -
 *   public_health_authority: agenda setter and beneficiary
 *   (institutional/constrained) — administers the regime; the mandate
 *   apparatus justifies its budget and jurisdiction - treatment_providers:
 *   primary beneficiary (organized/mobile) — collect the mandated patient
 *   stream and per-episode funding - general_public: beneficiary with diffuse
 *   cost (organized/constrained) — receives harm reduction, pays through
 *   taxation - drug_user_unions: excluded advocate (organized/constrained) —
 *   would redesign the regime around consent - public_health_researchers:
 *   analytical observer (institutional/analytical) — evaluates outcomes and
 *   coercion
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.58).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.55).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm-Reduction Substance Control Regime (Medicalized, Non-Criminalizing)").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0').
narrative_ontology:cs_kernel_codification('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', formalized).
narrative_ontology:cs_authority_grounding('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', expertise).
narrative_ontology:cs_interpretation_layer_present('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0').
narrative_ontology:cs_reading_relation('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', foundational, harm_minimization_grounds_state_authority).
narrative_ontology:cs_axiom_status(harm_minimization_grounds_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', harm_minimization_grounds_state_authority, instrumental).
narrative_ontology:cs_axiom('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', secondary, therapeutic_coercion_within_authority).
narrative_ontology:cs_axiom_status(therapeutic_coercion_within_authority, holdable).
narrative_ontology:cs_axiom_grounding('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', therapeutic_coercion_within_authority, instrumental).
narrative_ontology:cs_reference_frame('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', medicalized_public_health_stewardship).
narrative_ontology:cs_drift_state('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', contemporary_overdose_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ed0b73c-5de8-42cc-8dce-33bcbbbe1cd0', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_authority).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, general_public).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, mandated_treatment_subjects).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, civilly_committed_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, general_public).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, medical_model_of_addiction).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, public_health_exception_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the harm reduction regime: sets mandate criteria, funds treatment capacity, operates supervised consumption and disease surveillance, and refers users into mandated programs through courts and commitment statutes. The mandate apparatus justifies its budget and jurisdiction. It cannot unilaterally dismantle mandates without legislative change, and it bears political risk from both directions — coercion objections from civil liberties quarters and mortality blame from the public.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, public_health_authority, beneficiary).

% Clinics and programs receiving public funding and court-referred patients, paid per treatment episode and compliance milestone. The mandated stream is their reliable caseload base; they can shift toward voluntary clients, but mandated referrals are what scale their operations. Exit means reorienting to private-pay or voluntary markets and absorbing the loss of the mandated caseload.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    organized, biographical, mobile, national).

% Receives reduced public disorder, lower disease transmission, and a smaller acute-care burden than under either criminalization or unmanaged use. Pays for the treatment apparatus, supervised consumption sites, and enforcement through taxation. Its choice set is whatever legislatures fund; it neither administers the regime nor directly bears its mandates.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, general_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, general_public, payer).

% People who use substances and are ordered into treatment as a condition of avoiding detention or retaining housing, benefits, or custody. They must attend, comply, and frequently abstain on the program's terms; refusal triggers sanction or commitment proceedings. Exit means completing programs whose conditions the provider and the authority define; there is no route to services without the conditions.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, mandated_treatment_subjects, payer,
    powerless, biographical, trapped, national).

% Users detained under civil commitment after being judged a danger to themselves, confined for the duration of a treatment order. Release depends on clinical judgment within the same system that confined them. They bear the regime's heaviest imposition: confinement without consent, on the authority's own assessment of their risk.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, civilly_committed_users, payer,
    powerless, biographical, trapped, national).

% Supply the demand the regulated channel does not meet — substances outside programs, doses above safe-supply caps, regions without services. Their customers are medicalized, but they remain subject to supply-side criminal enforcement, asset seizure, and interdiction. Exit means forfeiting a livelihood built on the gap the regime itself maintains between regulated supply and total demand.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_operators, payer,
    moderate, immediate, constrained, continental).

% Organizations of current and former users advocating voluntary services, consent-based care, and an end to mandates. They are consulted late or symbolically in mandate design and hold no formal seat in the process that sets mandate criteria. Their preferred redesign — services on request, no conditions attached — is outside the conversation.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, drug_user_unions, excluded,
    organized, biographical, constrained, national).

% Evaluate overdose, infection, and coercion outcomes across regimes; publish the evidence base that supports supervised consumption's mortality benefits and documents mandated treatment's weak retention results. They hold no enforcement or funding power and can be cited or ignored by the authority.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_researchers, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes the population-level response to substance use: treatment capacity, supervised consumption, disease surveillance, and regulated supply are organized once, by public health institutions, instead of being dispersed across courts, emergency rooms, and the unregulated market.
% TRANSFER_FUNCTION: Moves public funding from general revenue into treatment infrastructure and provider institutions; moves users from streets and criminal dockets into mandated treatment programs; moves enforcement effort away from user criminalization and toward mandate compliance and supply-side interdiction.
% ABSENT_VOICES: Active drug users — the people the mandates bind — have no formal seat in mandate design; drug_user_unions are consulted marginally. Black market operators have no seat at all, though they bear the supply-side enforcement the regime retains. Legalization advocates object from outside this reading's framework — that no harm-minimization duty justifies coercing competent adults — and that objection is structurally unheard within the reading's own terms.
% DISAPPEARANCE_RATIONALE: If the harm reduction regime vanished overnight, supervised consumption sites and treatment capacity would disperse back into emergency rooms and the street; users currently mandated into care would return to criminal dockets or unmanaged use; providers would lose the mandated patient stream that anchors their operations; the black market would expand to meet demand no longer partially channeled. The public health apparatus built around substance use would reorganize around either re-criminalization or unmanaged crisis — the arrangement, not the underlying phenomenon, is what holds the current shape in place.
% FOUNDING_PROBLEM: Unmanaged substance use was producing mass overdose mortality and injection-related disease transmission while pure criminalization was producing mass incarceration without reducing use. The founding problem was finding a mode of state authority that reduces population harm without the costs of criminalizing users.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: national mortality statistics and coroner data attest that overdose deaths remain at or near record levels; the epidemiological literature attests ongoing injection-related infection burdens; and drug_user_unions — outside the regime's benefiting parties — attest the problem is live while disputing that mandates are the solution. No party inside the regime disputes that the founding problem exists.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end): the regime delivers real services (supervised consumption, treatment capacity, disease prevention) and does not criminalize users, but it takes compliance, attendance, and program submission from mandated subjects, detains committed users, and maintains enforcement against a criminalized supply tier. Suppression (0.55) is predominantly structural — court orders, commitment statutes, and benefit conditionality close off refusal — with a smaller internalized component: the institutionally assigned patient identity, which persists after individual orders lapse and leads some users to self-report into mandates. Theater (0.30): most activity is functional care, but a growing share of mandate activity is performative — abstinence-conditioned orders that contradict the reading's own evidence base, and compliance metrics that measure program throughput rather than health outcomes. Accessibility_collapse (0.50): refusal collapses inside the mandate system, but population-level alternatives persist (voluntary care, abstinence programs, the black market, legalization politics). Resistance (0.55): user unions, civil liberties litigation, jurisdictional non-adoption, and black market evasion. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is what the engine scales by directionality and scope. The measurement series run on one shared time grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From mandated_treatment_subjects and civilly_committed_users the same structure operates as coercive control in medical clothing — orders, sanctions, and detention administered by the institutions that define their 'risk'. From treatment_providers and the general_public it operates as care infrastructure and burden reduction. The public_health_authority experiences it as its statutory duty. Same-level divergence: mandated subjects (powerless, trapped) and black market operators (moderate, constrained) sit under the same regime but bear different instruments — mandate compliance versus supply-side enforcement risk — because the regime medicalizes demand while criminalizing supply. Inter-institutional divergence: the authority sets mandate criteria while providers collect the mandated flow; the authority bears political risk in both directions (coercion objections, mortality blame) that providers do not.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: treatment_providers (funded per episode, mobile exit), general_public (receives harm reduction, pays diffusely through taxation — the secondary payer role pulls it slightly off pure beneficiary), and public_health_authority (agenda setter with beneficiary secondary role: the mandate apparatus justifies its budget and jurisdiction). Declared victims map to high directionality: mandated_treatment_subjects and civilly_committed_users (trapped by orders and commitment statutes — the regime's full burden lands on them), and black_market_operators (constrained by criminalized supply, moderate power, immediate horizon). Drug_user_unions (excluded) and public_health_researchers (observer, analytical exit) carry no declared structural position and remain commentary-grade. No directionality overrides were needed: the beneficiary/victim declarations plus exit options place every declared seat, and no power atom hosts two agents the derivation cannot distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — overdose mortality plus the failure cost of criminalization — is live and independently corroborated, so mandatrophy is not resolved. The risk runs the other way: the mandate layer has grown faster than the harm-minimization warrant it cites (commitment expansion, abstinence-conditioned orders), which is extraction accumulation rather than atrophy. Classifying this as tangled_rope prevents two mislabels: reading it as rope would erase the mandate victims; reading it as snare would erase the demonstrably functional harm-reduction core. The omega on sunset behavior tracks whether the mandate apparatus is problem-bound or self-perpetuating — the mandatrophy question for this regime is prospective, not settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint instantiates the harm_reduction_reading of the substance_control_legitimacy kernel; the contest between readings is located in the source of state authority over substance use — a duty to minimize harm exercised without criminalization (this reading), adult autonomy limited to third-party harm (legalization_reading), or a moral duty to prevent harm through criminalization (prohibition_reading). Which source governs?',
    'Legislative or constitutional adoption of a sibling reading as the governing legitimacy doctrine, or a court ruling that fixes the authority source for substance control.',
    'Under prohibition_reading the victim set expands to all users as criminal enforcement targets and epsilon rises toward snare territory; under legalization_reading the mandate machinery dissolves, the victim set shrinks to third-party harm enforcement, and epsilon falls toward rope. This file''s epsilon (0.58) is valid only while the harm reduction reading governs the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Which reading of the substance control legitimacy kernel governs determines the constraint''s victim structure and epsilon.').

omega_variable(
    mandate_coercion_vs_care,
    'Is the burden the mandate machinery imposes on users coercive control, or the price of delivering care to a population that declines voluntary services?',
    'Compare retention and health outcomes between voluntary and mandated treatment tracks; track commitment order volumes and sanction rates against therapeutic outcomes across jurisdictions with different mandate intensity.',
    'If mandated tracks show no outcome advantage over voluntary care, the mandate layer is control riding on the care function and the regime drifts toward snare; if outcomes improve under mandates, part of the measured burden is coordination cost and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_coercion_vs_care, empirical, 'Whether treatment mandates function as care delivery or as coercive control.').

omega_variable(
    black_market_retention_intent,
    'Is the persistent black market a transitional residue of incomplete supply regulation, or a structural feature — the regime deliberately retains supply-side criminalization while medicalizing demand?',
    'Track legislative action on safe-supply expansion against supply-side enforcement budgets; if enforcement budgets grow while safe-supply coverage stalls, retention is deliberate rather than transitional.',
    'Deliberate retention means the regime maintains a permanently criminalized producer class as part of its structure — a two-tier arrangement pushing classification toward snare; transitional residue supports the tangled_rope reading with a possible scaffold-like trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_retention_intent, empirical, 'Whether the black market''s persistence is deliberate structure or incomplete transition.').

omega_variable(
    mandate_apparatus_sunset_behavior,
    'If overdose mortality falls substantially, will the mandate apparatus (commitment statutes, treatment courts, compliance monitoring) sunset with the founding problem, or persist as standing control infrastructure?',
    'Track mandate volumes, commitment orders, and enforcement budgets relative to the mortality trend over the coming decade.',
    'Persistence after the founding problem recedes would date mandatrophy and shift the regime''s later-life classification toward piton or snare dynamics; sunset behavior would confirm the tangled_rope reading with a scaffold-like exit path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_apparatus_sunset_behavior, empirical, 'Whether the mandate apparatus is problem-bound or self-perpetuating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(subs_tr_t4, observed).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(subs_tr_t8, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement_basis(subs_tr_t12, observed).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(subs_tr_t16, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(subs_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement_basis(subs_be_t4, observed).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(subs_be_t8, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement_basis(subs_be_t12, observed).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(subs_be_t16, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(subs_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement_basis(subs_su_t4, observed).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement_basis(subs_su_t8, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement_basis(subs_su_t12, observed).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(subs_su_t16, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(subs_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drug policy' decomposes into three structurally distinct constraints — sibling readings of the substance_control_legitimacy kernel. Each reading has its own epsilon, victim set, and classification: prohibition_reading criminalizes users (victim set: all users; high epsilon, snare-flavored); legalization_reading limits authority to third-party harm (no mandate victims; low epsilon, rope-flavored); this file instantiates harm_reduction_reading (victim set: mandated and committed users plus the criminalized supply tier; moderate epsilon, tangled_rope). The readings are linked as a constraint family. Epsilon is invariant within each file because each instantiates a different constraint, not one constraint measured differently; this reading's upstream position between the siblings (it borrows criminalization's supply enforcement and prefigures legalization's regulated supply) is recorded in its reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
