% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate as Obligation to Protect the Vulnerable Commons (public_health_primary reading)
 *   domain: public health law / constitutional rights / bioethics
 *
 * SUMMARY:
 *   This story instantiates the public_health_primary reading of the
 *   public-health-mandate kernel: the mandate is an obligation owed to the
 *   vulnerable commons — immunocompromised people who cannot be protected by
 *   their own action, and healthcare infrastructure that absorbs surge
 *   failure — discharged through compulsory collective action. The standing
 *   arrangement under contest is the mandate regime itself: immunization
 *   conditions attached to employment, clinical practice, and entry to
 *   services, enforced by agencies, employers, and service operators. The
 *   reading holds a genuine coordination function (voluntary uptake
 *   underproduces the protection level the vulnerable commons requires) while
 *   imposing sharp, concentrated costs on a defined refusing minority —
 *   termination, exclusion, and mobility restriction. Per the claim/metric
 *   independence rule, the claimed_type (tangled_rope) states what I believe
 *   is structurally true and the metrics state what I believe is
 *   descriptively true; neither was tuned to the other or to a predicted
 *   engine output. The epsilon referent is the standing mandate arrangement
 *   as this reading assesses it — not the voluntary-compliance world the
 *   sibling readings would endorse.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter (institutional/arbitrage) — issues requirements, defines exemptions, shifts between orders and guidance as courts narrow tools
 *   - immunocompromised_patients: primary intended beneficiary (powerless/trapped) — cannot vaccinate into safety; depends on population-level protection
 *   - hospital_systems: institutional beneficiary (constrained) — reduced surge exposure; also bears implementation and litigation costs
 *   - vaccinated_majority: beneficiary with compliance-cost residue (moderate/constrained) — receives risk reduction, carries scheduling, side-effect, and documentation burdens
 *   - mandate_resistant_employees: primary target (moderate/constrained) — employment conditioned on status; exits are exempt employers, remote work, or resignation
 *   - unvaccinated_healthcare_workers: sharpest target (moderate/trapped) — sector-wide licensure leaves no in-profession employer to transfer to
 *   - unvaccinated_service_denied: secondary target (powerless/constrained) — entry conditions fragment daily life into accessible and inaccessible spaces
 *   - private_employers: enforcement intermediary (organized/constrained) — administers verification and termination; gains workforce health, absorbs overhead and litigation exposure
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates police-powers claims against bodily-integrity and religious-liberty claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.62).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.68).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate as Obligation to Protect the Vulnerable Commons (public_health_primary reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public health law / constitutional rights / bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, 'a5f5a30b-3cab-40eb-af1e-d4347539fa3c').
narrative_ontology:cs_kernel_codification('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', formalized).
narrative_ontology:cs_authority_grounding('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', lineage).
narrative_ontology:cs_interpretation_layer_present('a5f5a30b-3cab-40eb-af1e-d4347539fa3c').
narrative_ontology:cs_reading_relation('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', foundational, vulnerable_commons_protection_duty).
narrative_ontology:cs_axiom_status(vulnerable_commons_protection_duty, holdable).
narrative_ontology:cs_axiom_grounding('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', vulnerable_commons_protection_duty, deontological).
narrative_ontology:cs_axiom('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', foundational, free_rider_externality_warrants_enforcement).
narrative_ontology:cs_axiom_status(free_rider_externality_warrants_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', free_rider_externality_warrants_enforcement, instrumental).
narrative_ontology:cs_reference_frame('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', collective_defense_of_vulnerable_commons).
narrative_ontology:cs_drift_state('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', post_emergency_retrenchment, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a5f5a30b-3cab-40eb-af1e-d4347539fa3c', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, hospital_systems).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, vaccinated_majority).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_employees).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, unvaccinated_healthcare_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, unvaccinated_service_denied).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, vaccinated_majority).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, private_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue immunization requirements, define exemption categories, and coordinate enforcement with employers and service operators. During acute outbreaks they escalate requirements; afterward they retire some and retain others. When courts narrow their formal tools they shift to advisory guidance and procurement conditions, keeping the program moving through different instruments.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Rely on the surrounding population's immunization for protection they cannot obtain for themselves; many complete every measure asked of them and remain exposed anyway. When community uptake sags they retreat further from public life, and no personal purchase restores the missing protection.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Absorb surge admissions during outbreaks and administer staff-immunization rules across their workforces. Lower admission volumes ease capacity strain and staffing pressure; implementing requirements, processing exemption requests, and defending the rules in court consume administrative and legal resources they would not otherwise spend.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, hospital_systems, beneficiary,
    institutional, generational, constrained, regional).

% Completed immunization and keep up with boosters; they receive reduced personal risk and constitute the protection level that immunocompromised people depend on. They also carry appointment scheduling, side-effect risk, and recurring status documentation, and they live inside the same polarized argument the requirements generate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, vaccinated_majority, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, vaccinated_majority, payer).

% Declined immunization and now face a conditional workplace: verify status or leave. Some found exempt employers or remote arrangements; others resigned, were terminated, or sued. For a substantial subset the refusal is bound up with political identity, so accommodating the requirement feels like surrender, which narrows the exits they are willing to take even where formal ones exist.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_employees, payer,
    moderate, biographical, constrained, national).

% Worked through the worst of the pandemic and then faced universal staff requirements adopted across hospitals, long-term-care facilities, and clinics simultaneously. With every in-profession employer imposing the same condition, transferring jobs changes nothing; the choice compresses to compliance or leaving clinical practice, training, and income built over years.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, unvaccinated_healthcare_workers, payer,
    moderate, biographical, trapped, national).

% Encounter entry conditions at restaurants, venues, transit, and events keyed to immunization status. Daily life fragments into accessible and inaccessible spaces that shift as jurisdictions and operators adopt or drop checks; travel plans, employment interviews, and family events turn on documentation they decline to hold.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, unvaccinated_service_denied, payer,
    powerless, biographical, constrained, regional).

% Administer status verification, process exemption requests, and dismiss non-compliant staff where rules require it. They gain a healthier, lower-liability workforce and regulatory cover, but absorb verification overhead, morale management, and litigation exposure — and many retained requirements after public mandates lapsed because workforce protection served their own operations.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, private_employers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, private_employers, payer).

% Hear challenges weighing state police powers against bodily-integrity, religious-liberty, and occupational claims. Their rulings have stayed a broad cross-industry requirement while upholding healthcare-sector mandates, drawing the boundary the other seats then operate within; they take testimony and record from every other seat but collect and pay nothing themselves.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in infectious-disease defense: individual immunization decisions underproduce the population-level protection that immunocompromised people and hospital capacity require, and the requirement aligns individual choices with the collective threshold.
% TRANSFER_FUNCTION: Moves compliance (and its risks, costs, and refusal consequences) onto the immunization-resistant — employment continuation, clinical practice, and service access made conditional on status — and transfers reduced-transmission security to immunocompromised patients, hospital systems, and the vaccinated majority.
% ABSENT_VOICES: The mandate-resistant are audible in streets and courthouses but largely absent from the rule-setting table: agency dockets weight clinical and institutional testimony, while conscientious-objector organizations, disability advocates skeptical of blanket rules, and immunocompromised people who oppose mandates on bodily-integrity grounds had to force their way in through litigation. They are outside the room where exemption categories and enforcement design get decided.
% DISAPPEARANCE_RATIONALE: If the requirement regime vanished overnight, employment relations would reorganize around voluntary status, service-entry conditions would dissolve, transmission dynamics during future outbreaks would shift burden onto the immunocompromised and onto hospital surge capacity, and the litigation and legislative machinery built around the rules would lose its object — the arrangements of every named seat depend on the regime existing.
% FOUNDING_PROBLEM: Communicable disease repeatedly devastated populations, and the people most likely to die — the immunocompromised, the elderly, infants — cannot secure their own protection; where voluntary uptake falls short of the threshold that shields them, only coordinated compulsion reaches it. The modern legal form descends from early twentieth-century compulsory-vaccination cases upheld as exercises of state protective power.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration is split along the same lines as the kernel contest. Independent epidemiological surveillance and peer-reviewed outbreak studies attest that vaccine-preventable disease and vulnerable-population mortality persist, supporting liveness; legislative findings in states that banned mandates, and testimony from bioethicists across camps, attest that the emergency-specific threat receded and voluntary uptake sufficed, supporting obsolescence. No source wholly outside the benefiting parties settles the question — which is itself the finding: the founding problem's status is exactly what the three readings dispute.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the arrangement imposes concentrated, career-ending and access-ending costs on a defined minority while the reading itself discounts those costs as legitimate externality internalization — the structural fact of coercion-via-job-loss registers strongly even under a sympathetic seat. Suppression is 0.68 as a raw structural property (unscaled by power or scope; only extractiveness is scaled by the engine): persistence depends on actively maintaining status-conditioned employment and entry, not on participant preference, though exemptions and testing-out keep it below total closure. Theater is 0.28: during the acute phase nearly all enforcement activity performed real epidemiological work, but the rising trajectory (0.08 to 0.28) tracks mandates outliving the threat conditions that justified them. Accessibility_collapse is 0.50 — alternatives narrow sharply inside mandated sectors but persist at the margins. Resistance is 0.66: litigation coalitions (which stayed a broad occupational requirement), protests, resignation waves, and legislative bans in several jurisdictions; notably, coalition formation among nominal powerless-class resisters produced real wins, so coalition power is live here. The three measurement series share one time grid (every 8 points across 0–48) so no metric row is sampled against another's gaps; the suppression_requirement series is authored deliberately because the story traces enforcement-capacity change — rapid build-up through the emergency, plateau, then partial relaxation as formal mandates lifted while sectoral and private enforcement persisted. Identity-lock operates on both sides: for a subset of the resistant, refusal fused with political identity such that compliance reads as surrender (ideological identity making exit unthinkable); for healthcare workers, professional identity binds them to practice they can only keep by complying. If either identity frame broke, the affected seat's computed position would soften markedly.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the setter/beneficiary seats compute differently from the same structural data. From the agencies' seat the arrangement is an obligation being discharged; from the trapped healthcare-worker seat it is comply-or-leave-the-profession; from the constrained general-employee seat it is a conditional workplace with partial exits. Inter-institutional divergence is pronounced: courts narrowed the arrangement (staying the broad occupational rule while upholding healthcare-sector mandates), legislatures in several states banned the tools outright, and private employers retained requirements after public ones lapsed — the same nominal constraint operating at different intensities across institutions. Same-level lateral divergence: two moderate-power payer seats (mandate_resistant_employees, constrained; unvaccinated_healthcare_workers, trapped) differ not by global standing but by a constraint-specific factor — sector-wide licensure closing in-profession exit. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: immunocompromised_patients (trapped, powerless) sit nearest the full-beneficiary end — the arrangement subsidizes them with protection they cannot buy; hospital_systems and vaccinated_majority derive low-to-moderate d; the three victim seats derive high d, amplified for the trapped healthcare-worker seat relative to the constrained employee seat. Two overrides correct derivation failures: private_employers carry no beneficiary/victim declaration (they are enforcement intermediaries, not clean beneficiaries), so the derivation would fall to the organized-power-atom fallback and miss their dual position — they enforce the arrangement AND absorb verification overhead and litigation exposure — hence d=0.45 rather than a fallback that would read them as pure setters. No override is applied to the moderate atom because it is shared by both beneficiary-side (vaccinated_majority) and victim-side (both employee seats) agents; their positions are correctly separated by the beneficiary/victim declarations and exit options alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pathogens devastate the unprotected commons; voluntary uptake underproduces the required protection threshold) is contested rather than dead: proponents attest persistent transmission and persistent vulnerability; opponents attest that emergency-specific threats receded and voluntary uptake suffices. Because founding_problem_status is contested and disappearance_verdict is world_rearranges, the mismatch consumer finds no dead-problem-plus-persistent-arrangement flag — the arrangement's persistence is disputed on the merits, not maintained past a vanished function. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure coordination (rope) would erase the concentrated career and access costs borne by the resistant; reading it as pure extraction (snare) would erase the real protection transferred to a constituency that cannot protect itself. The theater_ratio trajectory is the monitored drift signal: if retained post-emergency mandates keep rising theatrically against flat marginal benefit, the omega post_emergency_retention_function resolves toward inertial maintenance and the arrangement migrates toward the degraded type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the public_health_primary reading of kernel public_health_mandate_authority; how would the sibling readings (bodily_autonomy_primary, proportionality_reading) restructure the beneficiary/victim sets and the epsilon of the same standing arrangement?',
    'Generate and classify the sibling reading stories independently; compare victim sets, epsilon, and computed seat classifications across the three files.',
    'bodily_autonomy_primary would move the unvaccinated into the victim set and strip the commons-protection justification, raising epsilon toward snare territory; proportionality_reading would make epsilon track threat severity, availability of alternatives, and coercion magnitude rather than holding a fixed value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three readings, structurally distinct constraints.').

omega_variable(
    free_rider_framing_accuracy,
    'Does the unvaccinated-as-free-rider framing reflect a measured transmission externality, or is it constructed to exclude the unvaccinated from victim consideration?',
    'Attribution studies quantifying marginal transmission contribution by immunization status across settings, variants, and time since dose.',
    'If the externality is materially smaller than the framing assumes, the victim set expands and effective extraction on the resistant rises; if the framing is accurate, the asymmetry is burden allocation rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_framing_accuracy, empirical, 'Whether the externality framing that excludes the unvaccinated from the victim set is descriptively earned.').

omega_variable(
    immunocompromised_protection_failure,
    'Do mandates actually deliver the promised protection to immunocompromised patients, or do waning immunity, variant escape, and incomplete uptake leave them bearing mandate-side disruptions without receiving the protection?',
    'Outcome studies comparing infection and severe-disease rates among immunocompromised cohorts under mandate versus non-mandate regimes, controlling for variant period.',
    'If protection fails, immunocompromised_patients migrate into the victim set (they bear the costs of a regime that does not protect them), deepening the asymmetric structure toward snare-flavored extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_protection_failure, empirical, 'Whether the intended primary beneficiary seat actually collects the protection the arrangement transfers.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression on the resistant structural (employment termination, service denial, legal penalty) or internalized (social ostracism, disclosure avoidance, self-exclusion that persists after formal withdrawal)?',
    'Post-withdrawal behavior tracking: if status concealment and self-exclusion persist after formal mandates lapse, the internalized component is confirmed.',
    'An internalized component raises effective suppression above the structural measure and slows the recovery of voluntary participation after formal retrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a quasi-interpersonal social enforcement setting.').

omega_variable(
    post_emergency_retention_function,
    'Are mandates retained past the acute emergency phase performing residual protective function or drifting toward performative maintenance?',
    'Compare the theater_ratio trajectory against contemporaneous estimates of marginal epidemiological benefit; audit retained mandates against current threat assessments.',
    'Continued theater_ratio rise against flat marginal benefit supports drift toward inertial maintenance; stable benefit supports legitimate retention of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_emergency_retention_function, conceptual, 'Whether the retained post-emergency mandate stock is functional or theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(public_health_primary_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(public_health_primary_tr_t8, public_health_mandate_authority__public_health_primary, theater_ratio, 8, 0.11).
narrative_ontology:measurement(public_health_primary_tr_t16, public_health_mandate_authority__public_health_primary, theater_ratio, 16, 0.16).
narrative_ontology:measurement(public_health_primary_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.21).
narrative_ontology:measurement(public_health_primary_tr_t32, public_health_mandate_authority__public_health_primary, theater_ratio, 32, 0.25).
narrative_ontology:measurement(public_health_primary_tr_t40, public_health_mandate_authority__public_health_primary, theater_ratio, 40, 0.27).
narrative_ontology:measurement(public_health_primary_tr_t48, public_health_mandate_authority__public_health_primary, theater_ratio, 48, 0.28).

% Extraction over time
narrative_ontology:measurement(public_health_primary_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(public_health_primary_be_t8, public_health_mandate_authority__public_health_primary, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(public_health_primary_be_t16, public_health_mandate_authority__public_health_primary, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(public_health_primary_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(public_health_primary_be_t32, public_health_mandate_authority__public_health_primary, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(public_health_primary_be_t40, public_health_mandate_authority__public_health_primary, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(public_health_primary_be_t48, public_health_mandate_authority__public_health_primary, base_extractiveness, 48, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(public_health_primary_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(public_health_primary_su_t8, public_health_mandate_authority__public_health_primary, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(public_health_primary_su_t16, public_health_mandate_authority__public_health_primary, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(public_health_primary_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(public_health_primary_su_t32, public_health_mandate_authority__public_health_primary, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(public_health_primary_su_t40, public_health_mandate_authority__public_health_primary, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(public_health_primary_su_t48, public_health_mandate_authority__public_health_primary, suppression_requirement, 48, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'public health mandate' decomposes into three structurally distinct readings of one kernel (public_health_mandate_authority). This file authors the public_health_primary reading with its own epsilon (referent: the standing mandate arrangement as the commons-protection reading assesses it), its own beneficiary/victim structure (unvaccinated excluded from victims as free-riders; immunocompromised as intended beneficiaries whose protection may fail), and its own claimed type. bodily_autonomy_primary authors a different victim set and a higher epsilon over the same standing arrangement; proportionality_reading authors epsilon as severity-indexed. The upstream doctrinal lineage (police-powers case law) is cited as evidence by this reading and rejected by the autonomy reading; all three files link via affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
