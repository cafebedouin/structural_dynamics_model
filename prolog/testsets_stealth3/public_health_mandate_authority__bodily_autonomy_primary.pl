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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Bodily-Sovereignty-Primary Reading of Public Health Mandate Authority
 *   domain: public health law/constitutional rights/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading — bodily_autonomy_primary — of the
 *   kernel public_health_mandate_authority, and classifies the STANDING
 *   ARRANGEMENT under contest: the regime by which health authorities compel
 *   medical intervention (vaccination mandates with employment, enrollment,
 *   and access penalties), as that arrangement appears through the
 *   categorical bodily-sovereignty lens. Per the kernel-reading epsilon rule,
 *   the referent of extractiveness is this standing mandate arrangement as
 *   this reading sees it — NOT the consent-based alternative the reading
 *   endorses. The reading's own lights find the arrangement deeply
 *   extractive: it strips consent from a class of persons and enforces
 *   compliance through livelihood penalties, whatever the population-level
 *   payoff. KEY AGENTS (by structural relationship): -
 *   conscientious_objectors_to_mandate: Primary target
 *   (moderate/identity_locked) — bears the coercion costs; exit is fused with
 *   refusal identity. - marginally_hesitant_compliers: Secondary target
 *   (moderate/constrained) — pays the unwanted intervention under penalty
 *   pressure, receives its personal benefit. - public_health_agencies:
 *   Agenda-setter (institutional/arbitrage) — designs and enforces; receives
 *   the extracted compliance and expanded authority. -
 *   private_employers_enforcing: Delegated enforcer (powerful/arbitrage) —
 *   administers workplace penalties. - voluntarily_vaccinated_majority:
 *   Beneficiary (organized/mobile) — collects protection, bears no coercion.
 *   - immunocompromised_patients: Pure-subsidy beneficiary
 *   (powerless/constrained) — protected at zero bodily cost; per this
 *   reading's structural delta they are EXCLUDED from the victim set (no duty
 *   to protect via bodily invasion). - public_health_primary_advocates:
 *   Beneficiary-advocates (organized/mobile) — endorse the arrangement; zero
 *   extraction lands on them. - constitutional_courts: Analytical observer
 *   (institutional/analytical) — adjudicate the arrangement's validity.
 *   Family relationships: the colloquial question 'are mandates justified?'
 *   decomposes into three structurally distinct constraints linked by
 *   network.affects_constraints — this file (bodily_autonomy_primary, high
 *   epsilon for the standing arrangement, victims = the coerced refusers),
 *   public_health_mandate_authority__public_health_primary (epsilon indexed
 *   to commons failure, victims would include the immunocompromised as the
 *   unprotected), and
 *   public_health_mandate_authority__proportionality_reading (epsilon varying
 *   with threat severity, alternatives available, coercion magnitude, and
 *   duration). Each sibling carries its own epsilon, victim set, and
 *   classification; the epsilon differences are the disagreement, located
 *   structurally in victim-set membership and justificatory structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.72).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.52).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Bodily-Sovereignty-Primary Reading of Public Health Mandate Authority").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public health law/constitutional rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, 'cf86d54b-4e12-4e10-a3f3-44c17937775e').
narrative_ontology:cs_kernel_codification('cf86d54b-4e12-4e10-a3f3-44c17937775e', formalized).
narrative_ontology:cs_authority_grounding('cf86d54b-4e12-4e10-a3f3-44c17937775e', lineage).
narrative_ontology:cs_interpretation_layer_present('cf86d54b-4e12-4e10-a3f3-44c17937775e').
narrative_ontology:cs_reading_relation('cf86d54b-4e12-4e10-a3f3-44c17937775e', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('cf86d54b-4e12-4e10-a3f3-44c17937775e', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('cf86d54b-4e12-4e10-a3f3-44c17937775e', foundational, bodily_sovereignty_categorical_trump).
narrative_ontology:cs_axiom_status(bodily_sovereignty_categorical_trump, holdable).
narrative_ontology:cs_axiom_grounding('cf86d54b-4e12-4e10-a3f3-44c17937775e', bodily_sovereignty_categorical_trump, deontological).
narrative_ontology:cs_axiom('cf86d54b-4e12-4e10-a3f3-44c17937775e', secondary, collective_benefit_never_licenses_invasion).
narrative_ontology:cs_axiom_status(collective_benefit_never_licenses_invasion, holdable).
narrative_ontology:cs_axiom_grounding('cf86d54b-4e12-4e10-a3f3-44c17937775e', collective_benefit_never_licenses_invasion, deontological).
narrative_ontology:cs_reference_frame('cf86d54b-4e12-4e10-a3f3-44c17937775e', categorical_bodily_inviolability_baseline).
narrative_ontology:cs_drift_state('cf86d54b-4e12-4e10-a3f3-44c17937775e', post_jacobson_mass_mandate_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cf86d54b-4e12-4e10-a3f3-44c17937775e', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, voluntarily_vaccinated_majority).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, conscientious_objectors_to_mandate).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, marginally_hesitant_compliers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, marginally_hesitant_compliers).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, private_employers_enforcing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Refuse mandated vaccination on principled grounds and absorb the attached penalties: termination from employment, school and university exclusion, credential and access restrictions, and in some jurisdictions civil fines. Litigate, protest, and lobby for exemption expansion and statewide prohibition of mandates. Exiting means receiving an intervention they regard as a violation of their bodily self-determination, which for the committed core is not a price they will pay at any penalty level.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, conscientious_objectors_to_mandate, payer,
    moderate, biographical, identity_locked, national).

% Were uncertain or reluctant about the intervention but accepted it under penalty pressure to keep jobs, enrollments, or travel access. They bore the unwanted intervention itself without the conviction that softens it for volunteers, and they also received the personal protection the intervention confers. Their alternative was forfeiting income or education, which most judged worse than acquiescence.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, marginally_hesitant_compliers, payer,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, marginally_hesitant_compliers, beneficiary).

% Design the mandate architecture: coverage thresholds, exemption criteria, penalty schedules, and enforcement delegation to employers and schools. Receive the compliance their programs require and the authority expansion that comes with administering it. They can vary instruments (requirements versus incentives versus recommendations) and shift enforcement intensity without exiting the arrangement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies, beneficiary).

% Administer workplace mandates as delegated enforcers: require shots or testing, terminate noncompliant staff, and manage the administrative and attrition costs of doing so. They gain a workforce-continuity instrument and liability shielding, and they can scale enforcement up or down with little personal exposure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, private_employers_enforcing, agenda_setter,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, private_employers_enforcing, beneficiary).

% Accepted the intervention willingly or long ago and collect the reduced-transmission environment the mandate sustains. Nothing is extracted from them; their support supplies the political base that keeps enforcement viable.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, voluntarily_vaccinated_majority, beneficiary,
    organized, biographical, mobile, national).

% Cannot be vaccinated themselves or respond poorly if vaccinated, and depend on surrounding population immunity for protection. The mandate reduces their exposure at no bodily cost to them; they cannot exit the population in which transmission circulates, but they carry none of the constraint's burdens. Under this reading their protection is expressly denied standing as a justification for anyone else's compelled intervention.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_patients, beneficiary,
    powerless, immediate, constrained, national).

% Citizens, clinicians, and professional associations who endorse the mandate arrangement and argue from the protection of the vulnerable commons. No coercion lands on them: the arrangement imposes nothing they would not accept, so they collect its benefits while bearing none of its penalties.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, beneficiary,
    organized, generational, mobile, national).

% Adjudicate the validity of mandate regimes against constitutional limits, weighing police-power precedent against bodily-integrity and religious-liberty claims. They uphold some mandates, block others, and set the doctrinal conditions under which the arrangement persists or contracts.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Internalizes the vaccination externality: individual immune decisions undersupply population-level protection because contributors bear the full cost while benefits spill to non-contributors. The mandate converts a voluntary-contribution game into enforced uniform contribution, stabilizing coverage above transmission-breaking thresholds, protecting hospital capacity, and keeping schools and workplaces operable during epidemics.
% TRANSFER_FUNCTION: Moves decision-rights over bodily intervention — and the penalties attached to declining (employment, enrollment, mobility credentials, civil fines) — from mandate-refusing individuals to the collective program administered by health authorities and delegated enforcers; moves reduced disease burden outward to the general population, employers, and institutions.
% ABSENT_VOICES: Minors subject to school-entry mandates have no direct voice; parents, boards, and agencies speak for them. Immunocompromised patients' objection — that the categorical bar leaves them exposed to preventable transmission — is structurally pre-empted rather than heard, because the reading's axiom rules their protection out as a justification before deliberation begins. Workers terminated under emergency mandates entered the conversation as litigants only after enforcement had already acted.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished overnight, terminated workers would seek reinstatement or restructure their careers, school-entry requirements would revert to recommendations, credential-checkpoint systems would shut down, outbreak response would narrow to quarantine and isolation of confirmed cases, the challenge litigation docket would empty, and jurisdictional evidence suggests coverage would drift down unevenly until the next epidemic crisis rebuilt demand for compulsion.
% FOUNDING_PROBLEM: Recurrent epidemic free-riding: voluntary vaccination undershoots the coverage threshold at which transmission chains break, producing preventable mortality among the unprotected and surge collapse of hospital capacity — smallpox at the Jacobson era's turn, polio mid-century, measles resurgences in 1989-91 and 2019, COVID-19 in 2020-22.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by pre-vaccine-era vital statistics on epidemic mortality, insurance-sector loss data on outbreak-driven absenteeism, and demographer-attested cross-jurisdiction natural experiments in which coverage declines preceded measles resurgence. None of these sources capture mandate rents. The categorical reading itself disputes the problem's justificatory force rather than its occurrence — it grants the externality is real while denying that any collective benefit licenses the remedy, which is precisely where it separates from the public-health-primary sibling.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the standing arrangement possesses all three structural markers: a genuine coordination function (externality internalization that voluntary choice demonstrably undersupplies), asymmetric extraction (coercion costs concentrate on refusers while benefits flow broadly to people who never faced a penalty), and active enforcement (employment termination, school exclusion, checkpoint credentials). The reading's AXIOM is categorical condemnation — no collective benefit justifies non-consensual intervention — which is sharper than any structural label; where the computed per-seat verdicts diverge from that axiom's absolutism, that divergence is the datum this corpus exists to take, not an error to reconcile. Metrics describe actual operation: extraction 0.72 reflects livelihood-grade penalties concentrated on a minority seat; suppression 0.52 is the current enforcement intensity (suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness takes the directionality/scaling treatment); theater_ratio 0.22 stays low because coverage and transmission effects are functional, with only a modest performative layer (credential rituals, compliance signage). Accessibility_collapse 0.45: alternatives persist (exemptions, remote arrangements, jurisdictional exit, delay), so understanding the constraint does not close the option set. Resistance 0.60: sustained litigation, protest waves, and statewide prohibition statutes. The temporal series runs on ONE shared grid (t in {0,2,4,6,8,10}, years since 2019) with every tracked metric authored at every point; points through t=6 are observed, t=8 and t=10 are projected and flagged. The trajectory is crisis-cyclical rather than monotonic: a pandemic spike (peak extraction 0.84, enforcement 0.80 at t=2) followed by partial relaxation as emergency mandates lapsed and courts and legislatures pushed back — the oscillation tracks epidemic alarm cycles, and the alarm phase is when extraction is installed; it is intermittent reinforcement of the arrangement's reach, not noise. Receipt surface: gains demonstrably accrue to the agencies that receive compliance and authority, so gain_flow names that seat rather than asserting diffuse; fixing_cost is authored 'cheap' on jurisdictional evidence — several legislatures repealed or banned mandates without system collapse in the short run — while noting the long-run outbreak-cost objection that the proportionality sibling would press.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the agency and employer seats the arrangement is a functioning instrument they operate successfully — coordination-forward, low felt extraction, near-zero directionality. From the refuser seat the identical structure is livelihood coercion against an immovable identity commitment — extraction-forward, high directionality. From the voluntarily vaccinated and advocate seats it is background protection they never priced. Courts occupy an analytical seat weighing doctrine rather than burden. The engine computes these per-seat classifications from the structural data; the categorical axiom does not adjudicate between them, it condemns the arrangement wholesale from outside the seat calculus.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation: voluntarily_vaccinated_majority, public_health_primary_advocates, and immunocompromised_patients sit at the beneficiary pole (d near 0.0) — the last two satisfy the reading's structural delta that immunocompromised are excluded from the victim set and that zero extractiveness lands on public-health-primary advocates, who bear no coercion at all. The two payer seats derive toward the target pole: conscientious_objectors near-full (victim declaration, identity_locked exit amplifying), marginally_hesitant high-but-not-full (they also received the intervention's personal benefit, damping d below the objectors'). One directionality override is declared: power_atom 'powerless' pinned to d=0.08 for immunocompromised_patients. Without it, the derivation would read their constrained exit as target-proximity — but their inability to leave is general medical vulnerability, not a constraint-specific burden; relative to THIS constraint they bear zero cost and receive pure subsidy, so the derived d would be structurally wrong. No other agent shares the powerless atom, so the override touches only this seat. Agencies and delegated employers sit near-symmetric to slightly beneficiary-side: they receive compliance and authority while absorbing administration and attrition friction.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview closes the lifecycle question: founding_problem_status is 'live' (recurrent epidemic free-riding is corroborated by outside sources) and disappearance_verdict is 'world_rearranges', so the status-times-verdict consumer finds the consistent cell — no capture/zombie flag, and mandatrophy is NOT resolved: the arrangement has not outlived its function. The tangled_rope claim is what prevents misclassification in both directions: reading the arrangement as a rope would erase the asymmetric coercion this reading exists to name (penalties fall on refusers while beneficiaries never face the choice); reading it as a snare would erase the real coordination function that even this reading's own corroboration record attests (the externality is real; the dispute is over whether it licenses the remedy). The categorical axiom sits outside the structural taxonomy deliberately — it is a deontological ceiling on justification, not a competing estimate of the metrics — and the omega variables carry the question of whether that ceiling behaves as a discovered limit or an absolutized valuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the bodily_autonomy_primary reading of the public_health_mandate_authority kernel; how would the sibling readings (public_health_primary, proportionality_reading) restructure the victim set, epsilon, and classification?',
    'Generate the two sibling stories and compare engine-computed per-seat classifications; the structural deltas — immunocompromised patients entering the victim set as commons-failure casualties under public_health_primary, epsilon falling under proportionality''s admitted-justified region — locate the disagreement precisely.',
    'Under public_health_primary the immunocompromised seat flips from subsidy to casualty and agency seats acquire duty-weighting; under proportionality_reading some configurations compute as plain rope with no asymmetric residue, eliminating the tangled_rope verdict this reading produces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the mandate-authority kernel; sibling deltas carried here rather than in invented fields.').

omega_variable(
    categorical_claim_status,
    'Is the categorical axiom (''no collective benefit justifies non-consensual intervention'') a discovered structural limit, or an absolutized valuation that proportionality analysis would treat as one heavily weighted term?',
    'Test whether any factor configuration (catastrophic threat, low-coercion instrument, short imposition window) shifts adherents'' verdicts; verdicts invariant across all configurations indicate a deontological constant rather than an empirical threshold.',
    'If a constant, this reading''s epsilon is reading-indexed and stable and its foreclosure against public_health_primary holds; if threshold-responsive, the reading collapses into a high-threshold proportionality variant and its categorical edge dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_claim_status, conceptual, 'Whether the reading''s core axiom is a limit-discovery or an extremal preference.').

omega_variable(
    immunocompromised_seat_asymmetry,
    'Immunocompromised patients are excluded from this reading''s victim set and seated as pure subsidy-beneficiaries; do they become casualties of the reading''s OWN endorsed alternative, since dismantling mandates raises their exposure?',
    'Compare exposure and mortality trajectories in jurisdictions that dropped mandates against those that retained them, stratified by immunocompromise status.',
    'If their harm migrates onto the alternative arrangement, this reading''s beneficiary structure is incomplete and the coordination-function credit shrinks, strengthening the sibling-side reading; if not, the structural delta''s exclusion stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_seat_asymmetry, conceptual, 'Seat asymmetry: the group this reading refuses to count as victims of the mandate may be victims of the mandate''s absence.').

omega_variable(
    refuser_coalition_potential,
    'Can dispersed mandate-refusers convert class-level grievance into durable coalition power — litigation victories, statewide prohibition statutes, exemption expansion — sufficient to alter enforcement?',
    'Track legislative ban adoption, court outcomes, and exemption-rate trends across states over the projection window.',
    'Rising coalition power lowers payer-seat effective extraction and raises resistance further; failure to cohere keeps the class effectively unable to defend its members despite moderate nominal capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refuser_coalition_potential, empirical, 'Coalition-formation prospects of the target class.').

omega_variable(
    coercion_identity_fusion_share,
    'How much of the measured coercion burden among refusers is structural (penalties actually imposed) versus internalized (identity fusion making acceptance unthinkable regardless of penalty size)?',
    'Post-repeal behavioral tracking: if former-refuser populations accept vaccination once penalties vanish, the burden was structural; if refusal persists without penalties, identity fusion carries the constraint internally.',
    'A high internalized share means the scalar suppression measure understates the arrangement''s grip on the identity-locked seat, and repeal alone would not dissolve the constraint for that population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_identity_fusion_share, empirical, 'Structural versus internalized share of the measured coercion.').

omega_variable(
    quarantine_boundary_scope,
    'Does the categorical axiom extend to infection-control measures that restrict movement without invading the body (quarantine of confirmed infectious cases), or does the reading carve these out as non-interventions?',
    'Doctrinal analysis of the reading''s own authoritative texts distinguishing bodily invasion from liberty-of-movement restriction, plus adherence surveys among categorical advocates.',
    'A carve-out preserves a residual epidemic toolset and narrows the conflict with siblings to intervention proper; no carve-out extends the categorical conflict to all epidemic control and hardens the foreclosure against public_health_primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quarantine_boundary_scope, conceptual, 'Scope boundary of the categorical axiom within epidemic-control instruments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(publ_tr_t2, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2, 0.3).
narrative_ontology:measurement(publ_tr_t4, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 4, 0.27).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 6, 0.24).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 8, 0.23).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 10, 0.22).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(publ_be_t2, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2, 0.84).
narrative_ontology:measurement(publ_be_t4, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 4, 0.76).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 6, 0.73).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 10, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(publ_su_t2, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2, 0.8).
narrative_ontology:measurement(publ_su_t4, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, informed_consent_doctrine).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the mandate debate' covers three structurally distinct claims with different epsilons, victim sets, and failure modes, so it is modeled as three stories linked by network.affects_constraints. This upstream member (bodily_autonomy_primary) exerts structural pressure on both siblings: its categorical axiom defines the boundary the proportionality sibling's scale weighs against, and its foreclosure relation to public_health_primary marks the direct contradiction between 'never justified' and 'obligatory for the commons'. Sibling files must reciprocate the linkage and document their own epsilon indexing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, powerless, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
