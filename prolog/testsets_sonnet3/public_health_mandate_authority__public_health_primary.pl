% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
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
 *   human_readable: Public Health Mandate Authority — Vulnerable-Commons Reading
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This story instantiates the public_health_primary reading of the
 *   contested public_health_mandate_authority kernel: the mandate is framed
 *   as an obligation running toward the vulnerable commons (immunocompromised
 *   individuals who cannot generate their own immunity, and finite hospital
 *   capacity that serves everyone) rather than as a claim about individual
 *   bodily sovereignty. Under this reading, the immunocompromised enter the
 *   victim set precisely when mandates fail or lapse — they absorb the
 *   disease burden the mandate exists to prevent. The mandate-resistant are
 *   excluded from the victim set by this reading's own terms: they are framed
 *   as externality-imposing free-riders whose noncompliance transfers risk
 *   onto others, not as a harmed party in their own right. That framing is
 *   the reading's core structural commitment and is exactly what the sibling
 *   bodily_autonomy_primary reading rejects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.58).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.62).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority — Vulnerable-Commons Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, 'c55f2993-aff3-4949-9ba3-9783d4d9babf').
narrative_ontology:cs_kernel_codification('c55f2993-aff3-4949-9ba3-9783d4d9babf', distributed).
narrative_ontology:cs_authority_grounding('c55f2993-aff3-4949-9ba3-9783d4d9babf', distributed).
narrative_ontology:cs_reading_relation('c55f2993-aff3-4949-9ba3-9783d4d9babf', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('c55f2993-aff3-4949-9ba3-9783d4d9babf', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('c55f2993-aff3-4949-9ba3-9783d4d9babf', foundational, collective_protection_overrides_individual_refusal).
narrative_ontology:cs_axiom_status(collective_protection_overrides_individual_refusal, holdable).
narrative_ontology:cs_axiom_grounding('c55f2993-aff3-4949-9ba3-9783d4d9babf', collective_protection_overrides_individual_refusal, instrumental).
narrative_ontology:cs_axiom('c55f2993-aff3-4949-9ba3-9783d4d9babf', foundational, externality_imposition_forfeits_victim_standing).
narrative_ontology:cs_axiom_status(externality_imposition_forfeits_victim_standing, holdable).
narrative_ontology:cs_axiom_grounding('c55f2993-aff3-4949-9ba3-9783d4d9babf', externality_imposition_forfeits_victim_standing, conventional).
narrative_ontology:cs_created_at('c55f2993-aff3-4949-9ba3-9783d4d9babf', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_workers).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, hospital_systems).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, immunocompromised_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, employers_and_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces the mandate through licensing, employment conditions, and service-access rules. Frames its function as protecting those who cannot protect themselves biologically (immunocompromised) and preserving hospital capacity for everyone. Bears reputational and legal risk if mandates fail to hold, and institutional risk if compliance collapses.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Cannot mount effective immune response themselves and depend entirely on the vaccination/masking behavior of the surrounding population (herd effect) for protection. When mandates hold, they gain a functioning shield they cannot generate alone. When mandates are weakened, relaxed, or unevenly enforced, they absorb the resulting disease burden directly — hospitalization, death, exclusion from public life to self-protect. They have no exit from this dependency; their only lever is the mandate's existence and strength.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, immunocompromised_populations, payer).

% Staff hospitals and clinics that would be overwhelmed by uncontrolled transmission. Mandates on the broader population reduce the patient surge they must absorb and reduce their own occupational exposure. Can exit the profession but not easily exit the exposure risk while employed in it.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_workers, beneficiary,
    moderate, biographical, constrained, regional).

% Operate with finite bed and staff capacity. Population-level mandates function as a capacity-preservation mechanism, reducing the probability of care rationing. Cannot easily expand capacity on short notice, so depend on demand-side suppression via the mandate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, hospital_systems, beneficiary,
    organized, generational, constrained, regional).

% Object to the intervention on medical, religious, or bodily-autonomy grounds, or have been persuaded the risk-benefit calculus does not favor compliance. Under this reading they are treated as imposing an externality on the vulnerable commons rather than as principal victims. Face termination, loss of licensure, or exclusion from services as the enforcement mechanism; many lack the economic mobility to simply change jobs or relocate. Bear the coercive weight of the mandate without being counted, in this reading, among those the mandate exists to protect.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_workers, payer,
    powerless, biographical, trapped, local).

% Implement and enforce the mandate as a condition of employment or service access, passing through liability and public-health-authority pressure. Absorb administrative cost and labor-market friction (staffing shortfalls from terminations) as the price of compliance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, employers_and_institutions, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, employers_and_institutions, payer).

% Would argue that classifying the mandate-resistant as externality-imposing free-riders forecloses their standing as rights-holders whose bodily autonomy is being overridden. Their framing is not part of this reading's victim accounting by construction — that is the sibling bodily_autonomy_primary reading, not this one.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, civil_liberties_observers, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunity and behavior (vaccination, masking, isolation) so that transmission stays below the threshold that would overwhelm finite hospital capacity or expose those who cannot protect themselves through their own immune response.
% TRANSFER_FUNCTION: Moves the burden of disease risk from the immunocompromised and healthcare infrastructure onto the general population's bodily autonomy and, for the noncompliant specifically, onto their employment and service access.
% ABSENT_VOICES: Mandate-resistant individuals who dispute the risk-benefit calculus or object on bodily-autonomy grounds are structurally present as payers but are not counted as victims under this reading's own terms — they are framed as the externality-imposing party rather than a harmed party. Their objection is real but this reading does not adjudicate it as a wrong.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished overnight, immunocompromised populations would lose the herd-level protection they cannot generate biologically, hospital systems would face higher surge risk with no compensating capacity buffer, and the enforcement apparatus currently pressing on mandate-resistant workers would disappear along with the coercion they experience.
% FOUNDING_PROBLEM: Individual vaccination and infection-control decisions generate externalities on people who cannot bear the underlying risk themselves (immunocompromised) and on shared, capacity-limited healthcare infrastructure; without coordination, rational individual non-participation is not priced against the cost it imposes on those who depend on population-level immunity.
% FOUNDING_PROBLEM_CORROBORATION: Immunologists and hospital administrators outside the enforcement apparatus attest the underlying epidemiological problem (herd-immunity dependency, finite surge capacity) remains live wherever coverage drops. Civil liberties organizations and some public health ethicists — also outside the benefiting institutional seats — attest that whether THIS mandate structure remains the necessary or proportionate solution to that problem is actively disputed, particularly regarding duration and alternatives short of employment/service coercion.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises through the early interval as enforcement intensifies (mandates adopted, employment conditions imposed) and then plateaus once policy stabilizes — reflecting a coercion regime that reaches a steady state rather than continuing to escalate. Suppression follows a similar but earlier-peaking curve, consistent with a mandate whose enforcement apparatus is built rapidly during a crisis window and then partially relaxes as compliance norms settle. Theater ratio is modest and rising slightly: the coordination function (protecting the commons) is largely genuine, but some portion of enforcement activity (symbolic compliance checks, credential theater) grows over time independent of epidemiological need. All three metrics are authored on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public health authorities), this looks like rope or tangled_rope: a genuine collective-action solution to a real externality problem, with coercion as a regrettable but necessary enforcement cost. From the mandate-resistant seat, the same structure computes as much closer to snare: coercion applied to their employment and service access with their own objection structurally disqualified from counting as harm. The engine's per-seat computation should surface this divergence; this reading does not resolve it by fiat — it simply commits, as a matter of framing, to the vulnerable-commons victim accounting.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised populations and healthcare infrastructure are the structural beneficiaries in this reading: the mandate exists, by its own justification, to protect them, and they bear no compliance burden themselves. Mandate-resistant workers are the structural targets: they bear the coercive cost (employment loss, service exclusion) directly and, in this reading, are denied standing as victims because their objection is recast as an externality claim rather than a rights claim. This is the defining directionality move of the public_health_primary reading — it is not an error or oversight but the reading's core structural commitment, and it is precisely what the sibling bodily_autonomy_primary reading inverts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (externality of individual non-participation onto those dependent on herd effects and shared capacity) remains empirically live wherever coverage drops and capacity is finite — this is not a resolved mandatrophy case. But the specific mandate STRUCTURE (duration, employment coercion, service exclusion) can outlive the acute threat that justified it, which is exactly the terrain the sibling proportionality_reading is built to adjudicate and this reading, by design, does not weigh.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victim_set_boundary_contested_across_readings,
    'Should the mandate-resistant be counted among the constraint''s victims (as bodily_autonomy_primary holds) or excluded as externality-imposing free-riders (as this reading holds)?',
    'This is not empirically resolvable from within either reading alone — it depends on a prior normative commitment about whether externality-imposition forfeits standing as a rights-bearer. The kernel itself does not adjudicate between readings; each reading answers it differently by construction.',
    'If the mandate-resistant are counted as victims, this constraint''s structure converges toward the bodily_autonomy_primary reading''s snare-leaning classification. Under this reading''s own terms, they are excluded from victims, which is what keeps this reading''s classification in tangled_rope territory rather than snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary_contested_across_readings, preference, 'Whether externality-imposition disqualifies the mandate-resistant from victim status — the core structural fork between this reading and bodily_autonomy_primary.').

omega_variable(
    proportionality_calibration_unaddressed,
    'Is the specific mandate (its duration, its coercive mechanisms, its alternatives) proportionate to the threat it addresses, independent of whether the commons-protection framing is accepted?',
    'Sliding-scale assessment of threat severity over time, availability of less coercive alternatives (testing, voluntary programs), and duration of the mandate relative to the acute risk window — this is exactly the proportionality_reading''s domain and is left unresolved here.',
    'If the mandate persists well past the acute threat window without recalibration, this reading''s own founding-problem justification weakens even though the commons-protection framing itself is not in question — a mandatrophy risk internal to this reading, not a challenge to its victim-set logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calibration_unaddressed, conceptual, 'Whether mandate duration/mechanism proportionality is being tracked, distinct from the more fundamental victim-set question this reading takes as settled.').

omega_variable(
    herd_effect_threshold_uncertainty,
    'At what population compliance level does the herd-protection function this reading relies on actually hold, and how sharply does immunocompromised risk rise below that threshold?',
    'Epidemiological modeling and observed outbreak data in populations with varying compliance rates; direct measurement of hospitalization/mortality among immunocompromised populations correlated with local compliance levels.',
    'If the threshold is high and fragile, the coordination function this reading claims is more precarious than assumed, strengthening the case for coercive enforcement; if the protective effect degrades gracefully, the case for coercion (versus voluntary measures) weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_effect_threshold_uncertainty, empirical, 'Empirical uncertainty in the herd-immunity threshold underlying the coordination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.14).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__public_health_primary, theater_ratio, 18, 0.24).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.22).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__public_health_primary, theater_ratio, 30, 0.22).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__public_health_primary, theater_ratio, 36, 0.22).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__public_health_primary, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__public_health_primary, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__public_health_primary, base_extractiveness, 36, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__public_health_primary, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__public_health_primary, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__public_health_primary, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the public_health_mandate_authority kernel, each authored as a separate constraint with its own epsilon and victim set per the ε-invariance principle. public_health_primary (this story) places the immunocompromised among beneficiaries/conditional-victims and excludes the mandate-resistant from the victim set. bodily_autonomy_primary inverts this: the mandate-resistant are the primary victims and no collective benefit is treated as capable of justifying non-consensual intervention. proportionality_reading declines to commit categorically to either victim-set boundary, instead making legitimacy a function of threat severity, alternatives, coercion magnitude, and duration. All three are linked via affects_constraints to preserve the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
