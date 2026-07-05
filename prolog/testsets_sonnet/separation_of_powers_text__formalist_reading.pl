% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Separation of Powers — Nondelegation Reading
 *   domain: constitutional_law/administrative_law
 *
 * SUMMARY:
 *   This story instantiates the formalist reading of the separation-of-powers
 *   kernel: legislative and executive power occupy strictly bounded,
 *   non-overlapping categories, and any statute that hands an agency
 *   discretion to fill in a general standard is a forbidden delegation of
 *   legislative power unless cabined by an unusually determinate rule. Under
 *   this reading, the administrative state's ordinary operating mode — broad
 *   statutory mandates implemented through agency rulemaking — becomes
 *   structurally suspect. Agencies enter the victim set because their
 *   statutory authority is rendered persistently litigable; regulated
 *   incumbents and formalist courts benefit from the resulting leverage and
 *   doctrinal control. This is a deliberately narrow, single-reading story;
 *   the functionalist reading (which treats overlapping authority and
 *   intelligible-principle delegation as legitimate) and the
 *   unitary-executive reading (which contests a different axis —
 *   intra-executive control rather than the legislative/executive boundary)
 *   are separate constraints with their own ε values, linked here only
 *   through the kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Separation of Powers — Nondelegation Reading").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, 'f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317').
narrative_ontology:cs_kernel_codification('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', fixed_text).
narrative_ontology:cs_authority_grounding('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', lineage).
narrative_ontology:cs_interpretation_layer_present('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317').
narrative_ontology:cs_reading_relation('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', foundational, legislative_executive_categorical_impermeability).
narrative_ontology:cs_axiom_status(legislative_executive_categorical_impermeability, holdable).
narrative_ontology:cs_axiom_grounding('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', legislative_executive_categorical_impermeability, deontological).
narrative_ontology:cs_axiom('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', secondary, intelligible_principle_standard_insufficient_safeguard).
narrative_ontology:cs_axiom_status(intelligible_principle_standard_insufficient_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', intelligible_principle_standard_insufficient_safeguard, conventional).
narrative_ontology:cs_reference_frame('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', founding_era_categorical_separation).
narrative_ontology:cs_drift_state('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', post_administrative_state_expansion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f0a54ea9-bd20-4ddc-9ed8-ba79a9c74317', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congressional_incumbents).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, regulated_industry_incumbents).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, formalist_judiciary).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, environmental_health_beneficiaries).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulatory_dependent_workers).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, vesting_clause_textual_supremacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, nondelegation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates the boundary between legislative and executive power by reading the vesting clauses as categorical grants that cannot be blended. Enforces the boundary by striking delegations that lack a sufficiently determinate rule of decision, or by reviving nondelegation as a live check. Sets the doctrine's content case by case and controls how strictly the intelligible-principle standard is actually applied.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a doctrine that forces controversial policy choices back onto the legislative floor, letting incumbents claim credit for popular statutory language while blaming agencies for unpopular implementation, or alternatively benefit from being able to block agency action entirely by starving delegated authority. Can route around the constraint through appropriations riders and committee oversight regardless of the formal doctrine.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congressional_incumbents, beneficiary,
    institutional, biographical, arbitrage, national).

% Gain a durable litigation weapon: any agency rule addressing emissions, workplace safety, or financial conduct can be challenged as an unconstitutional delegation, creating years of regulatory uncertainty that favors incumbents with compliance infrastructure over new entrants and public-interest regulators. They finance the litigation that develops and hardens the doctrine.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industry_incumbents, beneficiary,
    organized, generational, mobile, national).

% Operate under statutes that necessarily contain open-textured standards because Congress cannot anticipate every technical contingency (drug safety thresholds, spectrum allocation, systemic financial risk). Under the formalist reading, this ordinary delegation of implementing detail becomes constitutionally suspect. Agencies cannot exit the constraint — their entire operating authority derives from the statutes now rendered fragile — and can only respond by narrowing rules, adding cumbersome notice-and-comment layers, or awaiting congressional re-authorization that rarely comes.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, biographical, trapped, national).

% Communities relying on agency-set air quality, water safety, and workplace exposure standards bear the cost when those standards are struck, delayed, or preemptively narrowed to survive nondelegation challenges. They have no direct voice in the litigation and no capacity to relocate away from the pollution or hazard the vacated rule would have addressed.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, environmental_health_beneficiaries, payer,
    powerless, generational, trapped, national).

% Depend on OSHA, NLRB, and similar agency rulemaking for workplace protections. When agency standards are vacated or agencies self-limit rulemaking ambition to avoid nondelegation exposure, protections weaken; workers cannot bargain around a missing federal floor and lack resources to litigate the underlying constitutional theory themselves.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulatory_dependent_workers, payer,
    powerless, biographical, constrained, national).

% Study whether the formalist boundary reading is historically continuous with founding-era practice or a modern reconstruction; testify in litigation and write amicus briefs read by the judiciary that sets the doctrine's actual content.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, diffuse).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents Congress from wholesale abdicating its lawmaking responsibility to unaccountable executive-branch actors, preserving a textual accountability chain from voters to legislators to law.
% TRANSFER_FUNCTION: Moves policymaking authority and litigation leverage from agencies and the constituencies they protect toward regulated incumbents and the judiciary that adjudicates the boundary; moves compliance costs and regulatory uncertainty onto agencies and diffuse public beneficiaries of regulation.
% ABSENT_VOICES: The communities and workers who rely on agency rules for health, safety, and economic protection have no seat in nondelegation litigation, which is typically brought by regulated firms against the agency; their interests are represented, if at all, by the agency itself, which is also the defendant whose authority is being narrowed.
% DISAPPEARANCE_RATIONALE: If the formalist boundary reading were abandoned overnight, agencies would face far less litigation risk in issuing technically detailed rules, Congress could delegate broad standard-setting authority with less concern for judicial invalidation, and the balance of practical policymaking power would shift back toward agencies and away from courts and regulated-industry litigants who currently use nondelegation challenges as a check.
% FOUNDING_PROBLEM: The founding-era problem was preventing a legislature from creating an unaccountable executive dictator by handing over its lawmaking function wholesale — a genuine concern rooted in anti-monarchical and anti-consolidation commitments.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the administrative state (outside both the regulated-industry beneficiaries and the agencies being constrained) document that broad delegations with general standards existed from the earliest Congresses and that the strict formalist boundary was rarely enforced by courts until a modern revival beginning in the late twentieth century; this scholarship supports a 'contested' rather than 'live' reading of whether the original founding problem, as opposed to a reconstructed version of it, is what the current doctrine actually addresses.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the measured interval (0.30 to 0.68) tracking the doctrine's revival: nondelegation challenges were largely dormant for most of the twentieth century and have intensified as a litigation strategy in recent decades, so early time points show low extraction and late time points show the doctrine's modern bite. Suppression tracks the same arc — as courts signal receptivity to formalist challenges, agencies preemptively narrow rulemaking ambition, which is itself a suppression effect distinct from any single case outcome. Theater ratio is moderate: real constitutional argument is happening, but the doctrine's on-and-off application (rarely actually invalidating major delegations even while being invoked constantly to threaten them) means a rising share of its function is deterrent posture rather than actual boundary enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the formalist judiciary's seat, this is principled constitutional restoration — enforcing a boundary the founders intended and that decades of administrative convenience eroded. From the agency and public-beneficiary seats, the identical doctrine operates as an enforced transfer of policymaking leverage to litigants who can afford constitutional challenges, with regulatory protection as the currency moved. The engine should compute these as different per-seat classifications from the same structural facts, not reconcile them to one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional incumbents and regulated industry are structural beneficiaries with mobile or arbitrage-level exit — they can route around the constraint via appropriations, oversight, or forum-shopping litigation, so their effective extraction sits near the beneficiary end. Administrative agencies are trapped targets: their authority derives entirely from statutes now rendered fragile, and they cannot exit the constitutional framework they operate within. Diffuse public beneficiaries of regulation (environmental health beneficiaries, regulatory-dependent workers) are powerless, trapped or constrained targets who bear the downstream cost of narrowed or vacated rules without any voice in the litigation that produces that outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing a fully unaccountable executive lawmaker) is genuinely old and was genuinely a coordination concern at the founding. But the current formalist doctrine, applied to an administrative state whose scale and technical character the founders could not have anticipated, arguably persists past the point its narrow founding function addresses — courts rarely actually invalidate major statutes on nondelegation grounds even while using the threat to reshape agency behavior, which is the marker of a doctrine functioning more as leverage than as active boundary enforcement in most disputes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_reading_historical_continuity,
    'Is the formalist nondelegation boundary a genuine recovery of founding-era constitutional structure, or a modern doctrinal reconstruction dressed in originalist language?',
    'Comparative historical analysis of early Congresses'' delegation practices (customs enforcement discretion, land office standards, militia calls) against the strictness of the modern formalist standard; if early delegations were routinely as broad as those the modern doctrine would strike, the formalist reading''s claim to historical fidelity weakens.',
    'If the formalist reading is a reconstruction rather than a recovery, its claimed_type as principled boundary-enforcement is undermined and the tangled_rope classification (genuine anti-consolidation coordination function coexisting with incumbent-capture extraction) is the more defensible read; if it is a genuine recovery, the coordination function is stronger relative to the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_reading_historical_continuity, conceptual, 'Whether the formalist boundary is historically continuous or a modern construction.').

omega_variable(
    kernel_reading_selection_pressure,
    'Which structural signals in a given case push courts toward the formalist reading rather than the functionalist or unitary-executive reading of the same constitutional text?',
    'Track which reading prevails as a function of the political valence of the regulatory action being challenged and the composition of the reviewing court; if reading selection correlates with outcome-favorability to a particular litigant class rather than with neutral doctrinal factors, the kernel is being read strategically rather than resolved on textual grounds.',
    'If reading selection is outcome-driven, the choice of formalist reading in any specific case is itself part of the extraction mechanism rather than a neutral interpretive event, which would raise the effective suppression this story attributes to the doctrine''s operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether formalist-reading invocation correlates with litigant identity rather than neutral textual factors.').

omega_variable(
    agency_beneficiary_status_ambiguity,
    'Should administrative agencies themselves be classified purely as victims, given that agency leadership and career staff also derive institutional power, budget, and mission from the very regulatory authority being constrained?',
    'Examine whether agency behavior under nondelegation threat shows genuine capacity loss (rules never issued, protections never extended) versus institutional self-preservation (agencies narrowing rules just enough to survive review while retaining core authority).',
    'If agencies substantially preserve authority through adaptive drafting, the victim classification should be read as primarily accruing to the public beneficiaries of foregone regulation rather than to the agencies as institutions, which are more accurately intermediate parties bearing compliance cost rather than pure victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_beneficiary_status_ambiguity, empirical, 'Whether agencies are pure victims or partially self-preserving intermediate parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sepa_tr_t8, separation_of_powers_text__formalist_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(sepa_tr_t16, separation_of_powers_text__formalist_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(sepa_tr_t24, separation_of_powers_text__formalist_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(sepa_tr_t32, separation_of_powers_text__formalist_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__formalist_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sepa_be_t8, separation_of_powers_text__formalist_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(sepa_be_t16, separation_of_powers_text__formalist_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(sepa_be_t24, separation_of_powers_text__formalist_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(sepa_be_t32, separation_of_powers_text__formalist_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__formalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sepa_su_t8, separation_of_powers_text__formalist_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(sepa_su_t16, separation_of_powers_text__formalist_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(sepa_su_t24, separation_of_powers_text__formalist_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(sepa_su_t32, separation_of_powers_text__formalist_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__formalist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints reading the shared separation_of_powers_text kernel. formalist_reading (this story) treats the legislative/executive boundary as categorical and strict, generating a high-ε constraint with agencies as victims. functionalist_reading treats the same boundary as flexible and permits intelligible-principle delegation, generating a substantially lower ε because it validates rather than invalidates ordinary agency rulemaking. unitary_executive_reading addresses a structurally distinct axis (intra-executive control over independent agencies) rather than the legislative/executive nondelegation boundary, and is only loosely coupled to this story through shared reliance on formalist interpretive method. All three are linked via network edges rather than merged into one constraint, per the ε-invariance principle: the formalist and functionalist readings of the identical constitutional text produce different ε values and different victim sets, which means they are structurally different constraints, not one constraint measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
