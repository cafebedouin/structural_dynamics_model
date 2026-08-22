% ============================================================================
% CONSTRAINT STORY: dignity_kernel__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__autonomy_rights_reading, []).

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
 *   constraint_id: dignity_kernel__autonomy_rights_reading
 *   human_readable: Autonomy/Rights Reading of the Dignity Kernel — AI Governance Application
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   This story instantiates the autonomy/rights reading of the contested
 *   dignity kernel as it operates specifically in AI governance: transparency
 *   mandates, consent architectures, algorithmic accountability rules, and
 *   labor/privacy protections all premised on the claim that human dignity is
 *   grounded in autonomy, rationality, and rights rather than in an unearned,
 *   capacity-independent status (imago Dei) or in an open-ended enhancement
 *   trajectory (posthumanism). The reading has real coordination value — it
 *   lets pluralistic, religiously diverse polities build enforceable AI rules
 *   without settling metaphysics — but it also creates an asymmetric
 *   extraction structure: developers and platforms who can formally satisfy
 *   consent/disclosure requirements capture the benefits of 'rights
 *   compliance' as a market and legal shield, while workers and data subjects
 *   whose consent is nominal rather than real continue to bear harm, and
 *   persons outside the paradigm of full rational agency are left with
 *   indirectly-derived, weaker protections. This story authors ONLY the
 *   autonomy/rights reading; the imago Dei and posthumanist readings are
 *   separate constraints with their own ε and stakeholder structures, linked
 *   via network.affects_constraints, not folded into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, 0.52).
domain_priors:suppression_score(dignity_kernel__autonomy_rights_reading, 0.38).
domain_priors:theater_ratio(dignity_kernel__autonomy_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__autonomy_rights_reading, "Autonomy/Rights Reading of the Dignity Kernel — AI Governance Application").
narrative_ontology:topic_domain(dignity_kernel__autonomy_rights_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__autonomy_rights_reading, '821d6de0-9f05-474f-a010-c204ed9ad67e').
narrative_ontology:cs_kernel_codification('821d6de0-9f05-474f-a010-c204ed9ad67e', distributed).
narrative_ontology:cs_authority_grounding('821d6de0-9f05-474f-a010-c204ed9ad67e', distributed).
narrative_ontology:cs_reading_relation('821d6de0-9f05-474f-a010-c204ed9ad67e', dignity_kernel__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('821d6de0-9f05-474f-a010-c204ed9ad67e', dignity_kernel__posthumanist_reading, influences).
narrative_ontology:cs_axiom('821d6de0-9f05-474f-a010-c204ed9ad67e', foundational, dignity_grounded_in_rational_autonomous_agency).
narrative_ontology:cs_axiom_status(dignity_grounded_in_rational_autonomous_agency, holdable).
narrative_ontology:cs_axiom_grounding('821d6de0-9f05-474f-a010-c204ed9ad67e', dignity_grounded_in_rational_autonomous_agency, deontological).
narrative_ontology:cs_axiom('821d6de0-9f05-474f-a010-c204ed9ad67e', secondary, rights_claims_require_no_theological_premise).
narrative_ontology:cs_axiom_status(rights_claims_require_no_theological_premise, holdable).
narrative_ontology:cs_axiom_grounding('821d6de0-9f05-474f-a010-c204ed9ad67e', rights_claims_require_no_theological_premise, conventional).
narrative_ontology:cs_reference_frame('821d6de0-9f05-474f-a010-c204ed9ad67e', enlightenment_rational_agent_framework).
narrative_ontology:cs_drift_state('821d6de0-9f05-474f-a010-c204ed9ad67e', contemporary_ai_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('821d6de0-9f05-474f-a010-c204ed9ad67e', '').
narrative_ontology:cs_kernel_id(dignity_kernel__autonomy_rights_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, rights_compliant_ai_developers).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, autonomy_respecting_platforms).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, gig_platform_workers_under_opaque_algorithms).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, surveilled_data_subjects).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, cognitively_impaired_persons_outside_rational_agency_frame).
narrative_ontology:constraint_vindicates(dignity_kernel__autonomy_rights_reading, liberal_individual_rights_doctrine).
narrative_ontology:constraint_vindicates(dignity_kernel__autonomy_rights_reading, rational_agency_as_moral_status_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce transparency, accountability, and consent-based AI governance rules premised on protecting autonomous, rational agents from opaque or coercive systems. They set disclosure requirements, audit obligations, and labor/privacy protections, and can sanction noncompliant deployers.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Build compliance into products early, gaining market advantage, regulatory goodwill, and reduced litigation exposure. The autonomy/rights frame gives them a clear, litigable standard they can engineer toward and market as a trust signal.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, rights_compliant_ai_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Are managed by algorithmic scheduling and rating systems whose logic is not disclosed to them. The autonomy/rights framework nominally protects their consent and self-determination, but enforcement is slow, complaint-driven, and asymmetric against platform legal teams; in practice they absorb the harm while awaiting remedy.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, gig_platform_workers_under_opaque_algorithms, payer,
    powerless, immediate, constrained, national).

% Have personal data harvested by AI systems whose consent mechanisms are formally rights-compliant (clickwrap, disclosures) but practically unavoidable if they wish to participate in modern economic and social life. The rights frame gives them standing to complain but not real alternatives.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, surveilled_data_subjects, payer,
    powerless, immediate, trapped, global).

% Persons whose capacity for autonomous, rational choice is diminished or contested (severe cognitive disability, advanced dementia, infancy) sit awkwardly inside a dignity framework keyed to rational agency. Protections for them are derived indirectly (via guardianship, extended personhood doctrine) rather than grounded directly, and AI systems built around autonomy-consent logic can misfire when applied to them.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, cognitively_impaired_persons_outside_rational_agency_frame, excluded,
    powerless, biographical, trapped, national).

% Industry bodies and standards consortia that codify what counts as adequate transparency and consent, gaining influence over the operational meaning of the rights standard and shaping enforcement expectations in their favor.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, autonomy_respecting_platforms, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__autonomy_rights_reading, autonomy_respecting_platforms, agenda_setter).

% Analyze whether the autonomy/rights grounding coherently protects the full range of human persons, or whether it structurally underprotects those without full rational agency while overprotecting formally-consenting but practically-coerced adults.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, bioethics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__autonomy_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(dignity_kernel__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a secular, pluralistically acceptable, and legally tractable standard for governing AI systems — transparency, consent, and accountability — that does not require agreement on any particular metaphysical or theological anthropology, enabling broad multi-stakeholder regulatory coordination.
% TRANSFER_FUNCTION: Moves compliance costs and reputational capital toward developers and platforms that can operationalize rights-based standards (disclosure regimes, audit trails, consent architectures), while moving unremedied harms toward workers and data subjects whose formal consent options do not track their real bargaining power; also moves moral-status certainty away from persons whose capacities fall outside the rational-agency paradigm.
% ABSENT_VOICES: Persons with profound cognitive disability or diminished capacity are largely absent from the standard-setting process itself — their interests are represented indirectly through guardians, disability advocates, or omitted altogether from the design assumptions of 'the autonomous rational agent' that AI consent architectures presuppose. Gig workers and data subjects are formally consulted through public comment processes but rarely have resources comparable to industry counsel.
% DISAPPEARANCE_RATIONALE: If the autonomy/rights grounding for dignity vanished from AI governance overnight, current transparency mandates, consent regimes, and labor-protection rules premised on protecting self-determining rational agents would lose their normative anchor; regulators would need to rebuild justification from a different anthropology (e.g., imago Dei's equal-worth-regardless-of-capacity, or a posthumanist capacities-maximization frame), materially changing which harms count as violations and who has standing to claim them.
% FOUNDING_PROBLEM: Post-Enlightenment secular states needed a basis for universal human rights and protections that did not depend on contested religious premises, while still grounding strong claims against exploitation, coercion, and instrumentalization — autonomy and rationality were offered as a neutral, philosophically defensible ground reachable by public reason.
% FOUNDING_PROBLEM_CORROBORATION: Secular human-rights theorists and constitutional courts continue to treat the founding problem as live — pluralistic societies still need non-sectarian grounds for rights claims. Disability-rights scholars and theologians (including outside the autonomy-rights tradition, e.g. imago Dei theorists and capabilities-approach philosophers such as those following Nussbaum) corroborate a competing status assessment: they argue the rational-agency grounding was never fully adequate to protect persons with diminished capacity and that its gaps have become more visible, not less, as AI systems formalize consent and agency as binary switchable properties.
narrative_ontology:disappearance_verdict(dignity_kernel__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(dignity_kernel__autonomy_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__autonomy_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignity_kernel__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__autonomy_rights_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__autonomy_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignity_kernel__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.52) and rising over the interval: as AI systems proliferate, formally rights-compliant consent mechanisms increasingly function as liability shields rather than substantive protections, so the gap between nominal compliance and actual autonomy protection widens. Suppression is moderate (0.38) — the mechanism is not coercive in the classic sense but relies on the practical unavailability of exit from data-driven digital life to convert 'consent' into something closer to compulsion. Theater ratio rises modestly (0.28 by endpoint) as compliance documentation increasingly substitutes for verified outcomes (Goodhart drift in audit and disclosure regimes). Accessibility collapse is moderate (0.4): meaningful alternatives to 'accept these terms or exit the digital economy' have narrowed but not vanished. Resistance is moderate-high (0.55): labor organizing, disability advocacy, and privacy litigation actively contest the standard's adequacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies and rights-compliant developers sit near the beneficiary end: agencies gain a workable enforcement standard, developers gain a certifiable compliance target and market differentiation. Gig workers and surveilled data subjects sit near the target end: their formal rights exist on paper but their exit options are constrained-to-trapped, so 'consent' extracts compliance from them without delivering commensurate protection. Cognitively impaired persons are structurally excluded from the frame's core justification (rational agency) even though the frame's institutions nominally extend protection to them via derivative doctrines — this is why they appear as excluded rather than simply as victims: the harm is exclusion from the grounding logic itself, not primarily extraction through it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing a pluralistically acceptable, non-sectarian ground for universal rights — remains partly live (contested pluralistic societies still need shared normative vocabulary) but the specific application to AI governance shows signs of mandate drift: the standard was built to prevent instrumentalization of persons, yet its operational form (checkbox consent, disclosure documents) increasingly enables the very instrumentalization it was meant to prevent, now dressed in rights-compliant language. This is not full mandatrophy (the coordination function is not dead) but the tangled_rope classification reflects that both a genuine coordination function and an asymmetric extraction channel run through the same structure simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_agency_boundary_problem,
    'Does grounding dignity in autonomy and rationality structurally underprotect persons whose rational agency is diminished, contested, or absent (severe cognitive disability, infancy, advanced dementia), compared to a capacity-independent grounding?',
    'Comparative outcome analysis: track enforcement and protection outcomes for cognitively impaired persons under autonomy/rights-grounded AI regulation versus jurisdictions or doctrines using capacity-independent (e.g. imago Dei-influenced or capabilities-approach) grounding for the same population.',
    'If the autonomy/rights frame systematically produces weaker protections for this population, it supports the excluded classification and suggests the reading''s coordination function is narrower than its universal-rights framing claims; if outcomes are comparable, the derivative-protection mechanisms are adequate and the exclusion is more formal than substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_agency_boundary_problem, empirical, 'Whether autonomy-grounded dignity structurally underprotects those outside full rational agency.').

omega_variable(
    consent_as_extraction_cover,
    'Is formal rights-compliant consent (disclosures, clickwrap, algorithmic transparency reports) functioning as genuine protection of autonomy, or as a legally sufficient cover story that legitimizes practically unavoidable data extraction and algorithmic management?',
    'Behavioral and legal analysis: measure rates of meaningful opt-out exercised versus rates of formal consent given under conditions of practical necessity (e.g., no viable alternative platform, employment dependent on app-based scheduling).',
    'If opt-out rates are near zero despite documented dissatisfaction, the consent architecture is functioning as extraction cover, supporting classification of the arrangement''s operational form as more extractive than its stated coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_as_extraction_cover, empirical, 'Whether formal consent mechanisms substantively protect autonomy or merely legitimize extraction.').

omega_variable(
    kernel_framing_alternative_grounding,
    'Would selecting the imago_dei_reading or posthumanist_reading as the operative AI-governance grounding, rather than the autonomy/rights_reading, change which harms are legally cognizable and which agents have standing?',
    'Comparative doctrinal analysis across the three sibling readings'' implied legal architectures — this is documented as a conceptual omega because it depends on framing choice, not empirical resolution.',
    'Under imago_dei_reading, protections for cognitively impaired persons would likely be stronger and non-derivative; under posthumanist_reading, enhancement-related harms currently treated cautiously might be reframed as flourishing rather than risk. The choice of reading is itself contested and this story deliberately generates only the autonomy/rights reading per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_grounding, conceptual, 'How kernel-reading choice would alter AI governance''s legal architecture and standing rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__autonomy_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignity_kernel__autonomy_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dign_tr_t8, dignity_kernel__autonomy_rights_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(dign_tr_t16, dignity_kernel__autonomy_rights_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(dign_tr_t24, dignity_kernel__autonomy_rights_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(dign_tr_t32, dignity_kernel__autonomy_rights_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(dign_tr_t40, dignity_kernel__autonomy_rights_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignity_kernel__autonomy_rights_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(dign_be_t8, dignity_kernel__autonomy_rights_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(dign_be_t16, dignity_kernel__autonomy_rights_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(dign_be_t24, dignity_kernel__autonomy_rights_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(dign_be_t32, dignity_kernel__autonomy_rights_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(dign_be_t40, dignity_kernel__autonomy_rights_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignity_kernel__autonomy_rights_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(dign_su_t8, dignity_kernel__autonomy_rights_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(dign_su_t16, dignity_kernel__autonomy_rights_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(dign_su_t24, dignity_kernel__autonomy_rights_reading, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(dign_su_t32, dignity_kernel__autonomy_rights_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(dign_su_t40, dignity_kernel__autonomy_rights_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__autonomy_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__imago_dei_reading).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__posthumanist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of dignity_kernel, decomposed per the epsilon-invariance principle: autonomy_rights_reading (this file, tangled_rope — genuine pluralistic-coordination function plus asymmetric extraction via nominal-consent mechanisms), imago_dei_reading (separate file — capacity-independent equal worth grounding, different victim set and likely different classification), and posthumanist_reading (separate file — enhancement-affirming grounding, different beneficiary/victim structure entirely). Each carries its own epsilon, beneficiaries, victims, and claimed_type; they are linked here rather than merged because measuring 'the dignity kernel' by different observables (capacity-based vs. status-based vs. trajectory-based) yields materially different extraction profiles — exactly the ambiguity the epsilon-invariance test is designed to catch and decompose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
