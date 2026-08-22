% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the JUDICIAL SUPREMACY READING of the
 *   constitutional interpretive authority kernel. The core premise is that
 *   courts possess final authority to interpret the constitution and nullify
 *   legislative acts deemed unconstitutional. This reading legitimates
 *   judicial power through rights-compliance language: judges are framed as
 *   guardians protecting fundamental rights against majoritarian
 *   encroachment, not as political actors wielding power. The constraint's
 *   operation extracts interpretive authority from the legislature and
 *   electoral process, subordinates democratic will to constitutional
 *   doctrine, and imposes suppressive force on legislatures attempting to
 *   reclaim interpretive space. The measured metrics reflect the reading as
 *   understood and practiced: high extractiveness (0.68 terminal), high
 *   suppression (0.71), and moderate-rising theater (0.42) indicating the
 *   proportion of contemporary judicial activity devoted to legitimating
 *   doctrine rather than resolving novel constitutional questions. This
 *   reading coexists with and influences the other sibling readings; it
 *   forecloses neither of them logically (each remains a live position held
 *   by different actors), but this reading's dominance in institutional
 *   practice creates structural pressure on the others.
 *
 * KEY AGENTS:
 *   - judiciary — institutional agenda-setter, defines constitutionality, wields nullification power
 *   - legislature — institutional payer, constrained by judicial doctrine, subordinated politically
 *   - electoral_majority — moderate payer, democratic will overridden by judicial doctrine
 *   - rights_bearers — powerless beneficiaries, protected by broad judicial rights recognition
 *   - executive_branch — secondary beneficiary, controls judicial appointments and doctrine-shaping
 *   - excluded_political_parties — moderate, structurally outside judiciary, constrained by courts they did not appoint
 *   - constitutional_scholars — analytical observer, shapes interpretive frameworks courts adopt
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.71).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, 'a9a4e65a-09e6-4ae8-afc2-555055b1a0c9').
narrative_ontology:cs_kernel_codification('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', formalized).
narrative_ontology:cs_authority_grounding('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', lineage).
narrative_ontology:cs_interpretation_layer_present('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9').
narrative_ontology:cs_reading_relation('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', constitutional_interpretive_authority__coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', foundational, unelected_judges_protect_fundamental_rights).
narrative_ontology:cs_axiom_status(unelected_judges_protect_fundamental_rights, holdable).
narrative_ontology:cs_axiom_grounding('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', unelected_judges_protect_fundamental_rights, deontological).
narrative_ontology:cs_axiom('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', foundational, constitutional_limits_superior_to_legislative_will).
narrative_ontology:cs_axiom_status(constitutional_limits_superior_to_legislative_will, holdable).
narrative_ontology:cs_axiom_grounding('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', constitutional_limits_superior_to_legislative_will, deontological).
narrative_ontology:cs_reference_frame('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', judicial_guardian_authority).
narrative_ontology:cs_drift_state('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', contemporary_contested_doctrine, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9a4e65a-09e6-4ae8-afc2-555055b1a0c9', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_bearers).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, fundamental_rights_override_majority).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_limits_on_state_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wields final interpretive authority over the constitution by nullifying legislative acts deemed unconstitutional. Justifies this power as guardianship of fundamental rights and constitutional limits on state power. The judicial seat controls what 'constitutional' means in practice; legislative attempts to redefine or circumvent judicial doctrine trigger further strikes-down. Judges are appointed (not elected) and tenure-protected, insulating them from direct electoral pressure.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, trapped, national).

% Drafts and enacts laws, but every legislative act risks judicial nullification if courts deem it unconstitutional. The legislature must anticipate and work around judicial doctrine, invest substantial political capital defending legislation in court, and accept defeats when judges strike down democratically passed laws. Its power is subordinated to judicial interpretation of constitutional limits.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, generational, constrained, national).

% Elects representatives to the legislature and expects those representatives to enact the policies the majority supports. When a court nullifies a democratically passed law, the majority's expressed will is overridden by unelected judges invoking constitutional doctrine. The majority's recourse is slow (constitutional amendment requires supermajority consent, judicial appointments require the legislature to work with the executive) and imperfect.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majority, payer,
    moderate, biographical, constrained, national).

% Individuals and groups whose rights are recognized and protected by court doctrine. When the judiciary interprets fundamental rights broadly, rights-bearers gain legal protections against legislative majoritarian encroachment. They benefit from judicial strikes-down of laws that would violate their rights, even when those laws have majority support. Their exit is geographic (moving to a jurisdiction with stronger rights protection) or exit via political organizing to shift judicial doctrine.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_bearers, beneficiary,
    powerless, biographical, mobile, national).

% Appoints judges and nominates higher-court justices. When the executive controls the appointment process, it can seed the judiciary with ideologically aligned jurists who interpret the constitution consonant with the executive's preferences. The executive also submits its own actions to judicial review, but the dynamics of this constraint primarily pit the executive's appointment power against the legislature's attempt to enact policy that the judiciary might strike down. The executive benefits from judicial nullification of hostile legislation and pays costs when courts strike down executive action.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, payer).

% Political parties that lack control of the executive appointment power are excluded from formal influence over judicial doctrine. When opposing parties control the courts, these parties face judicially enforced constraints on their legislative agenda even when they command electoral majorities. They would argue for coordinate construction or parliamentary supremacy if admitted to the framing; their exclusion from the judiciary-legislature negotiation is itself the enforcement mechanism.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, excluded_political_parties, excluded,
    moderate, biographical, constrained, national).

% Analyze the constraint's operation and compete to influence judicial doctrine through legal scholarship, amicus briefs, and academic networks. They sit outside the formal authority structure but shape the interpretive frameworks courts use. Their analytical seat permits observation of how judicial supremacy is legitimated and how doctrine evolves.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, binding framework for resolving disagreements about constitutional boundaries: rather than letting every legislature individually interpret what the constitution permits, a centralized judicial authority interprets the constitution uniformly across jurisdictions, providing a stable reference point for rights-holders and state actors alike.
% TRANSFER_FUNCTION: Transfers interpretive authority from the legislative/electoral process to the judiciary; transfers the power to nullify laws from elected representatives to appointed judges; transfers appeals against majoritarian will from the political process to constitutional litigation; transfers legitimacy from electoral mandate to rights-compliance doctrine.
% ABSENT_VOICES: Legislative majorities and electoral coalitions that would argue that final interpretive authority should remain with the people's elected representatives, or should be negotiated across branches rather than monopolized by the courts. These voices are structurally excluded because the judiciary defines what counts as 'constitutional' and can strike down legislative attempts to reclaim interpretive authority. Coordinate-construction advocates and parliamentary-supremacy theorists would argue for fundamentally different arrangements but have no institutional seat in this reading's framework.
% DISAPPEARANCE_RATIONALE: If judicial nullification power vanished, legislatures would immediately experiment with policies courts had previously struck down, the scope of recognized rights would shift to whatever the legislature permits, majoritarian constraint on minority rights would intensify, and constitutional meaning would become fragmented across legislatures rather than unified by courts.
% FOUNDING_PROBLEM: Early constitutional systems lacked a reliable mechanism to constrain governmental power overreach and protect individual rights against majoritarian encroachment; legislatures claimed ultimate interpretive authority and could unilaterally redefine constitutional limits to expand their own powers.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of judicial supremacy attest the problem remains live: without judicial review, legislatures routinely erode rights and expand power. Advocates of parliamentary supremacy attest the problem has been displaced: judicial nullification itself became the rights-violator, and majoritarian will is now subordinated to unelected judges' doctrinal assertions. Empirical analysts (constitutional historians and comparative-law scholars outside the benefiting parties) document that the founding problem is differently framed by competing readings: the problem is not natural but reading-indexed.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (early history when judicial review was contested and sporadic) to 0.68 (contemporary practice of routine, binding nullification). The plateau at t=35+ reflects stabilization: the constraint's extractive capacity is now normalized, and further intensification would require explicit constitutional override (which remains politically difficult). Suppression tracks similarly: early suppression was moderate (legislatures could contest judicial doctrine), rising to 0.71 as judicial power institutionalized and legislative pushback became futile — courts now successfully suppress legislative attempts to reclaim interpretive authority through substantive doctrine, appointment politics, and legitimation narratives. Theater ratio rises from 0.25 to 0.42 because contemporary judicial activity includes substantial defensive legitimation: opinions spend increasing pages on why judicial review is necessary, justified by rights-protection, and constrained by law — theater that was unnecessary when the power was contested and legitimacy in question. The temporal trajectory models a constraint whose extractive function matured and whose suppressive infrastructure hardened, then stabilized into normalized practice.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary seat and the legislature seat should compute sharply different classifications. From the judiciary's position, the constraint is genuine coordination — it solves the collective-action problem of constitutional uniformity, protects rights-bearers from majoritarian abuse, and operates under law-based constraints (stare decisis, the four-corners interpretive method). From the legislature's position, the same structure is extractive and coercive — the legislature's authority is subordinated to judges who claim interpretive monopoly, nullification is an unpredictable threat to democratic legislation, and the judiciary's 'law-based constraints' are narrative cover for policy-making by another name. The engine derives this divergence from the structural data: the judiciary is the agenda-setter (high d), the legislature is a payer (high d toward victim end), rights-bearers are beneficiaries (low d), the electoral majority is a payer (high d). Directionality per seat drives the per-seat type computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary: d ≈ 0.1–0.2 (beneficiary, trapped exit via appointment politics, institutional power, wields the constraint). Legislature: d ≈ 0.8–0.9 (payer, constrained exit via judicial review and appointment politics, institutional power but subordinated). Electoral majority: d ≈ 0.7–0.8 (payer, constrained exit via the political process and judicial review, moderate power). Rights-bearers: d ≈ 0.2–0.3 (beneficiary, mobile exit via migration or rights-recognition activism, powerless structural position but benefits from judicial doctrine). The high-d seats (legislature, electoral majority) should compute as snare or tangled-rope targets from their position; the low-d seats (judiciary, rights-bearers) should compute as coordination-beneficiaries. The story itself claims tangled_rope (coordination function + asymmetric extraction + active enforcement), and the structural data support that claim: genuine coordination (constitutional uniformity, rights protection) coexists with genuine asymmetric extraction (interpretive authority monopoly, nullification power, subordination of democratic will).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining government power, protecting rights against majoritarian abuse) is LIVE from the judicial supremacy reading's perspective but CONTESTED from the parliament supremacy reading's perspective. The mandate has NOT atrophied: courts still actively protect rights-bearers against majoritarian encroachment, still strike down legislation, still shape doctrine. However, the secondary mandate has shifted: beyond rights-protection, courts now also defend their own interpretive monopoly against legislative attempts to reclaim space. This secondary mandate-drift (from rights-protection to institutional self-preservation) is visible in the rising theater_ratio: contemporary opinions increasingly spend space justifying why nullification itself is constitutional and why legislatures cannot redefine constitutional boundaries. The constraint is not mandatrophy (it has not become pure theater), but it shows signs of mandate-drift: the primary justification (rights protection) remains real, but defensive institutional legitimation is increasingly visible. This is structurally sound for a tangled_rope that has matured and normalized: as the coordination function becomes accepted, enforcement effort shifts from justifying coordination to defending against exit attempts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rights_protection_vs_institutional_authority,
    'Is the measured extractiveness primarily the price of genuine rights-protection coordination, or is it primarily the cost of judges defending institutional authority?',
    'Temporal analysis of judicial doctrine: if rights-protection remains the dominant motivator, extraction should track rights-relevant nullifications; if institutional authority becomes the motivator, extraction should rise relative to rights-relevant cases as courts defend interpretive monopoly against legislative pushback. Compare the ratio of rights-protecting nullifications to institutional-boundary-defending nullifications over the interval.',
    'If rights-protection dominates, the constraint is structurally a tangled_rope whose extraction is the legitimate cost of coordination. If institutional authority dominates, the constraint drifts toward snare (extraction disguised as coordination). The mandate-drift visible in theater_ratio suggests partial shift toward the second; full empirical resolution would clarify the proportion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_vs_institutional_authority, empirical, 'The extent to which judicial extraction serves rights-protection vs. institutional self-preservation.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression structural (legislatures are legally barred from reclaiming interpretive authority by constitutionalist doctrine) or internalized (legislatures have accepted the subordination as legitimate)?',
    'Political behavior analysis: if suppression is structural, legislatures should actively resist and attempt to reclaim authority despite legal barriers; if internalized, legislatures should passively accept the subordination and only contest particular doctrinal outcomes, not the judicial review power itself. Compare legislative resistance to judicial review (constitutional amendment attempts, jurisdiction-stripping proposals, appointment battles) across the interval.',
    'If primarily structural, legislatures could re-establish authority through constitutional change; the barrier is political but not metaphysical. If primarily internalized, legislatures have accepted judicial supremacy as legitimate ordering, and changing it would require not just legal change but a shift in the self-understanding of democracy. The boundary between structural and internalized suppression is where political possibility lies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'The mechanism and reversibility of legislative subordination to judicial authority.').

omega_variable(
    elected_vs_appointed_legitimacy,
    'Does the constraint''s persistence depend on a reading that appoints judges have legitimate authority to override elected representatives, or does it depend on power asymmetry (judges wield power they cannot be directly removed for)?',
    'Normative analysis: survey judges, legislators, and citizens about the source of judicial authority to nullify. If normative legitimacy dominates, judicial authority should persist even when judges face political resistance; if power asymmetry dominates, judicial authority should erode if appointment politics shift (e.g., legislatures gain judiciary-control via long-term appointment dominance).',
    'If legitimacy-grounded, judicial supremacy is robust: institutional actors believe judges should have final say. If power-grounded, judicial supremacy is contingent on appointment politics: whoever controls appointments controls doctrine. This distinction is crucial for understanding whether the reading is a stable equilibrium or a temporary institutional settlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elected_vs_appointed_legitimacy, conceptual, 'The grounding of judicial authority in legitimacy vs. structural power asymmetry.').

omega_variable(
    kernel_reading_contest_structure,
    'Is the judicial supremacy reading a defensible institutional arrangement, or is it one reading of a fundamentally contested kernel where no reading can claim institutional finality?',
    'Comparative constitutional analysis: do other constitutional democracies converge on judicial supremacy, or do they experiment with alternative readings (parliamentary supremacy, coordinate construction)? Do successful democracies without judicial review show rights-protection gaps? Do democracies with judicial review show measurable reduction in majoritarian abuse?',
    'If judicial supremacy is empirically and normatively justified, this reading should show lower extraction when measured as coordination cost for genuine rights-protection. If it is one contestable reading among others, the extraction reflects institutional power-claiming rather than necessary coordination, and the constraint should compute as higher-extraction snare when measured from parliament supremacy or coordinate-construction framings. This omega documents the fundamental contestation at the kernel level: is there one right answer about interpretive authority, or is the answer reading-indexed?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the kernel admits multiple defensible readings or judicial supremacy is the uniquely justified answer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t7, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 7, 0.29).
narrative_ontology:measurement_basis(cons_tr_t7, observed).
narrative_ontology:measurement(cons_tr_t14, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 14, 0.33).
narrative_ontology:measurement_basis(cons_tr_t14, observed).
narrative_ontology:measurement(cons_tr_t21, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 21, 0.38).
narrative_ontology:measurement_basis(cons_tr_t21, observed).
narrative_ontology:measurement(cons_tr_t28, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 28, 0.41).
narrative_ontology:measurement_basis(cons_tr_t28, observed).
narrative_ontology:measurement(cons_tr_t35, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(cons_tr_t35, observed).
narrative_ontology:measurement(cons_tr_t42, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 42, 0.42).
narrative_ontology:measurement_basis(cons_tr_t42, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(cons_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t7, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 7, 0.48).
narrative_ontology:measurement_basis(cons_be_t7, observed).
narrative_ontology:measurement(cons_be_t14, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement_basis(cons_be_t14, observed).
narrative_ontology:measurement(cons_be_t21, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 21, 0.61).
narrative_ontology:measurement_basis(cons_be_t21, observed).
narrative_ontology:measurement(cons_be_t28, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 28, 0.65).
narrative_ontology:measurement_basis(cons_be_t28, observed).
narrative_ontology:measurement(cons_be_t35, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(cons_be_t35, observed).
narrative_ontology:measurement(cons_be_t42, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement_basis(cons_be_t42, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(cons_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t7, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 7, 0.59).
narrative_ontology:measurement_basis(cons_su_t7, observed).
narrative_ontology:measurement(cons_su_t14, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 14, 0.63).
narrative_ontology:measurement_basis(cons_su_t14, observed).
narrative_ontology:measurement(cons_su_t21, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 21, 0.68).
narrative_ontology:measurement_basis(cons_su_t21, observed).
narrative_ontology:measurement(cons_su_t28, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 28, 0.71).
narrative_ontology:measurement_basis(cons_su_t28, observed).
narrative_ontology:measurement(cons_su_t35, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(cons_su_t35, observed).
narrative_ontology:measurement(cons_su_t42, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 42, 0.71).
narrative_ontology:measurement_basis(cons_su_t42, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(cons_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint story is one reading of the contested constitutional_interpretive_authority kernel. The kernel admits three structural readings, each generating a distinct constraint with different beneficiary/victim sets and extraction profiles. The judicial_supremacy_reading asserts courts wield final interpretive authority; the parliamentary_supremacy_reading asserts legislatures retain it; the coordinate_construction_reading asserts no branch possesses finality. Each reading instantiates different ε, different beneficiaries/victims, and different suppression mechanisms. They coexist as live institutional and theoretical positions held by different actors and traditions. See commentary.kernel_context for the contest structure and commentary.directionality_logic for per-seat classification expectations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
