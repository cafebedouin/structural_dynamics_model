% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_constitutional_impossibility, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Secession Impossibility (Federalist Reading)
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the constitutional_impossibility_reading of
 *   the secession_legitimacy_boundary kernel. The reading asserts that the
 *   written constitutional text permits only negotiated exit via amendment,
 *   not unilateral secession; consequently, separatist movements' exit
 *   attempts are constitutionally void. This is one reading among four:
 *   popular_sovereignty (referendum is self-legitimating),
 *   grievance_threshold (injustice permits exit regardless of text), and
 *   treaty_primacy (indigenous treaty rights predate all three). The
 *   constitutional impossibility reading claims a mountain (constitutional
 *   structure as natural fact), but authors beneficiaries and extraction
 *   metrics that diverge from that claim — triggering false-summit analysis.
 *   The divergence is the measurement point: federal authority benefits from
 *   the constraint, and enforcement intensifies when separatism grows,
 *   suggesting the 'natural' boundary is doing selective work that could be
 *   called extractive from the separatist seat.
 *
 * KEY AGENTS:
 *   - Federal constitutional authority: sets the rule, interprets text, enforces via courts and administrative denial
 *   - Provincial separatist movements: attempt exit, blocked by amendment requirement, face delegitimization
 *   - Resident populations: benefit from federal integration but lose unilateral exit option
 *   - Non-separatist provinces: retain veto power over any exit attempt
 *   - International state system: watches precedent; enforces recognition only to authorized states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.62).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Secession Impossibility (Federalist Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political_economy/federalism").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).
domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '19c396fb-cf89-4f2d-97c3-21a972e4a965').
narrative_ontology:cs_kernel_codification('19c396fb-cf89-4f2d-97c3-21a972e4a965', fixed_text).
narrative_ontology:cs_authority_grounding('19c396fb-cf89-4f2d-97c3-21a972e4a965', lineage).
narrative_ontology:cs_interpretation_layer_present('19c396fb-cf89-4f2d-97c3-21a972e4a965').
narrative_ontology:cs_reading_relation('19c396fb-cf89-4f2d-97c3-21a972e4a965', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('19c396fb-cf89-4f2d-97c3-21a972e4a965', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('19c396fb-cf89-4f2d-97c3-21a972e4a965', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('19c396fb-cf89-4f2d-97c3-21a972e4a965', foundational, federal_constitutional_text_supreme).
narrative_ontology:cs_axiom_status(federal_constitutional_text_supreme, holdable).
narrative_ontology:cs_axiom_grounding('19c396fb-cf89-4f2d-97c3-21a972e4a965', federal_constitutional_text_supreme, conventional).
narrative_ontology:cs_axiom('19c396fb-cf89-4f2d-97c3-21a972e4a965', foundational, unilateral_secession_textually_foreclosed).
narrative_ontology:cs_axiom_status(unilateral_secession_textually_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('19c396fb-cf89-4f2d-97c3-21a972e4a965', unilateral_secession_textually_foreclosed, empirically_contingent).
narrative_ontology:cs_reference_frame('19c396fb-cf89-4f2d-97c3-21a972e4a965', written_constitutional_supremacy).
narrative_ontology:cs_drift_state('19c396fb-cf89-4f2d-97c3-21a972e4a965', contemporary_separatism_surge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('19c396fb-cf89-4f2d-97c3-21a972e4a965', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_constitutional_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, resident_populations_in_separatist_regions).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, non_separatist_provincial_actors).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_minorities_and_disadvantaged_groups).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, provincial_separatist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, resident_populations_in_separatist_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The federation's supreme constitutional authority, embodied in the judiciary, executive, and amendment mechanism. Interprets the constitutional text as permitting only negotiated exit through amendment. Adjudicates separatist claims as legally invalid. Enforces this interpretation by refusing recognition to unilateral secession and denying separatist institutions constitutional legitimacy.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_constitutional_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Seek independence via referendum or unilateral declaration. Under this reading, their exit attempts are constitutionally categorized as void ab initio — without legal standing. They must route through the amendment mechanism, which requires supermajority support across the entire federation, giving other provinces effective veto power. The constraint denies their preferred exit path while offering only a more costly one.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, provincial_separatist_movements, payer,
    organized, generational, constrained, regional).

% Receive federal services, constitutional protections, and market integration. Also constrained by the amendment requirement: a substantial local majority cannot exit unilaterally; they require federal-level consensus. Individuals face the choice of accepting federation membership or migrating to separatist jurisdiction if one were somehow established outside the constitutional order.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, resident_populations_in_separatist_regions, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, resident_populations_in_separatist_regions, payer).

% Retain veto power over any provincial exit via the amendment requirement. Benefit from federal integration and market access. The constraint ensures they cannot be exit-blocked by separatist neighbors; their sovereignty over remaining territory is preserved by federal guarantee.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, non_separatist_provincial_actors, beneficiary,
    organized, generational, mobile, national).

% Protected by federal constitutional authority, which provides recourse against local majority tyranny. If secession were unilaterally permissible, a province could exit to escape federal minority protections. The constraint's strictness preserves their access to federal remedy and cross-provincial mobility rights.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_minorities_and_disadvantaged_groups, beneficiary,
    powerless, biographical, trapped, national).

% Watches precedent-setting on secession legitimacy. Under this reading, unilateral secession claims are internationally invalid absent constitutional authorization. The constraint stabilizes borders and supports the international norm of sovereign territorial integrity against fragmentation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, international_state_system, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_constitutional_authority).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, supreme constitutional order whose rules are alterable only through its own amendment mechanism, not through exit. This solves the collective-action problem of institutional design: how to commit future generations to a framework without giving any coalition unilateral revision power. The mechanism requires broad consensus (usually supermajority) for fundamental changes, preventing cycling and erosion of constitutional authority through strategic exits.
% TRANSFER_FUNCTION: Moves exit options from unilaterally available (any separatist bloc can declare independence) to federally mediated (all secessions require supermajority amendment approval, giving non-separatist regions veto and negotiation leverage). The constraint transfers decisional power from provincial majorities to the federal amendment supermajority, concentrating gatekeeping authority at the center.
% ABSENT_VOICES: Indigenous sovereignties with pre-constitutional treaty claims are structurally excluded from the amendment framework itself — they would argue that federal constitutional authority does not override their treaty-grounded sovereignty. Regional minorities who would secede if unilateral exit were available are unrepresented in federal-majority decision-making and have no organized seat in the amendment debate.
% DISAPPEARANCE_RATIONALE: If the constitutional impossibility constraint vanished overnight and unilateral secession became legitimate, the federation would fragment wherever separatist regions had majority support. Non-separatist residents in those regions would face forced exit or relocation. Cross-provincial trade, regulatory integration, and federal programs would reconfigure. The remaining federation's size, resource base, and negotiating position would shift. International borders would reorganize; the state system would experience a cascade of recognition decisions. The entire political economy is federationally integrated; the constraint is not natural feature but actively enforced structural choice.
% FOUNDING_PROBLEM: Early federation faced the fragmentation risk: constituent regions, holding prior sovereignties, could exit at will, making union unstable and investment in common institutions risky. The constitutional framework solved this by enshrining exit closure: the union could credibly commit its members to permanence, enabling long-term integration and institutional development. The founding problem was instability and defection risk in founding coalitions.
% FOUNDING_PROBLEM_CORROBORATION: Federal constitutional authorities and jurists attest the founding problem remains live: any permissive secession rule would undermine federal authority and integration. Regional separatist movements and scholarly critics attest the founding problem is obsolete and the constraint has become a tool for suppressing legitimate self-determination; evidence from historical independence movements documents that many successful secessions were initially denied constitutional legitimacy before international recognition. Comparative federalism studies show no consensus on whether exit closure or permissiveness stabilizes federations; empirical evidence is ambiguous.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is measured as 0.62 at interval end: not maximal (the constraint genuinely preserves federal stability and integration benefits) but substantial (separatist regions lose exit that would be available in a permissive regime, and must negotiate in a structure where they are outvoted). Suppression is higher (0.71): the constraint's persistence depends on active judicial denial of separatist legitimacy, constitutional interpretation that forecloses the text's alternative readings, and federal refusal to recognize unilateral secession. The suppression metric captures the enforcement machinery, not acceptance. Theater is moderate (0.28): some constitutional interpretation here is performative (the text's historical originalists note structural ambiguity about secession; treating it as foreclosed requires interpretive work), but the amendment mechanism is real and has been used. The series shows extractiveness and suppression rising in early interval (periods 0-25) then plateauing (25-50), consistent with constitutional entrenchment and separatist acceptance of structural impossibility. Theater stays flat post-25, suggesting the interpretive novelty fades as doctrine solidifies.
 *
 * PERSPECTIVAL GAP:
 *   Federal authority sees the constraint as a natural constitutional feature: the text simply says amendment-only. Separatist movements see it as federal capture of constitutional meaning: the text is ambiguous, and federal courts interpret it to forbid what separatists claim should be permissible. The engine will compute different types from these seats from the same structural data. Federal seats (powerful, institutional) see low directionality (benefit from stability, mobility via political process within federation). Separatist seats (organized, constrained exit) see high directionality (pay the exit-closure cost, trapped by supermajority requirement). This gap is the payload — the framework measures how the same constraint distributes differently across positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal constitutional authority is a beneficiary (d ≈ 0.1-0.2): the constraint protects federal integrity and enforces its interpretation. Separatist movements are payers (d ≈ 0.8-0.9): they bear the cost of exit-foreclosure and bear suppression from courts and federal denial of legitimacy. Resident populations in separatist regions are ambiguously positioned (d ≈ 0.4-0.6): they benefit from federal services and market integration, but lose unilateral exit option their majority might prefer. Non-separatist provinces are beneficiaries (d ≈ 0.15-0.25): they retain veto over any exit and benefit from the federation's stability-through-permanence guarantee. Federal minorities are beneficiaries (d ≈ 0.1): they are protected by the same federal authority that enforces the secession constraint; local majorities cannot exit to escape federal minority protections.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows incipient mandatrophy: the founding problem (federation fragmentation, exit risk) has been substantially solved by a century of integration and institutional development. Yet the constraint persists at high enforcement cost (suppression 0.71) and shows rising extractiveness with intensifying separatism (measurement points 0-25). The founding problem status is contested precisely because the remedy (constitutional impossibility) has decoupled from the original malady. The theater_ratio (0.28) is stable but not negligible: post-25 interval shows constitutional interpretation becoming defensive (performing permanence against separatist counterargument) rather than discovering new necessity. If separatism were to decline (hypothetically), the constraint's enforcement cost would fall, and theater would likely rise (the performance of constitutional meaning without functional suppression). The mandatrophy is not yet acute but shows precursor signals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_ambiguity,
    'Does the constitutional text actually foreclose unilateral secession, or does it permit alternative readings that would legitimate separatist exit?',
    'Originalist historical analysis of constitutional drafting and ratification debates; comparative law study of how similar federal texts have been interpreted; separatist legal scholarship arguing for alternative readings; judicial concurrence or dissent that articulates the textual ambiguity.',
    'If the text is genuinely ambiguous and permits multiple readings, the constitutional impossibility reading is a choice, not a discovery, and the constraint shifts from mountain toward tangled_rope (federal authority extracting via interpretive authority). If the text is genuinely clear, the reading is justified and the constraint is closer to mountain-class (structural, not chosen).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity, conceptual, 'Whether constitutional text legitimately forecloses or permits secession readings.').

omega_variable(
    false_summit_beneficiary_structure,
    'Federal constitutional authority benefits from the constraint — does that benefit arise because the constraint is a natural structural feature, or because the constraint has been interpreted and enforced to produce benefit?',
    'Counterfactual: if the constitution were to be rewritten today from scratch, would this reading be chosen again? If different political coalitions currently gained power, would they re-interpret the text differently? Historical analysis of whether alternative readings were suppressed or genuinely lost.',
    'If the beneficiary structure is incidental (happened to fall that way), mountain classification holds. If beneficiary structure is maintained by interpretive work that could be undone, the constraint is falsely-summit: benefits cluster on the federal coalition precisely because they control constitutional interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_beneficiary_structure, preference, 'Whether federal benefit arises from natural constitutional structure or from maintained interpretive advantage.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) a structural barrier (separatist movements cannot exit because federal law forbids it) or internalized (separatists have come to accept the constitutional impossibility and view unilateral exit as illegitimate)?',
    'Measure separatist discourse over time: does anti-secession language arise from legal consequence-aversion (structural) or from adopted constitutional commitment (internalized)? Compare separatist rhetoric in jurisdictions with strict vs. permissive secession law. If separatism drops when federal enforcement visibly relaxes, suppression is structural; if it remains cultural norm-governed after legal enforcement ends, internalized.',
    'If structural, the suppression could be removed by constitutional amendment, and separatism would surge. If internalized, the suppression persists even if law changes (path-dependence). If both, internalization is higher than measurement suggests — the constraint''s effective suppression exceeds the structural component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in federal-separatist relationships.').

omega_variable(
    kernel_reading_contested,
    'Is the constitutional_impossibility_reading the only coherent interpretation of the federal constitution, or is it one contestable reading among multiple live alternatives?',
    'Jurisprudence survey across federal systems: which readings have been articulated and adopted by courts? Do competing readings have scholarly defenders and institutional advocates? Are the readings mutually exclusive or could they coexist under different framings?',
    'If contested and alternative readings are live, this constraint is one reading of a kernel, and sibling constraints should exist for popular_sovereignty, grievance_threshold, treaty_primacy readings. If this reading is uniquely justified, the kernel frame is misleading and the constraint should be re-authored as a mountain without the committer apparatus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contested, conceptual, 'Whether constitutional_impossibility is the sole reading or one among live alternatives of the secession kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sece_tr_t6, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(sece_tr_t37, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 37, 0.28).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sece_be_t6, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(sece_be_t37, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 37, 0.62).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(sece_su_t6, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(sece_su_t37, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 37, 0.71).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_authority_legitimacy__constitutional_grounding).

% DUAL FORMULATION NOTE:
% The secession_legitimacy_boundary kernel decomposes into four constraints, one per reading. Each reading instantiates a different constraint with different ε values (this reading's ε=0.62 reflects federal benefit from text interpretation; popular_sovereignty reading's ε differs because it treats referendum as legitimating force; treaty_primacy reading's ε differs because it includes indigenous sovereignty as an independent source). The readings coexist in real federations as competing normative claims held by different institutional actors (federal courts, separatist movements, indigenous governments). All four stories must be written separately to preserve ε-invariance and capture the structural divergence. Network links show influences: constitutional_impossibility influences popular_sovereignty (if constitutional text is treated as foreclosing, democratic referendum becomes suspect); treaty_primacy influences both (if indigenous treaties predate federal constitution, neither federal nor provincial authority is supreme).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
