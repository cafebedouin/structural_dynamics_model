% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__living_constitution_reading, []).

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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution Reading — Evolving Meaning Under Judicial Adaptation Authority
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the us_constitution_interpretive
 *   kernel: the living constitution reading, under which constitutional
 *   meaning evolves with societal values and interpretive authority derives
 *   from reasoned adaptation to contemporary conditions. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement under contest — the actual operating arrangement of
 *   evolving-meaning adjudication with judicial custody — assessed from this
 *   reading's own lights, never for the originalist or
 *   popular-constitutionalist arrangements this reading would reject. The
 *   reading regards much of the authority transfer it performs as protective
 *   function, but it authors the descriptive metrics honestly: the
 *   arrangement does displace state and democratic authority through a final,
 *   unappealable adjudicator, and that displacement is asymmetrically
 *   distributed. Claim and metrics are independent facts: the tangled_rope
 *   claim records this reading's structural assessment (genuine coordination
 *   function plus enforced asymmetric transfer); the engine computes per-seat
 *   classifications from the structural data and may diverge. KEY AGENTS (by
 *   structural relationship): - supreme_court_justices: Agenda-setter
 *   (institutional/identity_locked) — decides what the Constitution means;
 *   the arrangement's authority accrues to this seat -
 *   civil_rights_expansion_claimants: Primary beneficiary
 *   (organized/constrained) — obtains nationalized rights through litigation
 *   - reproductive_autonomy_advocates: Beneficiary (organized/constrained) —
 *   court-derived protection, court-reversible - lgbtq_rights_claimants:
 *   Beneficiary (organized/constrained) — precedent-dependent recognition -
 *   federal_government_branches: Secondary beneficiary
 *   (institutional/constrained) — validates expanded national power -
 *   state_legislatures: Primary payer (institutional/trapped) — loses policy
 *   authority to nationalized rights and federal reach -
 *   states_rights_advocates: Payer (powerful/trapped) — displaced allocation
 *   of authority, slow recourses - original_meaning_textualists: Payer
 *   (organized/mobile) — displaced from judicial majorities, built parallel
 *   institutions - local_democratic_majorities: Excluded (moderate/trapped) —
 *   policy choices invalidated without a seat in the conversation -
 *   constitutional_law_academy: Analytical observer (moderate/analytical) —
 *   shapes the argument space, holds no decision power
 *
 * KEY AGENTS:
 *   - supreme_court_justices: Agenda-setter (institutional/identity_locked) — final interpretive authority; gains accrue here
 *   - civil_rights_expansion_claimants: Primary beneficiary (organized/constrained) — nationalized rights via litigation
 *   - reproductive_autonomy_advocates: Beneficiary (organized/constrained) — acquired and lost protection through the same channel
 *   - lgbtq_rights_claimants: Beneficiary (organized/constrained) — precedent-dependent recognition
 *   - federal_government_branches: Secondary beneficiary (institutional/constrained) — expanded commerce and implied powers
 *   - state_legislatures: Primary payer (institutional/trapped) — policy authority displaced upward
 *   - states_rights_advocates: Payer (powerful/trapped) — sovereignty claims defeated, recourses slow
 *   - original_meaning_textualists: Payer (organized/mobile) — long exclusion, eventual capture via appointment arbitrage
 *   - local_democratic_majorities: Excluded (moderate/trapped) — invalidated without representation in the deciding forum
 *   - constitutional_law_academy: Analytical observer (moderate/analytical) — produces the legitimating scholarship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.62).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.54).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution Reading — Evolving Meaning Under Judicial Adaptation Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, '4ed06d36-ebeb-427d-9df0-7f281033842a').
narrative_ontology:cs_kernel_codification('4ed06d36-ebeb-427d-9df0-7f281033842a', fixed_text).
narrative_ontology:cs_authority_grounding('4ed06d36-ebeb-427d-9df0-7f281033842a', expertise).
narrative_ontology:cs_interpretation_layer_present('4ed06d36-ebeb-427d-9df0-7f281033842a').
narrative_ontology:cs_reading_relation('4ed06d36-ebeb-427d-9df0-7f281033842a', us_constitution_interpretive__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('4ed06d36-ebeb-427d-9df0-7f281033842a', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('4ed06d36-ebeb-427d-9df0-7f281033842a', foundational, generational_self_governance_entitlement).
narrative_ontology:cs_axiom_status(generational_self_governance_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('4ed06d36-ebeb-427d-9df0-7f281033842a', generational_self_governance_entitlement, deontological).
narrative_ontology:cs_axiom('4ed06d36-ebeb-427d-9df0-7f281033842a', secondary, judicial_reasoned_adaptation_is_legitimate_update).
narrative_ontology:cs_axiom_status(judicial_reasoned_adaptation_is_legitimate_update, holdable).
narrative_ontology:cs_axiom_grounding('4ed06d36-ebeb-427d-9df0-7f281033842a', judicial_reasoned_adaptation_is_legitimate_update, instrumental).
narrative_ontology:cs_reference_frame('4ed06d36-ebeb-427d-9df0-7f281033842a', reasoned_adaptation_framework).
narrative_ontology:cs_drift_state('4ed06d36-ebeb-427d-9df0-7f281033842a', post_dobbs_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4ed06d36-ebeb-427d-9df0-7f281033842a', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_government_branches).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, state_legislatures).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, local_democratic_majorities).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, commerce_clause_expansion_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, substantive_due_process_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, selective_incorporation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nine life-tenured judges decide which constitutional questions receive answers and what the answers are; their opinions bind every legislature, agency, and lower court in the country. Their institutional identity, legacies, and place in civic memory are constituted by the interpretive method they practice; departure means retirement, not release from the precedent structure they built.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, supreme_court_justices, agenda_setter,
    institutional, generational, identity_locked, national).

% Congress and the executive obtain validated reach under evolving Commerce Clause and implied-powers readings; national regulatory programs, civil rights enforcement, and administrative agencies stand on that validation. They cannot opt out of judicial review of their own statutes and depend on favorable doctrinal maintenance for programs already built.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_government_branches, beneficiary,
    institutional, generational, constrained, national).

% Litigants challenging segregation, discrimination, and disenfranchisement obtained outcomes that state legislatures of their era would not enact. Their access runs through expensive, decade-long litigation before judges they cannot elect or remove, and each gain must be defended in later cases rather than secured by statute.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants, beneficiary,
    organized, biographical, constrained, national).

% Secured national protection through substantive due process holdings rather than legislation, then watched that protection fall to a changed judicial majority decades later. Their fallback after retrenchment is fifty separate state legislative campaigns, illustrating how court-derived guarantees concentrate both acquisition and vulnerability in a single institution.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    organized, biographical, constrained, national).

% Decriminalization and marriage equality arrived through equal-protection and due-process holdings rather than enacted statutes. Their recognized status rests on precedents whose survival depends on future judicial majorities maintaining the evolving-meaning method that produced them.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Enact policy within traditional police powers — schooling, family law, criminal justice, elections — then defend it in litigation against federal constitutional challenges. Lose policy authority whenever courts read rights broadly or federal powers expansively. Cannot exit the union, the appellate structure, or the precedent their losses create.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, state_legislatures, payer,
    institutional, generational, trapped, regional).

% Political and legal movements defending state sovereignty and locally determined moral authority. Watch their preferred allocation of authority displaced by nationalized rights and expanded federal commerce power. Their recourses — judicial appointments and Article V amendment — are slow, uncertain, and controlled by opponents during long stretches.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, states_rights_advocates, payer,
    powerful, generational, trapped, regional).

% Scholars, practitioners, and judges committed to ratification-era public meaning. Spent decades without judicial majorities while building parallel institutions — law school centers, clerkship pipelines, appointment networks — that eventually produced a Court majority of their own. Their mobility came from constructing alternative venues inside the system, not from leaving it.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists, payer,
    organized, biographical, mobile, national).

% Voters and communities whose policy choices — school prayer, gun regulation, campaign finance limits, sentencing policy — are invalidated by courts they did not elect and cannot appeal beyond. Their constitutional voice is mediated entirely through litigation they may be unable to afford, and no institutional channel exists for them to contest interpretive method directly.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, local_democratic_majorities, excluded,
    moderate, biographical, trapped, local).

% Law professors, historians, and commentators who produce the doctrinal scholarship feeding both sides of the method debate, supply the historical research that legitimizing opinions cite, and train the clerks and judges who will apply the doctrine. Hold no decision power but shape the argument space future benches inherit.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, constitutional_law_academy, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__living_constitution_reading, supreme_court_justices).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__living_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single binding constitutional framework across more than two centuries of changed circumstances: settles federal-state conflicts, resolves interbranch disputes, and adapts the application of 18th-century text to conditions its drafters never confronted — industrial markets, electronic surveillance, digital speech, new claims of equality — without requiring a successful Article V amendment campaign for each change.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional meaning from ratification-era public meaning, state institutions, and ordinary democratic processes to the federal judiciary; and, through nationalized rights and expanded federal powers, moves concrete policy-setting authority from state capitals to Washington.
% ABSENT_VOICES: State governments and local majorities whose enactments are struck down appear only as litigating losers; originalist voices argued in scholarship but held no seats on deciding courts for long stretches; future generations bound by today's precedents have no representative in the room; popular movements find their constitutional arguments admitted only when converted into justiciable claims by counsel.
% DISAPPEARANCE_RATIONALE: Overnight removal of evolving-meaning adjudication would strand eight decades of precedent — school desegregation, one-person-one-vote, privacy, incorporation of the Bill of Rights against the states, the commerce-clause foundation of the modern administrative state — reopening every settled question at once. The federal-state balance and the entire rights landscape would reorganize around whichever replacement method captured the bench.
% FOUNDING_PROBLEM: How a fixed 18th-century charter can legitimately govern a nation transformed by industrialization, national markets, mass media, and expanding claims of equality — the Progressive-era problem of constitutional obsolescence crystallized when a court applying frozen meaning invalidated social legislation in the Lochner era.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians across methodological camps document the Lochner-era crisis and the New Deal settlement that produced the modern adaptive posture; Woodrow Wilson's Constitutional Government (1908) and the progressive jurisprudence literature predate the current beneficiary coalition; comparative constitutional law supplies an independent lineage (Canada's living-tree doctrine, 1929-30); even originalist scholarship concedes the underlying phenomenon — changed circumstances meeting a negligible formal amendment rate — while disputing the adaptive remedy rather than the problem's existence.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial but bounded: the arrangement transfers decision authority from states, local majorities, and rival interpreters to a single unappealable bench, yet a large share of that transfer is the protective function the reading itself endorses. Suppression (0.54) is primarily STRUCTURAL rather than coercive: finality (no appeal above the Court), binding precedent, and the practical closure of Article V make alternatives formally available but practically blocked; a smaller component is internalized (bar and law-school socialization treating Court pronouncements as settled law until reversed). Accessibility collapse is low (0.32) because rival methods remain intellectually and institutionally alive — originalism sustains journals, centers, and eventually a bench majority — which is exactly what distinguishes this from a natural-law profile. Resistance is high (0.62): a sustained counter-movement captured appointments, produced retrenchment decisions, and periodically revives court-curbing proposals. Theater ratio (0.42) reflects the growing share of opinion-writing that performs legitimacy (reasoned-elaboration rituals, history-selective citation) relative to doctrinal work that changes outcomes. The measurement series run on ONE shared grid (t = 0, 15, 30, 45, 60, 75, 85; linear mapping t=0 to 1937, t=85 to 2022, so every tracked metric is authored at every examined point). Trajectories: extraction accumulates across the Warren-Burger-Rehnquist expansions and plateaus after Dobbs; theater rises as legitimacy performance grows; suppression_requirement tracks the maturing enforcement infrastructure (certiorari control, nationwide injunctions, shadow-docket practice). The doctrinal record oscillates (expansion, backlash, partial retrenchment) but the underlying authority transfer is monotonic — the oscillation is in which side wins cases, not in where authority sits. Base_properties values are end-state (t=85) readings. Coalition note: the payer seats are heterogeneous (trapped institutional states, mobile organized textualists, unrepresented local majorities), and the eventual textualist-plus-states coalition succeeded precisely by routing around the constraint's enforcement surface through appointment politics rather than frontal amendment.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat, the arrangement is a craft it practices and a legacy it embodies — coordination it personally performs, with extraction experienced as burden (workload, legitimacy attacks). From the beneficiary seats, the same structure is protective function: the price of rights they could not obtain legislatively. From the trapped payer seats (states, local majorities), it operates as enforced displacement — loss of policy authority to a forum they cannot reach. From the mobile payer seat (textualists), it was a temporary exclusion that appointment arbitrage eventually reversed, so its severity is discounted by realized exit. Same-power divergence is visible among institutional seats: federal branches and state legislatures hold comparable formal power, but the former benefits from the constraint's directional flow while the latter pays it — differentiated by relationship to the constraint, not by global standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (rights-expansion claimants, federal branches) derive low directionality — the arrangement subsidizes them. Declared victims (state legislatures, states-rights advocates, textualists, local majorities) derive high directionality, modulated by exit: trapped state actors sit nearer full-target than the mobile textualists, whose realized arbitrage damps their effective target position. The agenda-setter seat is not listed as a beneficiary because it administers rather than collects passively — but the receipt surface records that the displaced interpretive authority demonstrably accrues to it, placing it near the beneficiary end. No directionality_overrides are authored: overrides key on power atoms, and this story's same-power seats carry OPPOSED relationships (institutional beneficiaries vs. institutional payers), so any per-atom override would corrupt one side; the structural derivation from beneficiary/victim declarations plus exit options is the accurate instrument here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adapting a fixed text to changed conditions — remains live, so no mandatrophy resolution is declared and the arrangement's mandate has not outlived its function. The classification discipline cuts both ways: recognizing the genuine coordination function (continuous peaceful adaptation, dispute settlement, framework stability across 235 years) blocks a pure-extraction misreading that would erase the civil-rights gains the structure delivered; recognizing the enforced asymmetry (who pays, who cannot exit, where authority accrues) blocks a pure-coordination misreading that would launder judicial supremacy as mere housekeeping. The arrangement is neither a scaffold — it carries no sunset and claims permanence — nor a piton: its function has not atrophied, its administrator actively maintains it, and identifiable seats profit enough to defend it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (living_constitution_reading) of the us_constitution_interpretive kernel; what would each sibling reading change structurally if instantiated?',
    'Author the sibling stories and compare structural surfaces: the originalist_reading relocates authority to ratification-era meaning — converting today''s beneficiary claimants into targets of judicial invention and restoring states to beneficiary position; the popular_constitutionalism_reading relocates authority to movements — converting the Court from agenda-setter to one contested participant among many.',
    'Classification is reading-relative: the same kernel yields different types, beneficiary sets, and epsilon values per reading. Cross-reading comparison is valid only through the network edges linking the family; averaging epsilon across readings is a category error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one-of-three readings; the readings disagree on the source of interpretive authority, and each redistribution flips beneficiary/victim sets.').

omega_variable(
    function_vs_capture_ambiguity,
    'Is the measured transfer of authority from states and democratic processes to courts the arrangement''s intended protective function (rights enforcement against local majorities) or rent-like capture of decision authority?',
    'Compare eras and case classes: where judicial review protected politically powerless minorities (Carolene Products line, Brown) versus where it entrenched elite economic preferences (Lochner) or manufactured policy with thin textual warrant; measure outcome alignment with disadvantaged-party interests across the interval.',
    'If most displacement is protective function, effective extraction falls toward coordination-cost levels and the verdict softens rope-ward; if a large share is rent-like, extraction rises and the arrangement drifts snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(function_vs_capture_ambiguity, empirical, 'Whether countermajoritarian displacement is function or capture — the central internal tension the reading itself acknowledges.').

omega_variable(
    judicial_supremacy_separability,
    'Is concentrated judicial authority inherent to evolving-meaning interpretation, or separable from it — could meaning evolve under departmentalist or popular-adaptive custody without exclusive judicial control?',
    'Institutional-design analysis and comparative practice: jurisdictions applying living-tree doctrine with weaker judicial supremacy; counterfactual modeling of cooperative interpretive institutions (congressional constitutional interpretation offices, popular constitutional conventions) sustaining adaptation without finality.',
    'If separable, part of the measured extraction is contingent institutional choice rather than intrinsic to the reading — lowering epsilon and softening the verdict toward rope; if inseparable, the concentration is structural and the tangled-rope assessment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_separability, conceptual, 'Whether the reading entails judicial supremacy or merely permits it — separates the method from the institutional form this story measures.').

omega_variable(
    dobbs_era_trajectory_ambiguity,
    'Does the post-Dobbs originalist ascendancy mark lifecycle decay of this reading''s enforcement capacity, or ordinary oscillation within its operation?',
    'Track subsequent appointments, retention of landmark precedents (Obergefell, incorporation line), and continued state-level reliance on evolved-rights frameworks over the coming decade; distinguish durable method displacement from single-term retrenchment.',
    'A decay reading dates a possible transition toward inertial maintenance (function surviving mainly as academic and lower-court habit); an oscillation reading keeps the late-interval measurements within normal variance and leaves the type stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dobbs_era_trajectory_ambiguity, empirical, 'Whether late-interval retrenchment is trend or cycle — determines how the final measurement points should be weighted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 0, 85).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__living_constitution_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_interpretive__living_constitution_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t15, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__living_constitution_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_interpretive__living_constitution_reading, theater_ratio, 45, 0.29).
narrative_ontology:measurement_basis(us_c_tr_t45, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__living_constitution_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).
narrative_ontology:measurement(us_c_tr_t75, us_constitution_interpretive__living_constitution_reading, theater_ratio, 75, 0.39).
narrative_ontology:measurement_basis(us_c_tr_t75, observed).
narrative_ontology:measurement(us_c_tr_t85, us_constitution_interpretive__living_constitution_reading, theater_ratio, 85, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t85, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t15, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(us_c_be_t15, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t45, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 45, 0.57).
narrative_ontology:measurement_basis(us_c_be_t45, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(us_c_be_t60, observed).
narrative_ontology:measurement(us_c_be_t75, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 75, 0.63).
narrative_ontology:measurement_basis(us_c_be_t75, observed).
narrative_ontology:measurement(us_c_be_t85, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 85, 0.62).
narrative_ontology:measurement_basis(us_c_be_t85, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t15, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement_basis(us_c_su_t15, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t45, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 45, 0.42).
narrative_ontology:measurement_basis(us_c_su_t45, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement_basis(us_c_su_t60, observed).
narrative_ontology:measurement(us_c_su_t75, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 75, 0.51).
narrative_ontology:measurement_basis(us_c_su_t75, observed).
narrative_ontology:measurement(us_c_su_t85, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 85, 0.54).
narrative_ontology:measurement_basis(us_c_su_t85, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'how the Constitution is interpreted' covers three structurally distinct claims, one per reading of the us_constitution_interpretive kernel. Each sibling gets its own epsilon, beneficiary/victim structure, and classification; forcing one story to span all three would make epsilon observer-dependent and violate DP-001. Sibling constraint IDs are assumed to follow this file's naming pattern (us_constitution_interpretive__{reading_id}); the upstream/downstream structure runs through appointment politics and doctrinal citation — this reading's accumulated precedent is the primary material object the originalist_reading seeks to displace and the popular_constitutionalism_reading seeks to democratize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
