% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the 'judicial supremacy' reading of the
 *   contested kernel 'constitutional_interpretive_authority'. It asserts that
 *   courts possess final authority to interpret the constitution, including
 *   authority to nullify legislative acts deemed to violate constitutional
 *   limits. The constraint is presented as a coordination mechanism
 *   protecting minority rights against majoritarian overreach—the judiciary
 *   as guardian of a constitutional order that transcends electoral politics.
 *   Structurally, it transfers interpretive authority from elected branches
 *   to an insulated judiciary, vesting in courts both the power to define
 *   constitutional meaning and the coercive force to enforce that definition
 *   against the legislature. The beneficiaries are the judiciary (which gains
 *   institutional authority) and rights advocacy organizations (which gain
 *   access to a litigation-based policy channel). The payers are the
 *   legislature (subordinated to judicial review) and the democratic majority
 *   (whose legislative choices can be voided). The claim/metric gap is
 *   deliberate: the reading CLAIMS the constraint is tangled_rope
 *   (coordination via rights protection + extraction via democratic
 *   subordination), while the authored metrics reflect substantial
 *   extractiveness, moderate suppression (the judiciary does not require
 *   overwhelming coercive force because the constraint's legitimacy
 *   frame—rights guardianship—is widely accepted), and rising theater_ratio
 *   (as the constraint's performance of 'neutral judicial interpretation'
 *   becomes more resource-intensive relative to its actual function of veto).
 *
 * KEY AGENTS:
 *   - Judiciary: agenda-setter, institutional power, benefits from interpretive authority and jurisdictional autonomy
 *   - Rights advocacy organizations: beneficiary, organized power, benefit from litigation-based policy access
 *   - Legislature: payer, institutional power, subordinated to judicial review, constrained exit (constitutional amendment)
 *   - Democratic majority: payer, organized power, experiences nullification of democratically enacted legislation, exit is long-term political pressure
 *   - Judicial literalists/originalists: internal dissent within the judiciary, observer role, contest the scope of judicial authority
 *   - Coordinate construction advocates: excluded from ruling doctrine, would argue for inter-branch constitutional dialogue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '3a97f4be-0ccb-49fc-9fc6-d7745aef3be1').
narrative_ontology:cs_kernel_codification('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', fixed_text).
narrative_ontology:cs_authority_grounding('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', extraction).
narrative_ontology:cs_interpretation_layer_present('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1').
narrative_ontology:cs_reading_relation('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', foundational, courts_possess_final_constitutional_interpretation).
narrative_ontology:cs_axiom_status(courts_possess_final_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', courts_possess_final_constitutional_interpretation, deontological).
narrative_ontology:cs_axiom('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', foundational, judicial_interpretation_transcends_political_preference).
narrative_ontology:cs_axiom_status(judicial_interpretation_transcends_political_preference, holdable).
narrative_ontology:cs_axiom_grounding('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', judicial_interpretation_transcends_political_preference, empirically_contingent).
narrative_ontology:cs_axiom('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', secondary, unelected_judiciary_protects_rights_against_majoritarian_erasure).
narrative_ontology:cs_axiom_status(unelected_judiciary_protects_rights_against_majoritarian_erasure, holdable).
narrative_ontology:cs_axiom_grounding('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', unelected_judiciary_protects_rights_against_majoritarian_erasure, instrumental).
narrative_ontology:cs_reference_frame('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', judiciary_as_final_constitutional_arbiter).
narrative_ontology:cs_drift_state('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', contemporary_politicization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3a97f4be-0ccb-49fc-9fc6-d7745aef3be1', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_organizations).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final interpretive authority over constitutional meaning via the power to nullify legislative acts deemed rights-violative. Justifies this authority as guardianship of fundamental rights against majoritarian overreach. Enforces the constraint by reviewing and striking down statutes; career judges operate within a framework that constitutionalizes their role. The judiciary does not directly collect rents but receives institutional autonomy and jurisdictional authority as its benefit—the power to set constitutional boundaries.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, trapped, national).

% Benefit from judicial supremacy by litigating constitutional claims and securing judicial nullification of legislation they oppose. They operate through the courts as their primary avenue for policy change when electoral politics fails them. Their exit option (mobilizing electoral pressure to amend the constitution or elect different judges) is substantially constrained by the embedded constitutional structure and long judicial tenures.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_organizations, beneficiary,
    organized, generational, constrained, national).

% Subordinated to judicial review: legislative acts are subject to nullification by courts interpreting constitutional limits. Legislators can attempt to draft around judicial doctrine, propose constitutional amendments, or appoint judges perceived as aligned with their views, but cannot unilaterally define constitutional meaning or immunize their acts from judicial scrutiny. The institutional exit (amending the constitution) requires supermajority consensus.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, generational, constrained, national).

% Experiences judicial nullification of legislation it democratically enacted. If the judiciary reads the constitution to forbid a law the majority enacted, the law is struck down regardless of electoral outcome. The majority's exit is long-term political pressure (amending the constitution, electing judges committed to different interpretations) rather than immediate legislative response. The constraint suppresses the majority's policy authority in the name of protecting minority rights.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majority, payer,
    organized, biographical, constrained, national).

% Represent an internal constituency within judicial opinion that contests the scope and legitimacy of judicial supremacy itself, arguing that judges should defer to legislative judgment when constitutional text is ambiguous or that courts should not actively police constitutional boundaries. They take a seat in the institutional contest over what judicial authority should entail.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_literalists_originalists, observer,
    institutional, generational, analytical, national).

% Would argue that constitutional meaning should emerge through inter-branch negotiation and political dialogue rather than judicial pronouncement, but operate within an institutional structure that has embedded judicial supremacy as constitutional practice. Their voice is present in academic discourse and some judicial dissents but structurally marginalized from the ruling doctrine.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects minority rights and fundamental liberties against majoritarian legislative overreach by vesting final constitutional interpretive authority in an insulated, non-electoral institution (the judiciary). Solves the problem of how to prevent electoral majorities from abolishing individual rights through legislation.
% TRANSFER_FUNCTION: Moves constitutional interpretive authority from the elected branches (legislature, president) to the judiciary; moves policy authority from whatever the legislature decides to what the judiciary permits within constitutional limits. Rights advocacy organizations gain access to a court-based policy channel; the legislature loses unilateral authority to define constitutional boundaries.
% ABSENT_VOICES: Coordinate construction advocates (who would argue for inter-branch negotiation rather than judicial pronouncement) and democratic-majoritarian skeptics (who would argue judicial review is illegitimate) are structurally excluded from the ruling framework—their alternative constitutional readings are not consulted in the legitimacy grounds, only contested within minority dissent and academic opposition.
% DISAPPEARANCE_RATIONALE: If judicial supremacy in constitutional interpretation disappeared—if the legislature could override judicial nullification through ordinary statute or if the judiciary possessed no authority to strike down laws—the constitutional order would reorganize: legislative majorities would govern without judicial check; minority rights protections would depend on legislative goodwill or supermajority amendment rather than judicially enforced limits. Rights advocacy strategy would shift from litigation to electoral mobilization.
% FOUNDING_PROBLEM: How to prevent electoral majorities from using their legislative power to abolish fundamental rights and minority protections through ordinary legislation; how to entrench constitutional limits against democratic erosion.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and rights advocacy organizations attest the founding problem is live and justify judicial supremacy as the solution. Legislative subordination advocates and democratic-majoritarian theorists (outside the benefiting parties) attest the founding problem is overstated and judicial supremacy itself becomes the problem—substituting unelected judgment for democratic will. Constitutional scholars and political theorists are divided; some cite historical patterns of majority tyranny (supporting the founding problem), others cite patterns of judicial overreach (disputing it).
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) because the constraint transfers policy authority from elected branches to courts and subordinates majoritarian legislation to judicial interpretation—a substantial asymmetry in who controls outcomes. It is not as high as a pure snare (0.85+) because the constraint does provide genuine coordination: minority rights are protected and majoritarian overreach is checked. Suppression is moderate (0.55) because the constraint operates primarily through legitimacy (courts are trusted as rights guardians) rather than raw coercion; the judiciary does not require overwhelming enforcement apparatus because the constitutional frame is widely internalized. Theater_ratio rises over the interval (0.25 to 0.42) as judicial activity becomes increasingly devoted to doctrinal refinement and performance of neutrality relative to core function—the growth reflects mounting internal contestation (literalists vs. living constitutionalists), rising visibility of appointment politics, and increasing resource expenditure on maintaining the appearance of objective interpretation. Accessibility_collapse is high (0.72) because once the judicial supremacy frame is embedded in constitutional practice, alternatives are substantially foreclosed: legislatures and the majority cannot simply override courts; their exit requires constitutional amendment or long-term political pressure. Resistance is moderate (0.58): the constraint meets real opposition from legislative subordination advocates, democratic majoritarians, and coordinate construction theorists, but the opposition operates within and against the embedded institutional structure rather than successfully displacing it.
 *
 * PERSPECTIVAL GAP:
 *   The two institutional seats compute radically differently: the JUDICIARY, as agenda-setter with maximal beneficiary position, would classify this constraint as rope—genuine coordination of a collective action problem (protecting rights against majoritarian erasure) with minimal coercion required because the coordination function is widely accepted. EXIT ANALYSIS from the judicial seat: judges can exit through appointment to lower courts or non-judicial roles, but the federal judiciary structure is stable; the time_horizon is generational because judicial legitimacy accrues over decades. THE LEGISLATURE, as payer with near-full target position, classifies this constraint as snare—subordination of elected authority to unelected judges enforcing a contested interpretation of an ambiguous text; the justification frame (rights protection) is the mechanism that sustains the subordination. EXIT from the legislative seat: constrained—the only exit from judicial review of constitutionality is amending the constitution, which requires supermajority consensus and decades. This divergence is structural, not opinion: the constraint allocates authority (to courts) and subordinates authority (from legislatures) in the same move. A legislator in a democratic majority cannot avoid the subordination and cannot exit without abandoning the political system itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply across seats: the judiciary is near the full beneficiary end (d ≈ 0.1) because the constraint allocates authority to it without imposing costs; it gains both the power to interpret and the legitimacy frame that justifies that power. Rights advocacy organizations sit near beneficiary (d ≈ 0.25) because they gain litigation access without running the system. The legislature is near the target end (d ≈ 0.8) because it bears the constraint through subordination and constrained exit; it pays the cost of judicial nullification and cannot unilaterally define constitutional meaning. The democratic majority is also near the target end (d ≈ 0.85) because its legislative choices are subject to override and its only exit is slow political organizing. Coordinate construction advocates are excluded rather than classified—their role is to contest the frame from outside. This directionality divergence is exactly what produces per-seat classification divergence: from the judiciary's seat this is a justified protection mechanism (rope-like); from the legislature's seat this is subordination via coercion legitimated through rights language (snare-like).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting rights against majoritarian oppression) is contested: the judiciary and rights advocates attest it is live and urgent; legislative subordination skeptics and democratic theorists attest it is overstated or that the judiciary has become the problem. The founding_problem_status is correctly authored as 'contested' rather than 'live' or 'dead' because no outside party (outside the benefiting/paying division) would independently adjudicate. The disappearance_verdict is 'world_rearranges' because the constraint is not a natural law—legislative authority and majoritarian policy space would expand if judicial supremacy disappeared. This profile does not trigger mandatrophy_resolved because the founding problem remains contested; however, the rising theater_ratio and the structural stability of the metrics suggest the constraint may be approaching a stable extraction equilibrium where the rights-protection frame is ritualized while the real function is veto. This would be a piton-trajectory risk (the function atrophies, but the performance continues). The engine will test this through the mandatrophy_analysis consumer: contested founding problem + stable metrics + rising theater = hypothesis that the constraint may be extracting without maintaining the coordination it justifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_vs_coordinate_construction,
    'Is judicial supremacy in constitutional interpretation the necessary structure for protecting rights, or is it one contingent institutional choice that could be replaced by a coordinate construction model without sacrificing rights protection?',
    'Comparative constitutional analysis of systems that employ different mechanisms (e.g., parliamentary supremacy with strong rights bills, legislative constitutional committees, inter-branch constitutional councils) and examination of whether minority rights survive and thrive under alternative structures.',
    'If coordinate construction or parliamentary supremacy can protect rights equally well, judicial supremacy becomes a choice to vest authority in courts rather than a necessary structure—reclassifying from justified coordination to institutional power distribution (potentially snare-like). If judicial review is uniquely necessary, the constraint remains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_coordinate_construction, conceptual, 'Whether judicial supremacy is structurally necessary for rights protection or one institutional option among alternatives.').

omega_variable(
    rights_protection_vs_policy_veto,
    'To what extent does judicial review protect genuinely vulnerable minorities against majoritarian oppression, versus blocking ordinary democratic policy choices the majority has endorsed?',
    'Systematic empirical analysis of which legislative acts courts nullify: tabulate the political characteristics of struck-down legislation and the structural vulnerability of affected groups. Compare patterns across different constitutional regimes.',
    'High rate of protection for actually vulnerable minorities would support the coordination framing; high rate of policy vetoes on contested (non-rights) questions would support the extraction framing. Mixed patterns would suggest tangled_rope structure is accurate—simultaneous coordination and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_vs_policy_veto, empirical, 'Whether judicial review primarily protects vulnerable minorities or functions as policy veto over majoritarian choices.').

omega_variable(
    unelected_authority_legitimacy,
    'What is the source of legitimacy for unelected judges to override the elected branches'' constitutional interpretations? Is it the institutional insulation from politics (objectivity defense), the expertise in legal reasoning, or the role as guardian of an extrapolitical constitutional order—and does that source remain credible when judges are appointed through explicitly political processes?',
    'Analysis of actual judicial appointment processes, patterns of ideological alignment between appointing officials and judicial voting behavior, and the success or failure of doctrinal efforts to maintain judicial objectivity despite political origins.',
    'If judicial legitimacy rests on political insulation but appointment is manifestly political, the constraint''s justification frame fractures—shifting from ''courts guard rights'' to ''courts installed by a faction enforce their preferences'' (snare-like reclassification). If legitimacy rests on expertise or legal craft and judicial reasoning remains distinguishable from political preference, the constraint remains tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unelected_authority_legitimacy, empirical, 'Whether the legitimacy source for judicial supremacy remains credible given political appointment processes.').

omega_variable(
    kernel_reading_alternative_framings,
    'This constraint is one reading of the contested kernel ''constitutional_interpretive_authority''. The sibling readings (parliamentary_supremacy_reading, coordinate_construction_reading) instantiate different ε values and different beneficiary/victim sets for the same kernel—different structural stories about who wields interpretive authority and who bears the cost. Is the kernel genuinely underdetermined (multiple coherent readings), or does evidence favor one reading''s framing over the others?',
    'Examination of whether the three readings produce contradictory predictions about institutional behavior; if they do, empirical test cases can falsify weaker readings. Also: analysis of whether the readings'' beneficiary/victim structures capture actual power distributions or whether they impose an interpretive frame that obscures structural reality.',
    'If evidence substantially favors one reading, that reading approaches the mountain end and the others become defective constructions. If all three readings generate coherent predictions under different assumptions about what constitutional authority is for, the kernel remains genuinely contested and each reading represents a different institutional choice with real costs and benefits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Whether this judicial supremacy reading is one of genuinely multiple coherent framings of constitutional authority, or whether it is a false reading obscuring a reality better captured by a sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cons_tr_t5, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(cons_tr_t15, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(cons_tr_t25, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cons_be_t5, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(cons_be_t15, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(cons_be_t25, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cons_su_t5, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(cons_su_t15, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(cons_su_t25, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member constraint family decomposing the contested kernel 'constitutional_interpretive_authority'. Each member represents a different institutional reading with distinct beneficiary/victim sets and ε values. The judicial_supremacy_reading instantiates high extractiveness (0.68) because it subordinates the legislature and democratic majority to judicial interpretation; the parliamentary_supremacy_reading instantiates lower extractiveness (estimated 0.42) because legislative interpretation tracks majoritarian will; the coordinate_construction_reading instantiates moderate extractiveness (estimated 0.55) because authority is distributed and no single branch captures exclusive benefit. All three readings address the same kernel (how to determine constitutional meaning) but with structurally different answers. They are linked via network.affects_constraints because change in one reading (e.g., legislative supremacy becoming dominant) would substantially alter the structural conditions (and ε) of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
