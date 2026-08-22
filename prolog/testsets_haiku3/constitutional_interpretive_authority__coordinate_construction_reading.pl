% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Constitutional Authority via Inter-Branch Dialogue (Coordinate Construction Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Under the coordinate construction reading, no single branch of the U.S.
 *   government possesses final interpretive authority over the Constitution.
 *   Instead, constitutional meaning emerges through inter-branch dialogue,
 *   political contestation, and the struggle for power across appointments,
 *   amendments, and enforcement. This reading describes a system where
 *   judicial decisions can be overridden by legislative amendment or
 *   executive non-compliance; where legislative enactments can be blocked by
 *   presidential veto or judicial adjudication; where executive power is
 *   checked by both. The constraint persists because all three branches
 *   benefit from retaining contestability—none wishes to cede finality to the
 *   others. The reading is one of three sibling interpretations of how
 *   constitutional authority is distributed (judicial supremacy,
 *   parliamentary supremacy, and this coordinate reading). The authored
 *   metrics reflect the real costs of this arrangement:
 *   moderate-to-moderate-low extractiveness (0.38), low suppression (0.22),
 *   and moderate theater (0.41), indicating a system that requires active
 *   political contestation to maintain but does not rely on heavy coercion.
 *
 * KEY AGENTS:
 *   - Legislature: sets rules through statute, controls appropriation and impeachment, appoints judges through confirmation. Under coordinate construction, is a co-equal constructor of constitutional meaning, not subordinate to courts.
 *   - Judiciary: decides cases and interprets constitutional text. Under coordinate construction, has no final authority; judicial decisions can be overridden by amendment, appointment change, or legislative action.
 *   - Executive: nominates judges, enforces (or refuses to enforce) court orders, shapes interpretation through prosecutorial discretion. Under coordinate construction, is a co-equal participant, not bound by judicial supremacy.
 *   - General Public / Political Contestation: participates through elections, amendment campaigns, and political mobilization. Gains from the constraint by retaining democratic power to shape constitutional meaning.
 *   - Interpretive Clarity Seekers: institutional actors and scholars who depend on stable constitutional meaning. Pay the cost of interpretive uncertainty and institutional friction.
 *   - Rule-of-Law Formalists: constitutional theorists committed to hierarchy and finality. Bear the cost of perpetual contestation and theoretical vulnerability.
 *   - Suppressed Minorities: groups lacking durable political support. Excluded from the dialogue; would argue for judicially-entrenched protections.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.38).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.22).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Constitutional Authority via Inter-Branch Dialogue (Coordinate Construction Reading)").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, '2aa5dc41-ca16-4676-abfb-5b381b1e70c9').
narrative_ontology:cs_kernel_codification('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', fixed_text).
narrative_ontology:cs_authority_grounding('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', distributed).
narrative_ontology:cs_reading_relation('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', foundational, no_final_interpretive_authority).
narrative_ontology:cs_axiom_status(no_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', no_final_interpretive_authority, deontological).
narrative_ontology:cs_axiom('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', foundational, constitutional_meaning_via_political_contestation).
narrative_ontology:cs_axiom_status(constitutional_meaning_via_political_contestation, holdable).
narrative_ontology:cs_axiom_grounding('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', constitutional_meaning_via_political_contestation, conventional).
narrative_ontology:cs_reference_frame('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', coordinate_inter_branch_dialogue_framework).
narrative_ontology:cs_drift_state('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', contemporary_executive_power_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2aa5dc41-ca16-4676-abfb-5b381b1e70c9', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, executive).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, general_public_political_contestation).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, interpretive_clarity_seekers).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, rule_of_law_formalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts statutes, controls appropriations, impeaches executive and judges. Under coordinate construction, participates as co-equal in constitutional interpretation through statutory enactment, amendment initiation, and non-compliance with judicial decisions. Can threaten amendment to override courts or presidential non-compliance to reverse judicial interpretations. Benefits from dispersed authority because it retains power against judicial supremacy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, legislature, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, legislature, beneficiary).

% Decides cases, interprets constitutional provisions and statutes. Under coordinate construction, has no final authority; decisions can be overridden by legislative amendment, executive non-compliance, or appointment of judges with different readings. Participates in constitutional dialogue by issuing opinions that legislature and executive then contest through political action. Benefits from retaining the appearance of authority and the power to shape initial outcomes in litigation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, judiciary, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, judiciary, beneficiary).

% Nominates judges, enforces or declines to enforce court orders, interprets constitution through prosecutorial discretion and agency policy. Under coordinate construction, is a co-equal participant not bound by judicial finality; can refuse to enforce Supreme Court decisions (see Lincoln, Jackson precedents), appoint judges to shift future interpretations, and shape constitutional meaning through executive action. Benefits from dispersed authority because it retains power against judicial supremacy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, executive, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, executive, beneficiary).

% Participates in elections, campaigns for constitutional amendment, mobilizes political coalitions around constitutional interpretation. Under coordinate construction, retains power to shape constitutional meaning through voting and amendment—cannot be overruled by courts, must persuade legislature and executive through electoral power. Benefits from interpretive contestation because it preserves the ability to participate in constitutional change through democratic channels.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, general_public_political_contestation, beneficiary,
    organized, generational, constrained, national).

% Institutional actors (judges, administrators, practitioners), legal scholars, and rule-of-law advocates who depend on stable, predictable constitutional meaning to perform their roles. Under coordinate construction, must absorb uncertainty about which branch's interpretation will prevail; litigation costs rise because disputes cannot be resolved by final judicial pronouncement; institutional planning becomes difficult because constitutional meaning can shift through political action.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, interpretive_clarity_seekers, payer,
    moderate, biographical, constrained, national).

% Constitutional theorists and judges committed to rule of law principles—hierarchy, finality, and predictability as essential to law's legitimacy. Under coordinate construction, their theoretical commitments are perpetually contested; judicial pronouncements of constitutional law can be overridden by political action; the constraint denies the institutional finality they view as necessary for law to constrain power rather than merely express it.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, rule_of_law_formalists, payer,
    moderate, biographical, constrained, national).

% Groups whose constitutional interests lack durable support across electoral majorities and all three branches (religious minorities, unpopular dissidents, stateless persons). Under coordinate construction, cannot rely on judicial entrenchment of rights because courts lack final authority; must maintain continuous political mobilization to defend constitutional status. If they lose electoral salience, the branches will collectively revise constitutional meaning against their interests without judicial intervention.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, suppressed_minority_interest, excluded,
    powerless, biographical, trapped, national).

% Academic and institutional structures (constitutional law scholarship, bar associations, nonpartisan commissions) that study and record how branches interpret the constitution. Serve analytical function of documenting when consensus emerges versus when contestation dominates. No direct stake in the constraint; interested in understanding its operation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, cross_branch_consensus_institutions, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__coordinate_construction_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the problem of how to interpret and update a fixed written constitution across three branches with conflicting institutional interests. Instead of requiring finality (which would concentrate power in one branch), coordinate construction solves the problem by dispersing authority: each branch participates in interpreting the constitution through its actions (enactment, adjudication, enforcement), and the actual meaning emerges from which branch's interpretation prevails through political contestation. The coordination function is to institutionalize amendment-without-amendment: constitutional change can occur through appointment, non-compliance, and reinterpretation without requiring formal amendment.
% TRANSFER_FUNCTION: Transfers interpretive power and the ability to shape constitutional meaning from judicial pronouncements (which would be final under supremacy readings) to political contestation: electoral victories, appointment control, legislative action, and mobilization. Winners of political contests gain the power to define constitutional meaning for a period; losers bear the cost of perpetual contestation and institutional friction. The flow is from those who seek clarity and finality (courts, formalists, minorities relying on entrenched rights) toward those who control branches and political coalitions.
% ABSENT_VOICES: Suppressed minorities who lack durable political coalition support are absent from the inter-branch dialogue—they have no seat at a table where all three branches negotiate. Rule-of-law formalists and comparative constitutionalists committed to hierarchy are systematically excluded from defending finality and predictability; their objections are noted but overridden by the branches' preference for contestability. International observers of the system would argue for clearer authority allocation; they are excluded from the debate by the U.S. system's sovereignty.
% DISAPPEARANCE_RATIONALE: If coordinate construction disappeared—replaced by genuine judicial supremacy or parliamentary supremacy—the constitutional system would fundamentally reorganize: litigation would stabilize around Supreme Court finality; amendment would become less necessary because courts would settle meaning; the political character of constitutionalism would shift from contestation to deference. The branches would reorganize their relationships around hierarchy instead of negotiation.
% FOUNDING_PROBLEM: A written constitution governs three competing branches with institutional interests. Early practice (Marbury v. Madison, Jackson's veto, Lincoln's suspension of habeas corpus) revealed that no single branch could enforce finality unilaterally—the other branches would resist and non-comply. The coordinate construction reading emerged historically as the de facto answer: constitutional authority is negotiated through inter-branch struggle, not adjudicated through hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists (Ackerman, Sunstein, Whittington) document that constitutional change occurs de facto through appointment and political contestation without formal amendment. Legal historians (Paulsen, Engel) trace executive defiance of judicial orders and legislative override of judicial precedent. Contemporary branch conflicts over DACA, immigration policy, and executive power demonstrate ongoing contestation. Judicial opinions (Scalia's originalism, Thomas's federalism arguments) sometimes invoke coordinate construction to resist other branches' readings. Legislative non-compliance with Supreme Court decisions (e.g., states' resistance to Roe v. Wade before overturning) demonstrates branches' actual practice of contestation. No single source external to the branches endorses finality; the branches' own practice of contestation corroborates the coordinate reading.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).
:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint does impose costs—interpretive instability, litigation friction, and the vulnerability of rights to political reversal—but these costs are distributed and the coordination benefit (no single actor monopolizes meaning) is substantial. The measurement series shows a rise from 0.25 to 0.40 over the first 24 time points (reflecting increasing awareness of the costs and increasing branch assertiveness in the post-2010 era), then stabilization at 0.38 as the system settles into a new equilibrium of continuous contestation without escalation to constitutional crisis. Suppression is low (0.22) because the constraint is defended primarily through political mechanisms (appointment, amendment, non-compliance) rather than coercive enforcement; the branches persuade each other through institutional power, not through silencing alternative claims. Theater is moderate (0.41) and rising slightly (0.28 to 0.44 then dipping to 0.41) because while the coordinate reading is substantively defended by political theorists and actualized through de facto practice, significant rhetorical effort goes into framing inter-branch conflicts as technical disputes rather than constitutional battles—the theater ratio reflects performative appeals to rule of law and separation of powers while actual power struggles continue beneath the rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The legislature and executive compute this reading as beneficial (low d, low extractiveness from their seats) because it preserves their power against judicial supremacy. The judiciary computes it as extractive (high d, high extractiveness from its seat) because it denies finality and subjects judicial decisions to political reversal. Clarity-seekers compute it as highly extractive (high d, costs of instability without benefits of power) because they depend on stable meaning. Rule-of-law formalists compute it as contradicting their core commitment (high d, theoretical vulnerability). The engine's per-seat computations should diverge sharply, which is the entire point of this reading—it distributes authority such that different seats experience fundamentally different structural relationships to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The three branches have equal power (institutional) but different exit options and time horizons. Legislature and executive can exit by achieving supremacy (defecting to a different constitutional reading enforced by their control of appointments and enforcement). Judiciary has lower exit options because finality is denied to it by the other two branches' refusal to comply. All three benefit from the coordinate reading as a stable equilibrium (each prefers contestation to a supremacy regime it doesn't control), but would each prefer unilateral supremacy. Clarity-seekers are trapped (constrained exit) because they cannot exit the legal system without abandoning their institutional roles. Rule-of-law formalists are similarly trapped. Suppressed minorities are trapped not by law but by lacking electoral/political power. The directionality derivation should show: branches near the beneficiary end despite naming them as targets/payers in formal structure (because they collectively benefit from the arrangement); clarity-seekers toward the target end (because they bear costs); minorities trapped without countervailing power.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve a mandatrophy problem—the founding problem (how to update a fixed text across branches) is genuinely live, contested, and unresolved. Each branch still believes it should have more authority; each branch still pursues supremacy where it can gain it (through appointment, amendment, non-compliance). The constraint persists because the status quo of contestation is more stable for all three branches than an escalation to a constitutional crisis that would resolve the question. This is a tangled_rope, not a piton, because: (1) the coordination function is real (the system does update constitutional meaning through inter-branch contestation); (2) the extraction is real (clarity-seekers and minorities pay costs); (3) enforcement is active (continuous political contestation, appointment battles, amendment campaigns). It is not a snare because the extraction is not primary—the extraction is a byproduct of the coordination, not the reason the branches maintain it. It is not a rope because the asymmetric costs (clarity-seekers and minorities) are substantial and the distribution is unequal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_vs_de_facto_supremacy,
    'Is the coordinate construction reading descriptive of how constitutional authority actually operates, or is it a normative claim that should govern how it operates—and if de facto one branch has achieved supremacy through soft power (appointment, compliance norms), does the reading dissolve?',
    'Empirical analysis of branches'' compliance with each other''s interpretive claims over time; measurement of who actually controls contested constitutional questions through threat, enforcement, and amendment.',
    'If judicial supremacy is de facto established through appointment and compliance, the coordinate reading is aspirational rather than descriptive, and the constraint should be reclassified toward snare (extraction through judicial gatekeeping) or piton (judicial supremacy maintained through theater). If true coordinate contestation is observed, the reading stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinate_vs_de_facto_supremacy, empirical, 'Whether coordinate construction is real or ideological cover for de facto supremacy.').

omega_variable(
    stability_vs_contestation_tradeoff,
    'Does the coordinate construction reading require perpetual contestation as a feature, or does it permit stabilization around shared interpretive commitments—and if stabilization occurs, does the constraint become rope (pure coordination) rather than tangled_rope?',
    'Historical analysis of periods of inter-branch consensus (e.g., post-New Deal on enumerated powers, post-Civil Rights on fundamental rights) versus periods of contestation (e.g., Reconstruction, 1930s, contemporary). Test whether periods of consensus involve reduced contestation machinery or merely latent contestation.',
    'If consensus is possible without collapsing the coordinate structure, the constraint might be classified as rope with lower extractiveness. If consensus requires suppression of contestation through appointment domination or non-compliance norms, it remains tangled_rope. If contestation is required by the reading''s logic, periods of consensus represent failures to instantiate the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_contestation_tradeoff, conceptual, 'Whether coordinate construction requires perpetual contestation or permits consensus.').

omega_variable(
    minority_protection_under_contestation,
    'Can suppressed minorities secure durable constitutional protections under a coordinate construction reading, or does the reading systematically favor majoritarian branches (legislature and electoral executive) at the expense of those lacking political power?',
    'Comparative analysis of minority-protection outcomes under coordinate construction (U.S.) versus judicial supremacy (Canada, Germany post-WWII) versus parliamentary supremacy with entrenched minority clauses (some European systems). Test whether minorities have greater security under frameworks that permit judicial entrenchment.',
    'If minorities are systematically vulnerable under coordinate construction, the constraint extracts from them as a class and should be classified snare from their seat. If coordination includes mechanisms to protect minorities (supermajority amendment requirements, etc.), the extraction is lower and the reading is more defensible as tangled_rope rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_under_contestation, empirical, 'Whether coordinate construction systematically disadvantages constitutional minorities.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the coordinate construction reading genuinely coexist with judicial supremacy and parliamentary supremacy readings, or does it functionally foreclose them by denying any branch the institutional power to enforce its reading?',
    'Test of actual supremacy claims: can a branch enforce judicial supremacy (by having courts void all legislative acts they deem unconstitutional) under coordinate construction? Can it enforce parliamentary supremacy (by having legislature void all judicial decisions they deem inconsistent with legislative authority)? If neither can be enforced because the other branches will non-comply, the coordinate reading forecloses supremacy claims.',
    'If coordinate construction forecloses supremacy, the relation is `forecloses` rather than `coexists_with`. If branches retain theoretical ability to pursue supremacy (even if politically costly), the relation is `coexists_with`. This affects the strength of the constraint—a foreclosing reading is structurally harder to escape; a coexisting reading is perpetually vulnerable to defection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether coordinate construction forecloses or merely coexists with supremacy readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement_basis(cons_tr_t32, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 8, 0.3).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(cons_be_t32, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 8, 0.18).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 16, 0.21).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 24, 0.24).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement_basis(cons_su_t32, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__coordinate_construction_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the coordinate_construction_reading of the contested kernel constitutional_interpretive_authority. Sibling readings are judicial_supremacy_reading (courts have final authority via constitutional guardianship) and parliamentary_supremacy_reading (legislature has final authority; no branch can void parliamentary acts). The three readings decompose the single kernel question (which branch has final interpretive authority?) into three structurally distinct constraints with different ε values, beneficiary/victim sets, and types. This reading authors moderate extractiveness (0.38) and tangled_rope classification; the judicial reading is expected to author lower extractiveness and snare/rope classification (judges as gate-keepers); the parliamentary reading is expected to author lower extractiveness and rope classification (coordination via legislative authority). All three are linked via network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
