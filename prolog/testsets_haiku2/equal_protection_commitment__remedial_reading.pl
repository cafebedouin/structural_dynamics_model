% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection: Remedial Reading (Race-Conscious Subordination Remedy)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The remedial reading of equal protection forbids state perpetuation of
 *   caste-like subordination and authorizes race-conscious measures to
 *   dismantle it. This reading treats the Constitution's guarantee of equal
 *   protection as a commitment not merely to color-blindness but to the
 *   eradication of inherited racial subordination embedded in law and state
 *   institutions. The reading creates a sharp beneficiary/victim inversion
 *   relative to prior color-blind regimes: historically subordinated groups
 *   become beneficiaries of remedial state action; historically privileged
 *   groups enter the victim set when they lose preferential access they
 *   previously held. The claim/metric gap is deliberate and structural: the
 *   remedial reading claims to instantiate coordination around a shared
 *   commitment to dismantle subordination (rope framing), while the authored
 *   metrics describe a substantially extractive, actively enforced
 *   arrangement (tangled_rope). The engine will compute the per-seat type
 *   divergence from this structural asymmetry — the constraint's defenders
 *   experience it as coordination; its payers experience it as extraction
 *   with legitimacy cover.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: primary beneficiary; gain authorized access to selective institutions and government contracting through race-conscious remediation
 *   - state_actors_implementing_remediation: agenda-setter; hold power to design and implement race-conscious measures within constitutional bounds
 *   - historically_privileged_racial_groups: primary victim; lose preferential access they previously held under color-blind or color-advantaging regimes
 *   - federal_judiciary: observer; adjudicates whether remedial measures fit the constraint's bounds
 *   - civil_rights_advocacy_organizations: beneficiary + secondary agenda-setter; litigate and lobby for remedial measures, shape doctrine
 *   - opposition_coalitions (colorblind reading proponents): excluded; their core premises are treated as foreclosed by the remedial reading's commitment to subordination remediation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection: Remedial Reading (Race-Conscious Subordination Remedy)").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional/political").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '0b5dc48e-4122-4e1a-911d-03b7a28d6876').
narrative_ontology:cs_kernel_codification('0b5dc48e-4122-4e1a-911d-03b7a28d6876', formalized).
narrative_ontology:cs_authority_grounding('0b5dc48e-4122-4e1a-911d-03b7a28d6876', lineage).
narrative_ontology:cs_interpretation_layer_present('0b5dc48e-4122-4e1a-911d-03b7a28d6876').
narrative_ontology:cs_reading_relation('0b5dc48e-4122-4e1a-911d-03b7a28d6876', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('0b5dc48e-4122-4e1a-911d-03b7a28d6876', equal_protection_commitment__diversity_reading, influences).
narrative_ontology:cs_axiom('0b5dc48e-4122-4e1a-911d-03b7a28d6876', foundational, subordination_remediation_constitutionally_required).
narrative_ontology:cs_axiom_status(subordination_remediation_constitutionally_required, holdable).
narrative_ontology:cs_axiom_grounding('0b5dc48e-4122-4e1a-911d-03b7a28d6876', subordination_remediation_constitutionally_required, deontological).
narrative_ontology:cs_axiom('0b5dc48e-4122-4e1a-911d-03b7a28d6876', secondary, race_conscious_remedy_necessary_for_subordination_elimination).
narrative_ontology:cs_axiom_status(race_conscious_remedy_necessary_for_subordination_elimination, holdable).
narrative_ontology:cs_axiom_grounding('0b5dc48e-4122-4e1a-911d-03b7a28d6876', race_conscious_remedy_necessary_for_subordination_elimination, empirically_contingent).
narrative_ontology:cs_reference_frame('0b5dc48e-4122-4e1a-911d-03b7a28d6876', subordination_remediation_mandate).
narrative_ontology:cs_drift_state('0b5dc48e-4122-4e1a-911d-03b7a28d6876', contemporary_political_backlash, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b5dc48e-4122-4e1a-911d-03b7a28d6876', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_implementing_remediation).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_racial_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, white_working_class_constituencies).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, asian_american_applicants).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, equal_protection_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, subordination_remediation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the accumulated effects of formal and informal racial subordination encoded in law, institutional practice, and property distribution. The remedial reading authorizes state measures (affirmative action, targeted contracting, voting-district remediation) that explicitly use race to reverse those effects. They benefit from race-conscious remediation but cannot unilaterally demand it — implementation depends on state actor cooperation and judicial approval of the remedy design.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Hold the power to design and implement remedial race-conscious measures within the equal-protection constraint as the remedial reading frames it. They decide which forms of subordination to remedy, which groups to include, what time horizon the remedy spans. They face judicial scrutiny and political opposition but retain substantial discretion over remedy design and implementation speed.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_implementing_remediation, agenda_setter,
    institutional, generational, arbitrage, national).

% Lose access to preferential opportunities (university admissions, government contracting) that they had occupied under race-neutral or race-advantaging regimes. The remedial reading treats their loss of these opportunities as a justified cost of dismantling subordination, not as discrimination. They have legal recourse to challenge remedies as exceeding their purpose or design, but the remedial reading places the burden on them to prove excess rather than on the state to prove necessity.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_racial_groups, payer,
    powerful, biographical, constrained, national).

% Adjudicates whether state remedial measures fit within the bounds the remedial reading sets: does the measure target genuine prior subordination, is the remedy narrowly tailored to dismantle that subordination, does it respect individual dignity while addressing group harm. Courts do not implement remedies but determine what the constitutional constraint permits and requires.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Litigate and lobby for remedial measures: they frame legal challenges, propose remedy designs, testify about subordination effects, and organize constituencies that benefit. They influence state actors toward more expansive remediation and shape judicial doctrine through briefs and testimony. They are not government but their advocacy directly shapes how state actors implement the remedial reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations, agenda_setter).

% Argue the remedial reading is incoherent or illegitimate: that equal protection forbids ANY state use of racial classification. They are excluded from the remedial reading's own framework — their core premises are treated as foreclosed by the reading's foundational axiom about subordination remediation. They mount constitutional challenges and political opposition but cannot argue within the remedial reading itself.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_opposition_coalitions, excluded,
    powerful, biographical, constrained, national).

% Experience remedial measures (affirmative action in college admissions, race-conscious hiring) as direct loss of opportunity even though they did not personally create the subordination being remedied. The remedial reading does not distinguish their claim to innocence from the claim of historically privileged groups generally; they pay the same cost but lack the institutional power to mount effective legal or political challenges at the federal level.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, white_working_class_constituencies, payer,
    moderate, biographical, constrained, regional).

% Experience remedial race-conscious measures in selective-admissions contexts where they are neither the historical beneficiary of subordination nor its historical perpetrator. They face the measurable cost of remedial preferences for other groups without the narrative fit the remedial reading provides for other historically privileged groups. Their positioning creates doctrinal friction with the remedial reading's own logic about historical subordination.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, asian_american_applicants, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, state_actors_implementing_remediation).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes that equal protection forbids states from perpetuating inherited caste-like subordination: it coordinates a shared commitment that some forms of subordination (those embedded in law and reproduced through state action) are incompatible with equal citizenship. This solves a coordination problem between state actors: no individual state wants to be the only one perpetuating caste structures, but without a binding commitment all would do so for administrative convenience.
% TRANSFER_FUNCTION: Moves opportunity and resource access from historically privileged racial groups (who held preferential access under prior race-neutral or race-advantaging law) to historically subordinated groups (who gain race-conscious placement in selective institutions and government contracting). The transfer occurs when state actors implement remedial programs; the magnitude depends on how aggressively subordination is defined and remediated.
% ABSENT_VOICES: Observers of the constraint from within the colorblind reading's framework are structurally excluded: they would argue that race-conscious remediation violates equal protection rather than enforces it, but the remedial reading treats their core premise (state color-blindness) as foreclosed by the prior commitment to remedy subordination. Descendants of enslaved peoples who would demand reparations beyond what the remedial reading currently authorizes are also partly absent — the reading does not resolve whether remediation must extend to economic redistribution or can stop at equal access.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight — if equal protection reverted to pure color-blindness and state actors could no longer explicitly use race to remedy subordination — selective institutions would restructure admissions (race-based legacy preferences would return, socioeconomic proxies would replace race-conscious remediation), government contracting would revert to race-neutral procurement (which historically favored white-owned firms), voting districts would no longer be drawn to remedy dilution of minority voting power. The disappearance would not make subordination vanish but would remove the constitutional authorization for race-conscious remediation of it.
% FOUNDING_PROBLEM: Formal legal segregation and slavery left behind inherited subordination: restricted property ownership, segregated schooling that persisted even after formal desegregation, occupational exclusion, voter suppression, and wealth gaps that reproduced themselves across generations. The remedial reading claims equal protection must authorize explicit race-conscious action to dismantle these structures because race-neutral law alone perpetuated them (facially neutral policies that had racially disparate effects continued the subordination under a color-blind veil).
% FOUNDING_PROBLEM_CORROBORATION: The remedial reading's own beneficiaries and civil-rights organizations attest the founding problem is live and requires race-conscious remedy. Federal courts have acknowledged persistent subordination effects (voting Rights Act findings on dilution of minority voting power; documented wealth gaps in housing and education). Critics and the colorblind reading attest the founding problem has been substantially addressed by formal legal equality and color-blind enforcement, and that the remedial reading mistakes the persistence of private disparities (which color-blind law cannot remedy) for the persistence of state-caused subordination (which it can). The corroboration is sharp: the remedial reading's authority structure rests on one reading of historical fact; the competing readings rest on a different reading of the same historical fact.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.38 because the remedial reading genuinely solves a coordination problem (state subordination is costly and destabilizing for all participants), but extractiveness rises to 0.52 because the remedy creates new asymmetries: state actors gain legitimacy and political support from remedial constituencies without bearing the redistribution cost directly; historically subordinated groups gain access but remain dependent on state actors' implementation choices; historically privileged groups pay a measurable cost (lost opportunity) without input into remedy design. The measurement series captures this: base_extractiveness rises steeply through the first 20 time points (as remedial programs scale up and the cost to privileged groups becomes concrete) then plateaus at 0.52 (the constraint stabilizes at a new equilibrium of remediation intensity). Theater_ratio remains moderate (0.28) because the remedial measures have real effects on admissions and contracting — the theater is not the measures themselves but the framing of remediation as correcting prior subordination rather than creating new group favoritism. Suppression_requirement rises similarly (0.35 to 0.48) because enforcement requires sustained judicial scrutiny and state commitment against political opposition from those who pay the cost. Accessibility_collapse is moderate (0.62): alternatives to the remedial reading exist (colorblind, diversity) and some privileged groups have legal recourse to challenge remedies; but once the remedial reading is established as constitutional law, alternatives for state actors shrink substantially. Resistance is high (0.71): the remedial reading meets sustained political and legal resistance from opposition coalitions and from privileged groups contesting individual remedial measures.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (historically subordinated groups + implementing state actors) experiences this as essential coordination: without the remedial reading's authorization, subordination persists indefinitely because color-blind law cannot remedy facially neutral practices with disparate effects. From this seat, the constraint is genuine rope — it solves a problem no individual state can solve alone (dismantling inherited subordination requires coordinated commitment). The victim seat (historically privileged groups) experiences it as extraction with constitutional cover: they lose concrete opportunity, the loss is attributed to their group's prior advantage rather than their individual fault, and they have limited recourse. The opposition seat experiences it as coherent violation: the remedial reading violates equal protection by using the tool (race-conscious classification) that equal protection was meant to forbid. The federal judiciary experiences it as boundary-setting: the constraint permits remediation but requires narrow tailoring and fit to actual subordination; judges constantly adjudicate whether remedies exceed these bounds. This divergence is the structural point — the engine computes it from the stakeholder positions, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups: beneficiary role, organized power, constrained exit, generational time horizon → d approaches 0.2 (beneficiaries with limited arbitrage options; they benefit but cannot walk away). State actors implementing remediation: agenda-setter role, institutional power, arbitrage exit options → d near 0.35 (they set the terms and can revise remedy design; they benefit from legitimacy but bear reputational cost from opposition). Historically privileged groups: payer role, powerful power, constrained exit, biographical time horizon → d approaches 0.75 (they pay the concrete cost of lost opportunity; powerful but exit-constrained by the constraint's scope; biographical horizon means the cost hits within their lifetime). Federal judiciary: observer role, institutional power, analytical exit → d = 0.5 (boundary-setters; neither beneficiaries nor payers, measuring the constraint's operation). The directionality overrides correct for asian-american applicants (moderate power, biographical horizon, payer role → baseline d ≈ 0.68) but override downward to 0.65 because their positioning creates doctrinal friction with the remedial reading's own logic about historical subordination; they are payers without the narrative fit that justifies payment for other groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading avoids false mandatrophy by grounding its legitimacy in a genuine prior problem: formal and informal subordination embedded in law and state institutions created measurable, documented harms (wealth gaps, educational exclusion, political disenfranchisement). The founding problem is not invented to justify the remedy; instead, the remedy is authorized specifically because the founding problem persists despite color-blind law. However, the constraint risks mandatrophy if remedial scope expands beyond demonstrable subordination effects (e.g., if remedies persist for groups that have achieved parity in opportunity, or if remedies are applied to remedy only group disparities unconnected to state action). The temporal measurement trajectory shows extractiveness plateauing at 0.52 rather than continuing to rise, suggesting the constraint reaches a stable equilibrium where remedial intensity is calibrated to maintain parity but not to achieve permanent group advantage. If extractiveness were to resume rising toward 0.70+, or if theater_ratio were to climb above 0.40, that would signal mandatrophy onset: the remedy's purpose is being replaced by group favoritism, enforcement is becoming performative rather than targeted. The current trajectory shows the constraint holding its mandate — remedial purpose remains primary, though the cost to privileged groups remains substantial and contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_vs_preferential_boundary,
    'At what point does a remedial measure cross from remedying documented subordination into creating new group favoritism? Where is the boundary between justified correction and unjustified preference?',
    'Temporal metrics on remedy effects: measure whether remedial programs produce parity in outcomes (stops the bleeding) vs. creating new advantage for the remedial group (overshoots parity). If remedies consistently stop at parity and do not persist after parity is achieved, the boundary is being respected; if remedies persist or amplify beyond parity, the boundary is being transgressed.',
    'If the boundary is transgressed, the remedial reading risks collapse into an incoherent preference system that violates its own logic (perpetuating subordination of the group now disfavored). Mandatrophy would follow: remedy becomes permanent favoritism maintained by theater and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_preferential_boundary, empirical, 'Whether remedial measures respect the boundary between correcting documented subordination and creating new group favoritism.').

omega_variable(
    historical_subordination_definition,
    'What counts as historical subordination entitling a group to remedial measures? How do we distinguish subordination caused by state action (remediable under equal protection) from subordination caused by private discrimination or historical accident (not remediable)?',
    'Document which groups the remedial reading recognizes as entitled to remedial measures and on what evidentiary basis. Compare documentary evidence of state-caused subordination (formal segregation, discriminatory law, state-enforced property restrictions) vs. state-tolerated but not state-caused subordination (private discrimination, cultural subordination, subordination in non-state institutions).',
    'A narrow definition (only groups with documented state-caused subordination) limits remedial scope and reduces tension with the colorblind reading; a broad definition (any group experiencing measurable subordination effects) expands scope and sharpens the conflict. The remedial reading''s coherence depends on holding the boundary between state-caused and state-tolerated subordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_subordination_definition, empirical, 'The definition of historical subordination that activates the remedial reading''s authorization.').

omega_variable(
    remedy_temporality_and_sunset,
    'How long should remedial measures persist? Is the remedial reading temporally bounded (remedies sunset once parity is achieved) or indefinite (remedies persist as permanent correction of inherited subordination)?',
    'Trace the duration of remedial programs in practice and their explicit or implicit renewal conditions. Do programs have articulated sunset triggers (when subordination effects are measured at parity)? Do they persist indefinitely despite achieved parity?',
    'A temporally bounded remedial reading (with sunset clauses) is more coherent and less vulnerable to mandatrophy; an indefinite reading risks becoming permanent group favoritism detached from its subordination-remediation purpose. Temporal boundedness also addresses the asian-american applicant problem: if affirmative action is remedial, when is the remedy complete?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_temporality_and_sunset, conceptual, 'Whether the remedial reading is temporally bounded to the duration of measurable subordination effects or indefinite.').

omega_variable(
    colorblind_vs_remedial_axiom_foreclosure,
    'Is the colorblind reading genuinely foreclosed by the remedial reading''s core axiom (subordination remediation is required), or do the two readings merely occupy different normative frameworks that could coexist in a pluralistic polity?',
    'Test whether a hypothetical judge could hold both axioms simultaneously without logical contradiction: ''equal protection forbids perpetuation of subordination'' (remedial) AND ''equal protection forbids any state use of racial classification'' (colorblind). The answer determines whether foreclosure is genuine or merely conflict.',
    'If foreclosure is genuine (the axioms directly contradict), the remedial reading''s claim to legitimacy rests on the axiom being correct and the colorblind axiom being false or coherent. If the axioms can coexist, the foreclosure is less about logical necessity and more about competing normative commitments, which would weaken the remedial reading''s authority claim. The remedial reading''s entire justification structure depends on subordination remediation being constitutionally mandatory, not merely permitted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_vs_remedial_axiom_foreclosure, conceptual, 'Whether the remedial reading''s core axiom logically forecloses the colorblind reading or merely competes with it.').

omega_variable(
    asian_american_applicant_doctrinal_tension,
    'How does the remedial reading address applicants from groups that neither perpetrated historical subordination nor experience it (e.g., asian americans in selective admissions)? Is their loss of opportunity justified by the subordination-remediation rationale, and if so, how?',
    'Examine how the remedial reading''s own doctrine handles this case: do courts rely on subordination history of the remedial group to justify preferences, and if so, do they address why groups without that history (that also face discrimination in some contexts) should pay the cost?',
    'If the remedial reading cannot coherently address this case within its own subordination-remediation logic, the constraint faces a coherence problem: it is applying costs to groups without a clear justification grounded in their role in prior subordination. This would signal either conceptual drift (remediation becoming cover for group favoritism) or missing conceptual machinery to handle multi-group competition for remedial status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asian_american_applicant_doctrinal_tension, conceptual, 'The remedial reading''s coherence in addressing groups that neither perpetrated nor experience the subordination being remedied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t5, equal_protection_commitment__remedial_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(equa_tr_t5, observed).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__remedial_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t15, equal_protection_commitment__remedial_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(equa_tr_t15, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__remedial_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t25, equal_protection_commitment__remedial_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(equa_tr_t25, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__remedial_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_commitment__remedial_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(equa_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t5, equal_protection_commitment__remedial_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(equa_be_t5, observed).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__remedial_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t15, equal_protection_commitment__remedial_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(equa_be_t15, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__remedial_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t25, equal_protection_commitment__remedial_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(equa_be_t25, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__remedial_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_commitment__remedial_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(equa_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t5, equal_protection_commitment__remedial_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement_basis(equa_su_t5, observed).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__remedial_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t15, equal_protection_commitment__remedial_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(equa_su_t15, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__remedial_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t25, equal_protection_commitment__remedial_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(equa_su_t25, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__remedial_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_commitment__remedial_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(equa_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__remedial_reading, 0.18).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel has three structurally distinct constraint readings: colorblind_reading (forbids race-conscious classification), diversity_reading (permits race as one factor for educational diversity), and remedial_reading (forbids subordination, permits race-conscious remediation). Each reading instantiates a different constraint with different beneficiary/victim structures, different ε values, and different persistence mechanisms. The remedial_reading (this constraint) FORECLOSES the colorblind_reading within a single constitutional framework but INFLUENCES the diversity_reading through different justifications for the same remedial tools. Sibling constraints are linked via network.affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
