% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right (Strict Scrutiny Framing)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Second Amendment reads: 'A well regulated Militia, being necessary to
 *   the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed.' The individual-right reading interprets
 *   this as protecting an individual liberty to own and carry firearms for
 *   lawful purposes (self-defense, hunting, recreation) that is NOT
 *   conditional on militia service, civic participation, or state-defined
 *   militia duty. This is one contested reading of the amendment's text.
 *   Under this reading, all individuals (who satisfy basic disqualifications
 *   like felony conviction) are beneficiaries of a broad constitutional
 *   protection; the state regulatory authority is constrained by strict
 *   scrutiny; and urban communities facing gun violence are structurally
 *   locked into bearing the costs of broad gun availability. The constraint
 *   extracts from state authority by converting it to a weaker police power,
 *   subject to federal judicial review.
 *
 * KEY AGENTS:
 *   - individual_gun_owners — constitutional beneficiaries whose liberty is protected (moderate power, biographical horizon, mobile exit)
 *   - firearms_manufacturers — structural beneficiaries enlarging civilian market access within an individual-right framework (powerful, generational, arbitrage exit)
 *   - second_amendment_advocacy_organizations — agenda-setter, litigate and maintain interpretive authority via Supreme Court victories (organized power, generational)
 *   - urban_communities_gun_violence_exposed — structural payers bearing concentrated mortality/morbidity costs (powerless, trapped exit, local scope)
 *   - state_regulatory_authority — institutional payer losing regulatory latitude to strict scrutiny review (institutional power, constrained exit)
 *   - Supreme Court majority — agenda-setter, enforces the reading through adjudication and constitutionality review (institutional power)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.42).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right (Strict Scrutiny Framing)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional/political").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '64b3431b-546e-404d-8179-c92cbea95d8e').
narrative_ontology:cs_kernel_codification('64b3431b-546e-404d-8179-c92cbea95d8e', fixed_text).
narrative_ontology:cs_authority_grounding('64b3431b-546e-404d-8179-c92cbea95d8e', lineage).
narrative_ontology:cs_interpretation_layer_present('64b3431b-546e-404d-8179-c92cbea95d8e').
narrative_ontology:cs_reading_relation('64b3431b-546e-404d-8179-c92cbea95d8e', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('64b3431b-546e-404d-8179-c92cbea95d8e', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('64b3431b-546e-404d-8179-c92cbea95d8e', foundational, individual_ownership_right_antecedent_to_state).
narrative_ontology:cs_axiom_status(individual_ownership_right_antecedent_to_state, holdable).
narrative_ontology:cs_axiom_grounding('64b3431b-546e-404d-8179-c92cbea95d8e', individual_ownership_right_antecedent_to_state, deontological).
narrative_ontology:cs_axiom('64b3431b-546e-404d-8179-c92cbea95d8e', secondary, militia_preamble_historical_not_operative).
narrative_ontology:cs_axiom_status(militia_preamble_historical_not_operative, holdable).
narrative_ontology:cs_axiom_grounding('64b3431b-546e-404d-8179-c92cbea95d8e', militia_preamble_historical_not_operative, empirically_contingent).
narrative_ontology:cs_reference_frame('64b3431b-546e-404d-8179-c92cbea95d8e', natural_rights_individual_liberty_framework).
narrative_ontology:cs_drift_state('64b3431b-546e-404d-8179-c92cbea95d8e', contemporary_gun_violence_regulatory_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('64b3431b-546e-404d-8179-c92cbea95d8e', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, second_amendment_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, urban_communities_gun_violence_exposed).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_regulatory_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and carry firearms for lawful purposes (self-defense, hunting, recreation) without requiring justification via militia participation or state-defined civic duty. This reading decouples the constitutional protection from mandatory militia service or active state membership, treating ownership as a standalone liberty. The individual enters the beneficiary set because the reading expands their access and protects it from state regulation that would condition exercise on civic purpose.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Operate within a constitutional framework that protects broad civilian market access. The individual-right reading enlarges the market by protecting ownership as a standalone liberty, not contingent on state-authorized militia duty. Manufacturers can rely on strict scrutiny limiting state regulatory barriers (licensing, background check expansions, safety mandates) that might otherwise compress civilian demand.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_manufacturers, beneficiary,
    powerful, generational, arbitrage, national).

% Set the framing and litigate the boundary: define what 'the right of the people' means, challenge state regulations as unconstitutional infringements, and enforce the reading through Supreme Court victories and legislative pushback. They maintain the interpretive authority over Second Amendment scope and administrate which regulations pass strict scrutiny.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, second_amendment_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Bear the concentrated costs of gun availability: disproportionate exposure to gun homicide, accidental discharge, domestic violence with firearms, and police encounters where weapon presence escalates lethality. These communities cannot exit — they are geographically locked into high-gun-access environments. The individual-right reading constrains the regulatory tools (restrictions, licensing, red-flag laws) these communities' elected representatives would deploy to manage the flow of weapons. They pay through mortality and morbidity.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, urban_communities_gun_violence_exposed, payer,
    powerless, immediate, trapped, local).

% Loses regulatory latitude: cannot tier licensing by geography or risk profile, cannot expand background checks beyond federal baseline, cannot impose magazine limits or safety requirements that survive strict scrutiny. The individual-right reading submits state police power to federal constitutional review, replacing traditional rational-basis deference with heightened scrutiny that strikes down regulations even when democratically enacted. State authority is not eliminated but substantially constrained by the reading's architecture.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Adjudicates the boundary and enforces strict scrutiny via case-by-case review. The reading is sustained by the Court's majority opinion (District of Columbia v. Heller, 2008, and successors); as long as the Court composition holds, the reading's authority derives from this institutional seat. The Court can modulate application (e.g., permitting some regulations as narrowly tailored) or shift by recomposition.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).

% Structurally excluded from the beneficiary/coordination analysis: would challenge the individual-right reading as misinterpreting historical text and Second Amendment ratification intent, would argue for a collective-right or civic-right reading, and would support state regulatory authority to restrict access. Their contestation is present in litigation and public discourse but does not determine the reading's instantiation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_prevention_advocates, excluded,
    organized, biographical, mobile, national).

% Provide expert analysis and competing interpretations: some support the individual-right reading as faithful to the text and Founding-era intent; others argue the text was always tied to militia service and the reading represents a modern revision. Their role is evidentiary and not determinative of the reading's authority, but they populate the interpretive field.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, historical_textualist_scholars, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, judicially enforced constitutional boundary on state regulatory authority over firearms: all individuals below that boundary possess protected liberty to own and carry firearms for lawful purposes; all state regulations above the boundary are presumptively unconstitutional. This coordinates how the political branches approach firearms policy by establishing a floor of individual protection below which democratically-determined restrictions cannot reach.
% TRANSFER_FUNCTION: Transfers regulatory authority from state legislatures (which can impose licensing, permitting, background-check expansion, magazine limits, waiting periods) to federal courts applying strict scrutiny. State legislatures can still regulate, but any regulation face heightened judicial review; regulations that survive that review are approved, while regulations that fail it are struck down. The constraint moves authority from the ballot box to the bench.
% ABSENT_VOICES: Communities experiencing gun violence disproportionately (urban, low-income, communities of color) would dispute that their regulatory interests are protected or heard within the individual-right framing; they argue the reading suppresses their ability to enact democratic firearms restrictions. Gun violence prevention organizations would argue for a collective-right or civic-right reading that makes the right conditional on militia service or civic participation.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight — if the Supreme Court overruled Heller or the amendment's text were repealed — state legislatures would immediately reassert regulatory authority: comprehensive licensing, permitting regimes, background-check expansion, and restrictions on categories of weapons would be enacted in many states within months. The firearms market would contract; manufacturers would face state-level regulation comparable to other dangerous goods. The political economy of firearms would reorganize around state police power, not individual constitutional protection.
% FOUNDING_PROBLEM: The historical purpose was to preserve the states' capacity to maintain armed militias (distinct from federal standing armies) and to protect against federal disarmament of the populace. The individual-right reading reframes this: it grounds the protection in an individual liberty to own firearms for self-defense and lawful purposes, severing the militia justification.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Heller opinion, academic work by Randy Barnett, Eugene Volokh) attest the individual-right reading is faithful to the text and founding intent. Revisionist and collective-right scholars (Cass Sunstein, Carl Bogus, Saul Cornell) attest the founding problem was militia-preservation and the individual-right reading is a modern revision. Legislative history from state ratification (1791) offers competing evidence. No neutral corroboration exists outside the adversarial tradition.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the reading's scope is broad (all individuals are beneficiaries), its implementation is enforced (strict scrutiny review), and the constraint transfers substantial regulatory authority away from democratically-accountable legislatures to the judiciary. Suppression is moderate (0.42) — not high — because the constraint's legitimacy is grounded in a formal constitutional text, not coercive force; the suppression measures the degree to which state alternatives (more restrictive regulations) are foreclosed by strict scrutiny, not the exercise of raw coercion. Theater is low-moderate (0.28): the functional core (the text's protection of individual ownership) is real, but a rising proportion of constitutional litigation is performative — parties litigate fact-specific applications rather than core principle, and the constitutional safe harbor is increasingly settled (extractiveness plateaus after t=20, theater_ratio rises but slowly). The measurement series show extraction rising sharply post-Heller (t=0 to t=15), then plateauing as the reading's authority becomes institutionalized and litigation moves to margins (magazine limits, waiting periods, permitless carry).
 *
 * PERSPECTIVAL GAP:
 *   From the individual-gun-owner and manufacturer seats, the constraint is a liberation — broad protection from state interference. From the urban-community and state-authority seats, it is an extraction of regulatory power. From the Supreme Court seat, it is the neutral application of constitutional text via judicial review. These are not opinion differences but structural divergences: the same constraint instantiates protection for some and constraint for others. The engine computes per-seat directionality from the beneficiary/victim declarations; the authored claim (tangled_rope) bridges coordination (protecting individual liberty) and extraction (suppressing state authority) — which the structural data sustain.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners: beneficiary role, moderate power, mobile exit → derives low directionality (d near 0.2–0.3), approaching subsidized-beneficiary end. Firearms manufacturers: beneficiary role, powerful institutional, arbitrage exit → derives near-beneficiary directionality (d near 0.1–0.2). Second Amendment advocacy organizations: agenda-setter role, organized power → derives beneficiary-side directionality (d near 0.25–0.35) because they administer a constraint that benefits them and their constituency. Urban communities: victim role (payer), powerless, trapped exit → derives high directionality (d near 0.85–0.95), full-target end. State regulatory authority: victim role (payer), institutional power, constrained exit → derives moderate-high directionality (d near 0.65–0.75) — they have institutional tools but lack exit (cannot opt out of the Constitution). Supreme Court majority: agenda-setter but analytical seat, institutional, no real stake in outcomes beyond institutional legitimacy → derives neutral directionality (d near 0.5). The wide range reflects the constraint's asymmetric structure: it benefits some while extracting from others, coordinating a constitutional floor while suppressing alternative regulatory arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving militia capacity and guarding against federal disarmament — has shifted away from militia-maintenance (standing armies are now the norm, state militias are National Guard, the founding militia concern is historical) toward individual self-defense and recreational ownership. The constraint persists, but its justification has drifted. This is not mandatrophy in the sense of complete functional atrophy (the constraint actively structures firearms law and politics), but it is mandate-drift: the stated purpose has been reframed. The reading survives not because the militia problem is live but because the individual-right framing has acquired independent legitimacy through Heller and subsequent jurisprudence. This is a tangled_rope signature, not a piton, because active enforcement (strict scrutiny review, litigation) keeps the constraint sharp; theater_ratio stays low because the functional core remains real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_justification_obsolescence,
    'Is the militia justification in the Second Amendment''s preamble the operative rationale for the individual-right protection, or is it a historical artifact decoupled from the protection''s modern meaning?',
    'Historical scholarship consensus on Founding-era militia concerns and textual interpretation; comparison with how other preambles (e.g., ''promote the general Welfare'') function in constitutional law.',
    'If militia-justification is operative, the individual-right reading requires justification beyond militia concerns (self-defense, recreational use, armed resistance to tyranny); if the preamble is artifact, the protection stands independently. This affects whether state regulation can be justified on public-safety grounds that are militia-unrelated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_justification_obsolescence, conceptual, 'Whether the amendment''s militia preamble grounds the protection or is historical surplus.').

omega_variable(
    strict_scrutiny_calibration,
    'Do regulations that survive strict scrutiny (e.g., prohibitions on felons, narrow permit schemes, ''sensitive places'' restrictions) represent genuine coordination with individual liberty, or are they performative exceptions that preserve the reading''s authority while allowing state enforcement discretion to vary?',
    'Empirical study of how jurisdictions implement post-Heller strict scrutiny: do permitted regulations actually function to prevent gun violence, or do they serve as a governance valve allowing appearance of state regulation while leaving broad civilian access intact?',
    'If strict scrutiny permits functional regulation, the constraint is tangled-rope (real coordination + real asymmetric extraction). If strict scrutiny is a pass-through and regulations are ineffective, the constraint approaches snare (extraction defended by false coordination claims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strict_scrutiny_calibration, empirical, 'Whether narrow permitted regulations represent genuine state capacity or theater masking broad extraction.').

omega_variable(
    reading_substitution_under_court_recomposition,
    'If Supreme Court composition changes and a majority adopts the collective-right or civic-right reading, does this constraint''s ε and type change, or do two distinct constraints coexist (one per reading)?',
    'Constitutional adjudication following Court recomposition; operation of the framework''s kernel-reading infrastructure.',
    'The framework models readings as distinct constraints, not as observables of a single constraint viewed differently. A Court recomposition would instantiate a NEW constraint story (collective_right_reading or civic_right_reading) with a different ε, different beneficiary set, different regulatory scope — not a reinterpretation of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_substitution_under_court_recomposition, conceptual, 'Whether Court recomposition changes this constraint or instantiates a sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(seco_tr_t5, second_amendment_scope__individual_right_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__individual_right_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(seco_tr_t15, second_amendment_scope__individual_right_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__individual_right_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(seco_tr_t25, second_amendment_scope__individual_right_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__individual_right_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(seco_tr_t35, second_amendment_scope__individual_right_reading, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(seco_be_t5, second_amendment_scope__individual_right_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__individual_right_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(seco_be_t15, second_amendment_scope__individual_right_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__individual_right_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(seco_be_t25, second_amendment_scope__individual_right_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__individual_right_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(seco_be_t35, second_amendment_scope__individual_right_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(seco_su_t5, second_amendment_scope__individual_right_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__individual_right_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(seco_su_t15, second_amendment_scope__individual_right_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__individual_right_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(seco_su_t25, second_amendment_scope__individual_right_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__individual_right_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(seco_su_t35, second_amendment_scope__individual_right_reading, suppression_requirement, 35, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment scope kernel decomposes into three constraint stories, one per reading: individual_right_reading (this file), civic_right_reading, and collective_right_reading. Each reading instantiates a different constraint with a distinct beneficiary set, extraction profile, and state regulatory scope. The three are linked via network.affects_constraints to indicate kernel membership. A Supreme Court decision is an instantiation of one reading as law; the constraint for that reading is active while sibling readings persist as contestation in dissent, scholarship, and politics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
