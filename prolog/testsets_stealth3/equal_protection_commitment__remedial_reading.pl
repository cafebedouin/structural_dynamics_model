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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Anti-Caste License for Race-Conscious Measures
 *   domain: constitutional law/political philosophy/social policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the remedial reading — of the equal
 *   protection kernel (fixed text: the Fourteenth Amendment): the Clause
 *   forbids perpetuation of caste and therefore licenses state use of
 *   race-conscious measures to dismantle subordination. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement this reading governs — the regime of remedial programs
 *   (contracting set-asides, targeted admissions, preferential hiring) as
 *   this reading itself assesses it — never for the colorblind counterfactual
 *   the reading opposes. The sibling readings (colorblind_reading,
 *   diversity_reading) are separate constraint files with their own epsilon,
 *   beneficiary sets, and types; they appear here only as network edges and
 *   committer omegas. Claim and metrics are independent authored facts: the
 *   claimed type (tangled_rope) reflects the structure I believe true — a
 *   genuine collective-action function (no private actor dissolves caste the
 *   state itself built) fused with asymmetric extraction (individuals denied
 *   goods by racial classification) held together by active judicial
 *   enforcement — while the metrics describe observed operation across
 *   1978-2023, including the enforcement collapse following the 2023
 *   admissions ruling. KEY AGENTS (by structural relationship): -
 *   historically_subordinated_racial_groups: primary intended beneficiary
 *   (organized/constrained) — receives redirected opportunity -
 *   state_actors_implementing_remediation: agenda-setter and administrative
 *   beneficiary (institutional/arbitrage) — designs, runs, and defends the
 *   programs; collects administrative gains -
 *   courts_as_equal_protection_enforcers: agenda-setter over the permission
 *   line (institutional/constrained) — currently withdrawing enforcement -
 *   racially_disfavored_applicants: primary payer (powerless/constrained) —
 *   bears the classification burden - non_preferred_public_contractors:
 *   secondary payer (organized/mobile) - future_generations: excluded seat
 *   (powerless/trapped) — inherits the outcome, holds no seat in program
 *   design - constitutional_law_academy: analytical observer — maps the
 *   readings, corroborates the genealogy
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: primary intended beneficiary (organized/constrained) — receives redirected seats, contracts, and positions
 *   - state_actors_implementing_remediation: agenda-setter and administrative beneficiary (institutional/arbitrage) — designs and administers programs, collects budgets and discretion, defends them in litigation
 *   - courts_as_equal_protection_enforcers: agenda-setter over the permission line (institutional/constrained) — polices the boundary between permitted remediation and forbidden classification; enforcement posture reversed in 2023
 *   - racially_disfavored_applicants: primary payer (powerless/constrained) — individuals denied admission, promotion, or award by racial criteria they cannot decline to be subject to
 *   - non_preferred_public_contractors: secondary payer (organized/mobile) — firms excluded from race-designated set-asides; litigate and lobby through trade associations
 *   - future_generations: excluded seat (powerless/trapped) — inherit either the repaired or unrepaired opportunity structure; absent from program design
 *   - constitutional_law_academy: analytical observer (analytical/analytical) — documents the reading's genealogy and the vestige-causation record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.46).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.4).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection Remedial Reading: Anti-Caste License for Race-Conscious Measures").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional law/political philosophy/social policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, 'eaaa2a33-3a8d-428c-b66c-85c6d7b158b1').
narrative_ontology:cs_kernel_codification('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', fixed_text).
narrative_ontology:cs_authority_grounding('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', lineage).
narrative_ontology:cs_interpretation_layer_present('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1').
narrative_ontology:cs_reading_relation('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', foundational, state_may_classify_race_to_dismantle_subordination).
narrative_ontology:cs_axiom_status(state_may_classify_race_to_dismantle_subordination, holdable).
narrative_ontology:cs_axiom_grounding('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', state_may_classify_race_to_dismantle_subordination, deontological).
narrative_ontology:cs_axiom('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', secondary, remedial_authority_extends_to_structural_vestiges).
narrative_ontology:cs_axiom_status(remedial_authority_extends_to_structural_vestiges, holdable).
narrative_ontology:cs_axiom_grounding('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', remedial_authority_extends_to_structural_vestiges, empirically_contingent).
narrative_ontology:cs_reference_frame('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', reconstruction_anticaste_guarantee).
narrative_ontology:cs_drift_state('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', contemporary_post_sffa_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('eaaa2a33-3a8d-428c-b66c-85c6d7b158b1', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_implementing_remediation).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, racially_disfavored_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, non_preferred_public_contractors).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, anticaste_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, vestige_causation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members receive redirected opportunities — university admissions offers, public-contract awards, positions — through programs justified as repair of state-enforced exclusion. Civil-rights organizations advocate for the programs' continuation and expansion. Membership in the group is ascribed and lifelong; no one exits the classification system by moving or opting out, and the group's internal composition (by class, immigrant lineage, and subgroup) determines who actually receives the transfers.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Federal, state, and local agencies design set-aside percentages, contracting goals, admissions supplements, and hiring benchmarks; they collect the administrative appropriations, staffing, and discretionary authority the programs carry, and they defend the programs in court. When judicial weather shifts, they can restructure — converting racial criteria into facially neutral proxies, shifting to geographic or disadvantage-based formulas, or devolving program design to subordinate units — which makes their exposure to the arrangement's fortunes unusually adjustable.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_implementing_remediation, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, state_actors_implementing_remediation, beneficiary).

% The judiciary polices the boundary between permitted remediation and forbidden classification, case by case, with each generation of judges redrawing the line. Its posture moved across the interval from upholding congressionally found remediation, to strict scrutiny of every classification, to striking down race-conscious admissions outright in 2023 while leaving adjacent domains open. Bound by precedent and the constitutional text, it cannot exit adjudication; its enforcement choices are the arrangement's life support.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, courts_as_equal_protection_enforcers, agenda_setter,
    institutional, generational, constrained, national).

% Individuals whose applications are scored down or excluded because of their racial classification — denied a seat, a promotion, or an award on criteria tied to ancestry. Each bears the loss at a specific biographical moment; alternatives exist (other schools, other employers, private markets) but at real cost, and no one can decline to be classified. Individually they have little leverage; organized through litigation vehicles, their coalition proved capable of overturning the arrangement's largest domain.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, racially_disfavored_applicants, payer,
    powerless, biographical, constrained, national).

% Firms ineligible for race-designated set-aside contracts bid on the remainder of public work and absorb the revenue those designations redirect. Trade associations litigate against the programs and lobby legislatures to repeal them. Exit to purely private-sector work is available at the cost of margin and relationships, making this seat materially more mobile than individual applicants.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, non_preferred_public_contractors, payer,
    organized, biographical, mobile, national).

% People not yet born will inherit either the repaired or the unrepaired opportunity structure, along with whatever resentments and dependencies the remedy's long operation compounds. No seat in current program design speaks for them; both the unpaid debt and the remedy's costs accumulate across cohorts while the present occupants of every other seat decide.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, future_generations, excluded,
    powerless, civilizational, trapped, national).

% Scholars map the readings of the Clause, reconstruct the Reconstruction-era record, and test the vestige-causation claims with historical and economic data. They hold no stake in program outcomes beyond professional reputation, and their testimony and treatises supply the external corroboration on which the founding-problem assessment partly rests.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, constitutional_law_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dismantling entrenched group subordination is a collective-action problem no individual, firm, or market process solves: caste hierarchies were built by centuries of coordinated state action and reproduce themselves through accumulated advantage. The arrangement coordinates state power — set-asides, targeted admissions, preferential contracting — to redirect opportunity flows faster than uncoordinated attrition would, stated here without evaluation of whether it succeeds.
% TRANSFER_FUNCTION: Moves access to scarce goods — university seats, public contracts, employment positions, and the lifetime earnings attached to them — from applicants and firms selected without regard to race toward members of designated historically subordinated groups, via state-administered selection criteria backed by legal enforcement.
% ABSENT_VOICES: The individual denied applicant appears only as a statistical category — no seat in program design represents the specific person who lost the specific seat. Future generations who inherit the outcome are absent entirely. Proponents of class-based (income-linked) preferences are audible in public debate but structurally marginal inside program design, where racial categories are the operating variables.
% DISAPPEARANCE_RATIONALE: If the permission vanished overnight, hundreds of public contracting programs, court-ordered desegregation plans, and hiring policies would become legally indefensible at once; opportunity flows would reroute through race-neutral criteria; agencies built around remedial compliance would reorganize or dissolve; and the political coalition structure built on defending or attacking the programs would lose its organizing object. The world does not stay put — it rearranges around the removal.
% FOUNDING_PROBLEM: Built to solve the failure of formally neutral rules to dismantle a caste system the state itself had constructed and enforced for centuries: Reconstruction's unresolved question of what the Fourteenth Amendment permits and requires toward the freed population and their descendants, once formal-equality instruments proved unable to undo accumulated state-made disadvantage.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: economic historians and demographers document persistent intergenerational gaps traceable to specific state policies (redlining maps, discriminatory GI Bill administration, school-segregation records), and courts' own factual findings in desegregation and contracting cases attest the vestige record. Notably, colorblind-reading advocates concede the historical facts of state-enforced caste while disputing the remedial inference — the problem's existence is attested even by the reading's principal opponents, who contest only its solution.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).
:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.46, terminal) sits in the expected band: the arrangement moves concrete scarce goods (university seats, public contracts, positions) from racially disfavored individuals to preferred ones under state coercion — a real, bounded, targeted transfer, smaller in reach than monopoly-scale extraction but borne by identifiable people. Suppression (0.40, terminal) reflects the post-2023 enforcement decay: the machinery that compelled compliance and excluded the colorblind alternative was strongest circa 2013 and has been partially dismantled by the apex court itself. Theater ratio (0.42, terminal) rises monotonically across the interval — compliance documentation, disparity studies commissioned and shelved, goal-setting without enforcement teeth — classic Goodhart drift as proxy compliance replaces direct remedial delivery; the underlying transfers remain real, so the ratio stays below the piton threshold. Accessibility collapse (0.45): race-neutral alternatives (class-based preferences, place-based investment, outreach) remain partially viable and the reading's own narrow-tailoring doctrine requires preferring them when adequate — alternatives bend but do not vanish. Resistance (0.75) is among the highest of any domestic legal constraint: sustained litigation culminating in the 2023 reversal, ballot-measure repeals in multiple states, and durable political backlash. The temporal series share one grid (1978, 1986, 1994, 2003, 2013, 2023) with every tracked metric authored at every point: extractiveness peaks at the 1990s set-aside high-water mark then declines; suppression_requirement ratchets upward for thirty-five years as enforcement defended against escalating attack, then drops sharply in 2023 when the enforcing court switched sides — an enforcement-decay trajectory, not noise. Coalition note: the primary payer seat is individually powerless, but class-level coalition (litigation associations) proved decisive — the 2023 reversal was won by organized payers, which is why resistance outruns suppression at interval end.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical text. From the denied applicant's chair, the rule reads as the very caste-marking it claims to dismantle: a state assigning burdens by ancestry. From the beneficiary chair, the same rule reads as the minimum available instrument of repair for harms no individual caused and no market will undo. From the implementing agency's chair, it is ordinary governance with budgetary upside. From the bench, it migrated across the interval from permission (1980s) to strict scrutiny (1990s-2000s) to withdrawal (2023). The engine computes this divergence per seat from power, exit, and directional data; nothing in the claimed type adjudicates it. The excluded future-generations seat matters for the consensus-provenance check: unanimity inside program design is purchased partly by leaving the inheriting cohorts outside the room.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries anchor the low-d end: historically_subordinated_racial_groups receive the transfer (d near 0.0), with their constrained exit doing no amplification since they are not targets. State implementers hold a dual position — declared beneficiary, but also the administrative runner — so their derived d sits low-to-mid; their arbitrage-grade exit (redesigning programs into facially neutral proxies when challenged) further damps their effective extraction exposure. Payers anchor the high-d end: racially_disfavored_applicants bear the full classification burden with constrained exit (they cannot decline to be raced), placing them near the full-target end; non_preferred_public_contractors are similar but their mobility damps chi somewhat. Courts sit mid-range: they wield the constraint rather than subsidize from it, and their constrained exit (precedent-bound) keeps them from the beneficiary pole. Suppression is authored as a raw structural property and is deliberately NOT scaled — only extractiveness rides the directionality and scope modifiers in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — what a caste-building state owes the descendants of those it subordinated — is live by this reading's lights and contested outside them; the R5 triple records status=live with disappearance_verdict=world_rearranges, so no zombie flag fires. But the theater_ratio trajectory (0.15 to 0.42) is the early-warning signature: if the founding problem were later declared dead while the program machinery persisted, the mismatch consumer would flag capture/zombie conversion and the predicted terminal type is piton — an apparatus administering the appearance of repair. The tangled_rope claim is what prevents mislabeling in both directions: the colorblind sibling's move (treating the whole arrangement as pure extraction, a snare of racial classification) erases the genuine coordination function — dismantling state-built caste is a collective-action problem no market or charity solves; the apologetic move (treating it as pure rope) erases the identifiable people denied goods by race. Both halves are structurally present, both are enforced, so both must be named. The missing sunset (omega: transition_endpoint_indeterminacy) is the mandatrophy hinge: a transitional arrangement with no operational endpoint is structurally invited to outlive its mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (remedial_reading) of the equal_protection_commitment kernel; would the colorblind_reading sibling invert the entire structural surface?',
    'Doctrine evolution at the apex court, constitutional amendment, or emergence of superprecedent entrenching one reading; tracked as sibling files, not inside this one.',
    'Under the colorblind sibling, the beneficiary and victim sets swap places entirely: remedial programs become forbidden classifications, state implementers become violators rather than administrators, and this reading''s epsilon collapses toward the sibling''s own assessment of the same programs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: which reading of the equal protection kernel controls state racial classification.').

omega_variable(
    vestige_causation_dispute,
    'Are present-day racial disparities causally traceable to state-enforced caste (vestiges of official discrimination), or do they arise from post-intervention factors the remedial frame cannot claim?',
    'Causal-inference work on policy natural experiments: redlining-map discontinuities, GI Bill administration records, school-desegregation cohort studies, wealth-transfer tracing.',
    'If vestige causation fails broadly, the reading''s secondary axiom loses its empirical footing and the drift vector moves from repudiation_pressure toward axiom_overriding at severe magnitude; if it holds, the remedial object remains live and the permission retains its warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vestige_causation_dispute, empirical, 'Whether the reading''s factual predicate (live vestiges of state-built caste) survives empirical testing.').

omega_variable(
    transition_endpoint_indeterminacy,
    'When does remediation end? The reading characterizes its measures as transitional, but no operational sunset test exists — no metric, date, or tribunal defines the state at which race-conscious measures must stop.',
    'Doctrinal specification of an endpoint criterion (disparity-band thresholds, cohort-completion tests), or candid abandonment of the transitional characterization in favor of a permanent-structure account.',
    'Without an endpoint, the arrangement''s transitional justification erodes while its machinery persists — the classic path from hybrid coordination-extraction toward inertial maintenance; with a specified endpoint, the sunset question becomes decidable and the mandatrophy clock starts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_endpoint_indeterminacy, preference, 'The missing sunset: the reading''s transitional self-description has no operational termination condition.').

omega_variable(
    beneficiary_inversion_under_observer_shift,
    'Who counts as historically subordinated for purposes of the remedy? The beneficiary/victim structure inverts depending on observer position — in inter-minority competition (e.g., admissions), the same mechanism that benefits one subordinated group extracts from another.',
    'Comparative seat analysis across program domains: which ascribed groups receive net transfers and which bear net denials, disaggregated by program type and region.',
    'If the beneficiary set is internally divided, effective extraction concentrates on the sub-groups the frame cannot see, and per-seat classifications diverge sharply from the aggregate picture; the reading''s own coalition becomes partially a payer coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_inversion_under_observer_shift, conceptual, 'Boundary instability in the beneficiary set: the remedy''s categories do not partition cleanly into helped and harmed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_remedial_tr_t1978, equal_protection_commitment__remedial_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement_basis(ep_remedial_tr_t1978, observed).
narrative_ontology:measurement(ep_remedial_tr_t1986, equal_protection_commitment__remedial_reading, theater_ratio, 1986, 0.2).
narrative_ontology:measurement_basis(ep_remedial_tr_t1986, observed).
narrative_ontology:measurement(ep_remedial_tr_t1994, equal_protection_commitment__remedial_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement_basis(ep_remedial_tr_t1994, observed).
narrative_ontology:measurement(ep_remedial_tr_t2003, equal_protection_commitment__remedial_reading, theater_ratio, 2003, 0.33).
narrative_ontology:measurement_basis(ep_remedial_tr_t2003, observed).
narrative_ontology:measurement(ep_remedial_tr_t2013, equal_protection_commitment__remedial_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement_basis(ep_remedial_tr_t2013, observed).
narrative_ontology:measurement(ep_remedial_tr_t2023, equal_protection_commitment__remedial_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement_basis(ep_remedial_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(ep_remedial_be_t1978, equal_protection_commitment__remedial_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement_basis(ep_remedial_be_t1978, observed).
narrative_ontology:measurement(ep_remedial_be_t1986, equal_protection_commitment__remedial_reading, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement_basis(ep_remedial_be_t1986, observed).
narrative_ontology:measurement(ep_remedial_be_t1994, equal_protection_commitment__remedial_reading, base_extractiveness, 1994, 0.56).
narrative_ontology:measurement_basis(ep_remedial_be_t1994, observed).
narrative_ontology:measurement(ep_remedial_be_t2003, equal_protection_commitment__remedial_reading, base_extractiveness, 2003, 0.53).
narrative_ontology:measurement_basis(ep_remedial_be_t2003, observed).
narrative_ontology:measurement(ep_remedial_be_t2013, equal_protection_commitment__remedial_reading, base_extractiveness, 2013, 0.49).
narrative_ontology:measurement_basis(ep_remedial_be_t2013, observed).
narrative_ontology:measurement(ep_remedial_be_t2023, equal_protection_commitment__remedial_reading, base_extractiveness, 2023, 0.46).
narrative_ontology:measurement_basis(ep_remedial_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(ep_remedial_su_t1978, equal_protection_commitment__remedial_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement_basis(ep_remedial_su_t1978, observed).
narrative_ontology:measurement(ep_remedial_su_t1986, equal_protection_commitment__remedial_reading, suppression_requirement, 1986, 0.45).
narrative_ontology:measurement_basis(ep_remedial_su_t1986, observed).
narrative_ontology:measurement(ep_remedial_su_t1994, equal_protection_commitment__remedial_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement_basis(ep_remedial_su_t1994, observed).
narrative_ontology:measurement(ep_remedial_su_t2003, equal_protection_commitment__remedial_reading, suppression_requirement, 2003, 0.62).
narrative_ontology:measurement_basis(ep_remedial_su_t2003, observed).
narrative_ontology:measurement(ep_remedial_su_t2013, equal_protection_commitment__remedial_reading, suppression_requirement, 2013, 0.66).
narrative_ontology:measurement_basis(ep_remedial_su_t2013, observed).
narrative_ontology:measurement(ep_remedial_su_t2023, equal_protection_commitment__remedial_reading, suppression_requirement, 2023, 0.4).
narrative_ontology:measurement_basis(ep_remedial_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, diversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what equal protection requires for race' decomposes into three structurally distinct constraints — one per reading of the fixed-text kernel. Their epsilon values differ because their beneficiary/victim structures differ: the colorblind sibling declares the remedial programs themselves the violation (its victim set includes this reading's beneficiaries); the diversity sibling permits a narrower class of measures with a different compelling-interest predicate; this remedial reading licenses the broadest remedial class. Upstream/downstream: the colorblind reading supplies the interpretive pressure that ultimately narrowed this one (the 2023 reversal reasons in near-colorblind terms), and the diversity reading historically absorbed cases this reading could no longer hold after Bakke restricted standalone remediation. All three files link each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
