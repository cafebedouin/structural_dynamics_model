% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Authority
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story captures the judicial supremacy reading of
 *   constitutional authority: the claim that the constitutional text
 *   establishes courts as final, unchallengeable arbiters of all
 *   constitutional questions, with authority to invalidate legislative and
 *   executive acts without remedy. The reading is instantiated as a
 *   tangled_rope — it performs a genuine coordination function (final
 *   settlement of constitutional disputes) while extracting asymmetric power
 *   from elected branches through the counter-majoritarian veto. The
 *   claimed_type and metrics are authored independently per the ε-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: Primary agenda_setter (institutional/identity_locked) — holds interpretive monopoly
 *   - federal_judiciary: Primary beneficiary (institutional/identity_locked) — professional identity fused to hierarchy
 *   - constitutional_law_elite: Beneficiary (organized/constrained) — professional capital depends on monopoly
 *   - congress: Primary payer (institutional/constrained) — legislative acts subject to unchallengeable veto
 *   - state_legislatures: Payer (organized/constrained) — no amendment pathway, only Court composition change
 *   - executive_branch_agencies: Payer (institutional/constrained) — regulatory capacity constrained by review
 *   - the_people: Excluded (powerless/trapped) — electoral choices reversible by unelected judges
 *   - legal_scholars_critics: Observer (moderate/analytical) — documents democratic deficit, no lever
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.78).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, 'f2ccedf7-4602-4367-b98b-a821628beacb').
narrative_ontology:cs_kernel_codification('f2ccedf7-4602-4367-b98b-a821628beacb', formalized).
narrative_ontology:cs_authority_grounding('f2ccedf7-4602-4367-b98b-a821628beacb', lineage).
narrative_ontology:cs_interpretation_layer_present('f2ccedf7-4602-4367-b98b-a821628beacb').
narrative_ontology:cs_reading_relation('f2ccedf7-4602-4367-b98b-a821628beacb', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('f2ccedf7-4602-4367-b98b-a821628beacb', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('f2ccedf7-4602-4367-b98b-a821628beacb', foundational, judicial_finality_unchallengeable).
narrative_ontology:cs_axiom_status(judicial_finality_unchallengeable, holdable).
narrative_ontology:cs_axiom_grounding('f2ccedf7-4602-4367-b98b-a821628beacb', judicial_finality_unchallengeable, conventional).
narrative_ontology:cs_axiom('f2ccedf7-4602-4367-b98b-a821628beacb', foundational, counter_majoritarian_veto_legitimate).
narrative_ontology:cs_axiom_status(counter_majoritarian_veto_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f2ccedf7-4602-4367-b98b-a821628beacb', counter_majoritarian_veto_legitimate, deontological).
narrative_ontology:cs_reference_frame('f2ccedf7-4602-4367-b98b-a821628beacb', marbury_madison_settlement).
narrative_ontology:cs_drift_state('f2ccedf7-4602-4367-b98b-a821628beacb', contemporary_originalist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2ccedf7-4602-4367-b98b-a821628beacb', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_law_elite).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, congress).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, state_legislatures).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch_agencies).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, counter_majoritarian_veto).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold final, unchallengeable authority to invalidate legislative and executive acts. Their interpretive decisions are binding on all other actors with no constitutional remedy. Collect institutional legitimacy and career-defining authority from this monopoly; their self-conception is fused with the role of constitutional guardian.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, supreme_court_justices, agenda_setter,
    institutional, generational, identity_locked, national).

% Lower court judges operate within the interpretive framework set by the Supreme Court; their decisions gain binding force from the supremacy structure. They benefit from the institutional prestige and career stability the hierarchy provides, while their professional identity is constituted through participation in this interpretive chain.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, federal_judiciary, beneficiary,
    institutional, generational, identity_locked, national).

% Academics, practitioners, and clerks whose professional capital depends on the Supreme Court's interpretive monopoly. They build careers interpreting, predicting, and litigating within the Court's framework. Exit means abandoning the entire professional ecosystem built around judicial supremacy.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_law_elite, beneficiary,
    organized, biographical, constrained, national).

% Legislative acts can be invalidated by judicial review with no constitutional override mechanism short of amendment (prohibitively difficult) or Court self-reversal (unpredictable). Policy space is constrained by the need to anticipate judicial veto; legislative majorities bear the cost of bills struck down after investment.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, congress, payer,
    institutional, biographical, constrained, national).

% State laws are subject to federal judicial review under the same supremacy doctrine. They lack even the federal legislature's amendment pathway; their constrained exit is limited to interstate compacts or seeking Court composition change over decades.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, state_legislatures, payer,
    organized, biographical, constrained, national).

% Administrative actions and regulations face judicial invalidation under Chevron deference erosion and major questions doctrine. Agencies must self-censor policy ambitions to survive review; the cost is foregone regulatory capacity and compliance overhead.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch_agencies, payer,
    institutional, biographical, constrained, national).

% Popular majorities see their electoral choices reversed by unelected judges with life tenure. No direct exit from the constitutional structure; the only structural remedy (amendment) requires supermajorities the same Court can shape. Their voice is filtered through institutions the constraint empowers.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, the_people, excluded,
    powerless, biographical, trapped, national).

% Analyze the constraint from outside its operational machinery. They document the democratic deficit, track counter-majoritarian outcomes, and propose alternatives (departmentalism, popular constitutionalism) but hold no institutional lever to change the structure.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legal_scholars_critics, observer,
    moderate, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative settlement of constitutional meaning, preventing infinite regress of interpretive disputes and giving legal order a stable reference point. Solves the coordination problem of 'who decides finally' when branches conflict on constitutional text.
% TRANSFER_FUNCTION: Moves final interpretive authority — and with it the power to nullify legislative and executive action — from the elected branches to the unelected judiciary. The transfer is the counter-majoritarian veto: policy preferences of temporary majorities are blocked by the Court's reading of the Constitution.
% ABSENT_VOICES: The people as a collective constituent power — they would object to the permanent removal of constitutional questions from democratic contestation, but they are not a seated institution in this arrangement. Also absent: state constitutional conventions, which the supremacy doctrine displaced as final arbiters of federal constitutional meaning.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, Congress and the President would immediately treat their own constitutional interpretations as binding. Legislation currently blocked by Court precedent (e.g., campaign finance restrictions, voting rights enforcement, abortion access protections) would advance. State legislatures would enact policies currently preempted by federal judicial review. The entire architecture of constitutional litigation would collapse and reorganize around legislative-executive negotiation.
% FOUNDING_PROBLEM: The founding problem was the perceived need for a neutral arbiter to prevent legislative tyranny and ensure uniform constitutional meaning across states — the Anti-Federalist fear of state courts interpreting the Constitution differently, and the Federalist fear of congressional overreach unchecked by any institution.
% FOUNDING_PROBLEM_CORROBORATION: Federalist No. 78 (Hamilton) attests the founding problem as live: courts as 'least dangerous branch' checking legislative excess. The Anti-Federalist papers (Brutus) attest it as dead: they predicted judicial supremacy would become the very tyranny it was meant to prevent. Modern originalists (e.g., Scalia, Barnett) argue the founding problem was judicial review itself, not supremacy — departmentalism was the original design. Living constitutionalists (e.g., Tribe, Balkin) argue the problem evolved and supremacy is the adaptation. No corroboration outside the benefiting parties (the judiciary and its professional ecosystem) supports the claim that the founding problem requires *unchallengeable* finality rather than coordinated interpretation.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.65) is high because the constraint transfers final interpretive authority — a form of political power — from accountable branches to unaccountable judges, and the transfer is sustained by the Court's own decisions (Marbury, Cooper v. Aaron, City of Boerne). Suppression (0.78) is higher still because the constraint's persistence depends on actively foreclosing legislative overrides (departmentalism, jurisdiction stripping, Court-packing threats) and constitutional amendments (Article V's supermajority design). Theater ratio (0.22) is moderate: the Court performs genuine legal reasoning, but an increasing share of decisions (shadow docket, major questions doctrine, non-delegation revival) serve the institutional interest in maintaining the monopoly rather than the coordination function. Accessibility collapse (0.68) is substantial but not total — departmentalism and popular constitutionalism persist as live alternatives. Resistance (0.55) is significant: Congress has periodically challenged (Reconstruction, New Deal, Warren Court backlash), but the constraint has absorbed each challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's seat (agenda_setter, identity_locked), the constraint appears as genuine coordination — the Court *is* the constitutional settlement mechanism. From Congress's seat (payer, constrained), the same structure operates as enforced extraction — legislative majorities invest in bills the Court can nullify without accountability. From the people's seat (excluded, trapped), the constraint is a snare — their democratic choices are reversed by a structure they cannot exit. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary (justices + lower courts) are structural beneficiaries: they collect the interpretive monopoly rents (institutional authority, professional prestige, career-defining power) and their exit is identity_locked — their professional self-conception is constituted by the role. Congress, state legislatures, and agencies are structural victims: they bear the costs of constrained policy space, invalidated legislation, and compliance overhead. Their exit is constrained — they can seek Court composition change over decades or pursue near-impossible amendments. The people are excluded: they would object but hold no institutional seat. The constitutional law elite are beneficiaries with constrained exit — their professional ecosystem depends on the monopoly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for neutral arbiter to prevent legislative tyranny and ensure uniform meaning) is contested: originalists argue departmentalism was the design; living constitutionalists argue supremacy is the adaptation. The constraint persists not because the founding problem is live in its original form, but because the benefiting parties (judiciary + professional ecosystem) have identity_locked exit and institutional power to suppress alternatives. This is mandatrophy: the coordination function (final settlement) has been captured by the extraction function (unchallengeable veto), and the arrangement persists because the cost to fix (Article V amendment, Court-packing, jurisdiction stripping) exceeds what any payer can bear, while beneficiaries are identity-locked into maintaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_vs_coordinate_construction,
    'Does the judicial supremacy reading''s core premise (unchallengeable judicial finality) logically foreclose the coordinate construction reading''s core premise (distributed interpretive authority with no single final arbiter) within any single constitutional framework?',
    'Analyze whether a constitutional system can simultaneously hold that courts have final, binding authority on constitutional questions AND that other branches have equal interpretive authority. Historical test: early republic departmentalism (Jefferson, Jackson) treated Court opinions as non-binding on other branches — was this a coherent framework or a transitional instability?',
    'If forecloses: the two readings cannot coexist in one framework; the kernel''s structural tension is binary. If coexists_with: the kernel supports genuine pluralism where different institutional actors hold different readings simultaneously. Determines cs_structure.reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_coordinate_construction, conceptual, 'Whether judicial supremacy and coordinate construction are logically incompatible within one framework.').

omega_variable(
    counter_majoritarian_extraction_measurement,
    'How much of the measured extractiveness (ε=0.65) is attributable to the counter-majoritarian veto power itself versus the coordination function''s genuine cost?',
    'Compare policy outcomes under judicial supremacy vs. coordinate construction in matched domains (e.g., campaign finance, voting rights, abortion). If outcomes diverge substantially and the Court''s reading consistently blocks majority preferences, the delta measures extractive veto. If outcomes converge, the coordination function may dominate.',
    'If extraction is mostly veto power: the tangled_rope classification is strongly supported — genuine coordination with heavy asymmetric extraction. If extraction is mostly coordination cost: the constraint may be closer to rope with high Boltzmann floor. Affects ε attribution and mandate drift analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_extraction_measurement, empirical, 'Disentangling coordination cost from extractive veto in the measured ε.').

omega_variable(
    identity_lock_mechanism_judiciary,
    'What specific identity-fusion mechanism binds judges to the judicial supremacy reading — professional identity (career path dependence), institutional identity (the Court has ''become'' its function), or ideological identity (worldview making departmentalism unthinkable)?',
    'Track judicial behavior when the Court''s institutional interest conflicts with individual justices'' ideological preferences (e.g., Court-packing threats, legitimacy crises). If justices defend the institution against their own ideological allies, institutional identity dominates. If they defect, professional/ideological identity dominates.',
    'If institutional identity: the constraint is a piton candidate — the Court maintains the structure theatrically because it has ''become'' the function. If professional identity: exit is career-suicide but theoretically possible. If ideological: the reading is held as truth, not interest — harder to dislodge but also more brittle if the ideology fractures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_judiciary, conceptual, 'The identity-lock mechanism binding judges to the supremacy reading.').

omega_variable(
    kernel_reading_structural_delta,
    'Does this reading''s structural delta (judiciary as beneficiary, legislature as victim, high ε, foreclosure of legislative override) accurately capture the irreducible difference from sibling readings, or does it overstate foreclosure where influence or coexistence is the true relation?',
    'Compare the operational behavior of actors under each reading. In coordinate construction systems (early US, some states), do legislatures actually exercise interpretive authority or do they defer? In parliamentary supremacy systems (UK, NZ), does the legislature actually exercise final constitutional authority or do courts achieve de facto supremacy? The behavioral delta reveals the structural delta.',
    'If the delta is overstated: the reading_relations may need adjustment (forecloses → influences or coexists_with). If accurate: the kernel''s contest is genuinely structural, not merely rhetorical. Affects cs_structure.reading_relations and the kernel''s contamination network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, empirical, 'Validating the declared structural delta against sibling readings'' operational behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1789, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(cons_tr_t1803, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1803, 0.08).
narrative_ontology:measurement(cons_tr_t1857, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1857, 0.12).
narrative_ontology:measurement(cons_tr_t1905, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1905, 0.15).
narrative_ontology:measurement(cons_tr_t1937, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(cons_tr_t1954, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(cons_tr_t1973, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(cons_tr_t2000, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(cons_tr_t2022, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(cons_tr_t2024, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t1789, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(cons_be_t1803, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1803, 0.25).
narrative_ontology:measurement(cons_be_t1857, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1857, 0.35).
narrative_ontology:measurement(cons_be_t1905, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1905, 0.45).
narrative_ontology:measurement(cons_be_t1937, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1937, 0.4).
narrative_ontology:measurement(cons_be_t1954, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1954, 0.5).
narrative_ontology:measurement(cons_be_t1973, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1973, 0.58).
narrative_ontology:measurement(cons_be_t2000, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(cons_be_t2022, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 2022, 0.65).
narrative_ontology:measurement(cons_be_t2024, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1789, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1789, 0.3).
narrative_ontology:measurement(cons_su_t1803, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1803, 0.45).
narrative_ontology:measurement(cons_su_t1857, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1857, 0.6).
narrative_ontology:measurement(cons_su_t1905, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1905, 0.7).
narrative_ontology:measurement(cons_su_t1937, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1937, 0.55).
narrative_ontology:measurement(cons_su_t1954, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1954, 0.65).
narrative_ontology:measurement(cons_su_t1973, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1973, 0.72).
narrative_ontology:measurement(cons_su_t2000, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(cons_su_t2022, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(cons_su_t2024, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, parliamentary_primacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, voting_rights_act_enforcement).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, campaign_finance_regulation).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, abortion_access_jurisprudence).

% DUAL FORMULATION NOTE:
% This constraint is the judicial_supremacy_reading of the constitutional_authority_boundary kernel. The coordinate_construction_reading and parliamentary_primacy_reading are sibling constraints from the same kernel. The ε values differ substantially: judicial_supremacy_reading ε≈0.65 (high extraction via counter-majoritarian veto), coordinate_construction_reading ε≈0.15 (distributed authority, low extraction), parliamentary_primacy_reading ε≈0.10 (legislative finality, minimal judicial extraction). The decomposition follows the ε-invariance principle: each reading instantiates a different constraint with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
