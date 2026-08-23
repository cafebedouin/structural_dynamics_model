% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Fourteenth Amendment Equal Protection — Anti-Caste Reading
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the anti-caste reading of the Fourteenth
 *   Amendment's Equal Protection guarantee: the claim that the Constitution
 *   obligates the state to act affirmatively against racial, gender, and
 *   status hierarchy rather than merely abstaining from explicit
 *   classification. The constraint's concrete operation is the corrective
 *   apparatus built from 1954 onward — school desegregation orders,
 *   contracting set-asides, race-conscious admissions, majority-minority
 *   districting, disparate-impact liability — together with the narrower
 *   residue that survives the post-1990 retrenchment. The story is a member
 *   of a constraint family decomposed per the epsilon-invariance principle:
 *   the colloquial label 'equal protection' conflates two structurally
 *   distinct constraints. This story's epsilon attaches to the
 *   corrective-transfer arrangement (the remedial apparatus this reading
 *   built and partially lost); the sibling story,
 *   fourteenth_amendment_equal_protection__formal_equality_reading, attaches
 *   its epsilon to the classification-prohibition arrangement. The two files
 *   are linked through network.affects_constraints and must never pool their
 *   seat data. Claim/metric independence is preserved deliberately: the
 *   claimed type (tangled_rope) reflects the structure I believe true — a
 *   genuine coordination function fused with asymmetric transfer under active
 *   enforcement — while the metrics describe the constraint's actual current
 *   state: extraction persisting at 0.52 amid rollback, enforcement decayed
 *   to 0.30 from a 0.72 peak, and theater risen to 0.58 as statements,
 *   training, and reporting substitute for material redistribution. Any
 *   divergence between the claim and the engine's computed per-seat types is
 *   the datum the corpus exists to collect. KEY AGENTS (by structural
 *   relationship): - subordinated_racial_communities: primary beneficiary
 *   (organized/constrained) — receives access, remedy, and enforcement
 *   priority - women_in_male_dominated_fields: beneficiary
 *   (organized/constrained) - asian_american_admission_applicants: primary
 *   target (moderate/constrained) — bears preference displacement -
 *   white_working_class_preference_bearers: target (moderate/constrained) —
 *   bears marginal displacement, supplies rollback base -
 *   non_certified_prime_contractors: target (powerful/arbitrage) — loses bid
 *   eligibility, strongest payer exit -
 *   civil_rights_litigation_organizations: beneficiary (organized/mobile) —
 *   collects docket, funding, standing - diversity_compliance_professionals:
 *   beneficiary (moderate/mobile) — collects budgets scaled to mandate
 *   breadth - united_states_supreme_court: agenda_setter
 *   (institutional/constrained) — sets the constraint's scope -
 *   federal_civil_rights_enforcement_agencies: agenda_setter/beneficiary
 *   (institutional/constrained) — administers enforcement -
 *   subordinated_group_members_beyond_remedy_scope: excluded
 *   (powerless/trapped) — harmed but unreachable by the remedy -
 *   comparative_constitutional_analysts: analytical observer — sees the full
 *   structure against peer constitutions
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.52).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.3).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Fourteenth Amendment Equal Protection — Anti-Caste Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, 'd568f8e1-c350-4d03-918e-af49ffd9608d').
narrative_ontology:cs_kernel_codification('d568f8e1-c350-4d03-918e-af49ffd9608d', fixed_text).
narrative_ontology:cs_authority_grounding('d568f8e1-c350-4d03-918e-af49ffd9608d', lineage).
narrative_ontology:cs_interpretation_layer_present('d568f8e1-c350-4d03-918e-af49ffd9608d').
narrative_ontology:cs_reading_relation('d568f8e1-c350-4d03-918e-af49ffd9608d', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('d568f8e1-c350-4d03-918e-af49ffd9608d', foundational, guarantee_requires_dismantling_inherited_caste).
narrative_ontology:cs_axiom_status(guarantee_requires_dismantling_inherited_caste, holdable).
narrative_ontology:cs_axiom_grounding('d568f8e1-c350-4d03-918e-af49ffd9608d', guarantee_requires_dismantling_inherited_caste, deontological).
narrative_ontology:cs_axiom('d568f8e1-c350-4d03-918e-af49ffd9608d', secondary, classification_neutrality_entrenches_hierarchy).
narrative_ontology:cs_axiom_status(classification_neutrality_entrenches_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('d568f8e1-c350-4d03-918e-af49ffd9608d', classification_neutrality_entrenches_hierarchy, empirically_contingent).
narrative_ontology:cs_reference_frame('d568f8e1-c350-4d03-918e-af49ffd9608d', reconstruction_charter_of_caste_abolition).
narrative_ontology:cs_drift_state('d568f8e1-c350-4d03-918e-af49ffd9608d', post_sffa_doctrinal_repudiation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('d568f8e1-c350-4d03-918e-af49ffd9608d', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_racial_communities).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, women_in_male_dominated_fields).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_litigation_organizations).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, diversity_compliance_professionals).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, asian_american_admission_applicants).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, white_working_class_preference_bearers).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, non_certified_prime_contractors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, federal_civil_rights_enforcement_agencies).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, anti_subordination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Black, Latino, and Native American communities hold the intergenerational position the constraint addresses. They receive expanded access to selective universities, contracting set-asides, majority-minority districting, and enforcement priority. They cannot exit the polity or the racial order their position is constituted through; their leverage is organizational — churches, unions, advocacy networks — and their claims are framed across generations.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_racial_communities, beneficiary,
    organized, generational, constrained, national).

% Receive targeted recruitment, fellowship set-asides, and Title IX enforcement that opened construction trades, faculty lines, and athletic infrastructure. Costs arrive as tokenism burdens and as backlash politics that periodically strips the programs. Exiting into unaffected sectors is possible but expensive for careers built inside the affected institutions.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, women_in_male_dominated_fields, beneficiary,
    organized, biographical, constrained, national).

% Sit on the disfavored side of holistic-review preferences at selective institutions: race-conscious admissions hold their admit rates below what test-and-grade baselines predict. Recourse is litigation, private institutions, or study abroad; for public flagship pipelines in their home states there is little room to route around the classification.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, asian_american_admission_applicants, payer,
    moderate, biographical, constrained, national).

% Bear displacement at the margin of admissions, hiring, and promotion queues wherever preferences bind, typically without elite-network fallbacks. They supply the electoral base for initiative bans such as California's Proposition 209 and Michigan's Proposal 2 and for the wider rollback coalition; exit is geographic relocation or disengagement from selective pipelines.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, white_working_class_preference_bearers, payer,
    moderate, biographical, constrained, national).

% Large non-certified firms lose bid eligibility under municipal and state set-aside percentages or must subcontract through certified partners at a margin cost. They hold the strongest exit position among the payer seats: jurisdiction shopping, joint ventures, and lobbying for repeal.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, non_certified_prime_contractors, payer,
    powerful, biographical, arbitrage, national).

% Staff the offices the mandate creates: chief diversity officers, Title IX coordinators, equity consultants, report writers. Salaries, headcount, and consulting markets scale with the mandate's breadth regardless of whether material transfer follows; their skills port to adjacent HR and compliance work if the mandate shrinks.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, diversity_compliance_professionals, beneficiary,
    moderate, biographical, mobile, national).

% Movement-law firms and advocacy groups whose dockets, funding, and standing depend on the constraint's enforcement frontier. They litigate in both directions — defending programs where attacked, pressing new fronts where dormant — and can shift portfolios to voting rights or speech cases if the mandate collapses.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_litigation_organizations, beneficiary,
    organized, generational, mobile, national).

% Interprets the Amendment's guarantee and thereby sets the constraint's scope case by case: from Green and Bakke through Croson, Adarand, Grutter, and SFFA it has alternately expanded and contracted the remedial reading. It cannot exit the interpretive role; its degrees of freedom are precedent, text, and appointment politics.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, united_states_supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% DOJ's Civil Rights Division, the EEOC, and the Education Department's OCR administer investigations, consent decrees, and funding leverage. Enforcement intensity is simultaneously their budget and their mission; retrenchment strands career staff whose expertise is the enforcement machinery itself.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, federal_civil_rights_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, federal_civil_rights_enforcement_agencies, beneficiary).

% Face hierarchy in domains the constraint does not reach — wealth transmission, algorithmic screening, platform moderation, non-covered employers — and lack the standing, counsel, or category fit that brings a claim inside the apparatus. They watch remedy flow to the credentialed slice of their communities.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_group_members_beyond_remedy_scope, excluded,
    powerless, generational, trapped, national).

% Study the Amendment alongside India's reservation system, South Africa's restitution clauses, and Germany's Basic Law social-state provisions. They hold no stakes in the American dispute and observe which designs dismantle inherited hierarchy durably and which decay into administration.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, comparative_constitutional_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, diversity_compliance_professionals).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a society-wide response to a problem individual action cannot solve: inherited racial, gender, and status hierarchies reproduce themselves through apparently neutral transactions, so uncoordinated institutions each reproduce caste while no single actor can stop it alone. The constraint pools corrective effort, standardizes remedy across jurisdictions, and commits successive officeholders to a shared dismantling project.
% TRANSFER_FUNCTION: Moves positional goods (admissions places, contracts, promotions, district boundaries), public expenditure, and enforcement attention from historically advantaged applicants, firms, and institutions toward members of groups subordinated by those hierarchies; secondarily, it moves discretion over classification from local actors to courts and federal agencies.
% ABSENT_VOICES: Members of subordinated groups whose harms fall outside the remedy's scope — wealth gaps, algorithmic and private-platform discrimination, underfunded schools outside desegregation orders — would object that the apparatus under-reaches. Asian-American advocates spent decades unheard inside civil-rights coalitions before SFFA gave their seat standing. Future generations inherit the outcome without a vote.
% DISAPPEARANCE_RATIONALE: Overnight removal dissolves surviving set-asides, consent decrees, and disparate-impact exposure; selective institutions revert immediately to facially neutral criteria; the compliance and litigation apparatus built on the mandate collapses; distributional flows reorganize around formal-equality administration within roughly a decade. The arrangement's absence visibly rearranges who gets what.
% FOUNDING_PROBLEM: Hereditary caste enforced through law: the Reconstruction framers built the Amendment to arm Congress and the courts against Black Codes, racial terror, and the legal reproduction of slavery's status line — a problem defined as caste itself, not merely unequal process.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Reconstruction historiography (the academic seat) attests the caste-dismantling genealogy; sitting justices across both wings attest the genealogy in their opinions while disputing present status — the SFFA majority concedes the Amendment's protective origin and denies its extension. No attestation of the problem's liveness comes from a seat wholly outside the dispute, which is itself the signal that the status is contested rather than dead.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the constraint mandates real asymmetric transfers of positional goods, and extraction has proven sticky — as direct remedies were struck down (Croson, Adarand, Prop 209, SFFA), residual mandates, consent-decree administration, compliance burdens, and litigation kept the transfer load from falling proportionally, producing a plateau rather than a proportional decline. Suppression 0.30: suppression is authored as a raw structural property and is never scaled by power or scope; the current constraint suppresses the colorblind-administration alternative only weakly, having lost the enforcement capacity (peak 0.72 during busing-era compulsion) that once made the alternative unavailable. Accessibility collapse 0.40: the formal-equality alternative never collapsed — it stayed legally articulable throughout, won initiative battles in multiple states, and ultimately captured the Supreme Court, which is exactly why this is not a natural-law profile. Resistance 0.70: massive resistance, state initiatives, and organized litigation represent sustained, partially successful opposition. Theater 0.58 and rising monotonically across the shared eight-point grid is the story's sharpest signal: Goodhart drift from remedy toward performance (diversity statements, mandatory training, reporting bureaucracies) as material channels closed. The trajectory is arc-shaped, not cyclical — mobilization, peak, retrenchment, residual — so no intermittent-reinforcement mechanism is alleged. All three tracked series run on one shared time grid (points 0 through 70 in decade steps), so no metric row borrows an end-state value from another.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent experiences from identical structural data. From the payer seats, the constraint arrives as uncompensated displacement administered by distant institutions — a structure experienced as pure burden, since the coordination benefit is invisible from inside a rejected application. From the beneficiary seats, the same apparatus is experienced as remedy finally commensurate to the injury. From the agenda-setter seat (the Court), the constraint is a maintenance obligation — a text to be interpreted, with each retrenchment experienced as fidelity rather than extraction. The compliance and litigation professions experience the constraint as livelihood. None of these perceptions is authored anywhere in the story; the engine derives each seat's classification from power, exit options, directionality, and role, and the divergence among them is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place subordinated_racial_communities, women_in_male_dominated_fields, civil_rights_litigation_organizations, and diversity_compliance_professionals near the subsidy end of the directionality range — the constraint's operation routes goods toward them and they bear little of its cost. Victim declarations place asian_american_admission_applicants, white_working_class_preference_bearers, and non_certified_prime_contractors near the full-target end; exit modulation refines this — the contractors' arbitrage-grade exit pulls them back toward symmetric, the trapped and constrained applicant seats sit nearer full target. The agenda-setter seats sit mid-range with damping, since the Court and agencies both wield and obey the constraint. No directionality_overrides are authored: the derivation from beneficiary/victim data plus power and exit produces the correct relationships, and the one suspected distortion — whether compliance professionals truly benefit or merely intermediate — is routed through the material_transfer_vs_intermediary_capture omega rather than a metric override, because it is an open empirical question, not a known structural fact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (hereditary caste enforced through law) is authored as contested, not dead: proponents point to persistent wealth, segregation, and representation disparities; opponents hold the original wrongs remediated and race-consciousness itself now the injury. Because status is contested rather than dead, the constraint's mandate has not demonstrably outlived its function and no mandatrophy-resolution flag is warranted. The mismatch consumer reads founding_problem_status x disappearance_verdict: contested x world_rearranges produces no zombie flag, correctly. The danger this story flags instead is forward-looking: with theater_ratio crossing 0.5 and enforcement capacity decayed, the apparatus risks drifting toward a piton profile — administrators maintaining performance after material function atrophies — if the residual extraction continues to accrue to intermediaries rather than to the subordinated communities the constraint names. The classification prevents mislabeling in both directions: reading the constraint as pure coordination ignores the documented displacement of identifiable payers; reading it as pure extraction erases the genuine collective-action problem (self-reproducing hierarchy) that individual actors provably cannot solve alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'How does instantiating the formal_equality_reading instead of this anti-caste reading change the constraint''s structure?',
    'Compare against the sibling story fourteenth_amendment_equal_protection__formal_equality_reading: its beneficiary and victim sets invert (remedial-program participants become the protected class and classification targets become its enforcers), its epsilon relocates onto race-conscious state action itself, and its claimed type shifts toward restraint-enforcement. The comparison is the resolution.',
    'Classifying the same constitutional text flips from a corrective-transfer constraint to a classification-suppression constraint; seat-divergence and contamination analyses must run per reading and never pool the two files'' structural data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame record: this file is one reading of the equal-protection kernel; the sibling reading emits a structurally different constraint with a different epsilon referent.').

omega_variable(
    transitional_mandate_vs_standing_vigilance,
    'Is the corrective mandate transitional — designed to sunset when hierarchy closes — or standing vigilance against continuously regenerated hierarchy?',
    'Long-run disparity trajectories combined with doctrinal handling of sunset expectations (Croson''s aspiration to end set-asides within a generation, Grutter''s twenty-five-year diversity expectation, SFFA''s refusal to extend one): if parity plateaus and holds without the mandate, scaffold dynamics obtain; if hierarchy measurably regenerates on relaxation, the mandate is standing.',
    'A transitional verdict would push the constraint toward scaffold typing with a declared sunset clause and re-date its justification window; a standing verdict stabilizes tangled_rope classification with indefinite enforcement obligations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_mandate_vs_standing_vigilance, conceptual, 'Whether the anti-caste mandate carries an implicit sunset or asserts permanent structural vigilance.').

omega_variable(
    material_transfer_vs_intermediary_capture,
    'Do remedial gains land materially with subordinated communities, or are they progressively captured by the administrative and litigation professions that run the mandate?',
    'Flow audits: net admissions shifts net of legacy and athletic preferences, set-aside contract award concentration, and growth of diversity-administration payroll against community wealth and mobility deltas.',
    'Dominant material transfer supports the tangled-rope coordination claim and the current receipt attribution; dominant capture pushes the constraint toward snare or piton dynamics, would justify re-attributing gain_flow, and would date the drift to the theater-ratio crossover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_transfer_vs_intermediary_capture, empirical, 'Where the constraint''s transfers actually accrue: beneficiary communities or intermediary professions.').

omega_variable(
    counterfactual_baseline_dispute,
    'Which counterfactual baseline fixes the magnitude of the constraint''s transfers — merit-neutral selection, legacy-and-wealth-blind selection, or full structural correction?',
    'Quasi-experiments across jurisdictions and eras with differing remedial intensity (initiative-ban states versus matched peers) estimating distributional deltas under each candidate baseline.',
    'Magnitude estimates for extractiveness vary with the baseline chosen; the constraint''s identity, beneficiary structure, and victim structure do not. This omega bounds measurement error on epsilon — it does not license observable-dependent relabeling, which would require decomposition into separate stories instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_baseline_dispute, empirical, 'Baseline dependence of the extraction magnitude estimate over a fixed referent.').

omega_variable(
    payer_coalition_durability,
    'Can the displaced-payer seats — disfavored applicants, excluded contractors, preference-bearing workers — sustain the cross-group coalition that produced SFFA, or does it dissolve after the admissions victory?',
    'Track post-SFFA litigation and initiative activity across contracting, employment, and K-12 assignment domains; durable multi-front coordination signals coalition power converting into further rollback.',
    'A durable coalition accelerates enforcement decay below the modeled trajectory and raises effective resistance; dissolution leaves residual mandates intact at approximately current extraction levels with theater continuing to rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payer_coalition_durability, empirical, 'Durability of the payer-side coalition that drove the recent retrenchment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(four_tr_t0, observed).
narrative_ontology:measurement(four_tr_t10, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(four_tr_t10, observed).
narrative_ontology:measurement(four_tr_t20, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(four_tr_t20, observed).
narrative_ontology:measurement(four_tr_t30, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(four_tr_t30, observed).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(four_tr_t40, observed).
narrative_ontology:measurement(four_tr_t50, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement_basis(four_tr_t50, observed).
narrative_ontology:measurement(four_tr_t60, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement_basis(four_tr_t60, observed).
narrative_ontology:measurement(four_tr_t70, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 70, 0.58).
narrative_ontology:measurement_basis(four_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(four_be_t0, observed).
narrative_ontology:measurement(four_be_t10, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(four_be_t10, observed).
narrative_ontology:measurement(four_be_t20, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(four_be_t20, observed).
narrative_ontology:measurement(four_be_t30, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(four_be_t30, observed).
narrative_ontology:measurement(four_be_t40, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(four_be_t40, observed).
narrative_ontology:measurement(four_be_t50, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement_basis(four_be_t50, observed).
narrative_ontology:measurement(four_be_t60, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(four_be_t60, observed).
narrative_ontology:measurement(four_be_t70, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 70, 0.52).
narrative_ontology:measurement_basis(four_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(four_su_t0, observed).
narrative_ontology:measurement(four_su_t10, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(four_su_t10, observed).
narrative_ontology:measurement(four_su_t20, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(four_su_t20, observed).
narrative_ontology:measurement(four_su_t30, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(four_su_t30, observed).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(four_su_t40, observed).
narrative_ontology:measurement(four_su_t50, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement_basis(four_su_t50, observed).
narrative_ontology:measurement(four_su_t60, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement_basis(four_su_t60, observed).
narrative_ontology:measurement(four_su_t70, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 70, 0.3).
narrative_ontology:measurement_basis(four_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, resource_allocation).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial concept 'equal protection' resolves into two structurally distinct constraints emitted from one kernel text. This story (anti_caste_reading) carries epsilon for the corrective-transfer arrangement — subordinated groups in the beneficiary set, displaced applicants and contractors in the victim set, remedial programs carrying substantial extraction. The sibling (formal_equality_reading) carries epsilon for the classification-prohibition arrangement, with inverted seat structure. Upstream/downstream: neither reading is epistemically prior; they are rival instantiations held by opposing factions, linked here so contamination and seat analyses propagate across the family without merging their referents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
