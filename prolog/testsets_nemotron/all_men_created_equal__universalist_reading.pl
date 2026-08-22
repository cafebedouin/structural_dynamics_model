% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Equality as Universal Principle Requiring Iterative Expansion
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   The universalist reading of 'all men are created equal' treats the
 *   Declaration's equality clause as a generative principle whose scope must
 *   expand iteratively to include those originally excluded — enslaved
 *   people, women, propertyless men, racial minorities, LGBTQ+ persons —
 *   regardless of the founders' concrete intentions or 18th-century social
 *   taxonomy. This reading animates Reconstruction, the Civil Rights
 *   Movement, and contemporary equal protection jurisprudence. It functions
 *   as a tangled rope: it solves a genuine coordination problem (how to
 *   legitimate a political order that claims universal equality while
 *   practicing hierarchy) while extracting coordination costs from
 *   exclusionary power structures that must surrender privileges. The
 *   constraint requires active enforcement (judicial review, legislative
 *   action, social movements) to expand inclusion against resistance. Theater
 *   ratio declined from founding (high — the principle was largely
 *   performative at inception) toward mid-20th century (lower — genuine
 *   expansion occurred) with recent uptick as formal equality is declared
 *   achieved while structural disparities persist.
 *
 * KEY AGENTS:
 *   - marginalized_groups_claiming_inclusion: Primary beneficiary (organized/identity_locked) — gains standing and rights through iterative expansion
 *   - exclusionary_power_structures: Primary target/victim (institutional/powerful) — bears costs of surrendering privileges and restructuring institutions
 *   - civil_rights_institutions: Agenda setter/beneficiary (institutional/generational) — administers expansion through courts, legislation, enforcement
 *   - originalist_institutional_interests: Payer/victim (organized/powerful) — resists expansion as judicial overreach beyond founder intent
 *   - expanded_citizenship_subjects: Beneficiary (moderate/constrained) — those newly included at each expansion phase
 *   - status_quo_beneficiaries_of_restricted_equality: Victim (powerful/constrained) — those who benefit from restricted equality (historically: slaveholders, segregationists, patriarchal structures)
 *   - analytical_observers: Observer (analytical/analytical) — scholars, jurists, philosophers tracking the expansion dynamic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.42).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.38).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Equality as Universal Principle Requiring Iterative Expansion").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '1b2b1d1c-c009-48a0-83bf-64d8115a9db3').
narrative_ontology:cs_kernel_codification('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', fixed_text).
narrative_ontology:cs_authority_grounding('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', lineage).
narrative_ontology:cs_interpretation_layer_present('1b2b1d1c-c009-48a0-83bf-64d8115a9db3').
narrative_ontology:cs_reading_relation('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', foundational, equality_principle_generative_not_descriptive).
narrative_ontology:cs_axiom_status(equality_principle_generative_not_descriptive, holdable).
narrative_ontology:cs_axiom_grounding('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', equality_principle_generative_not_descriptive, deontological).
narrative_ontology:cs_axiom('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', foundational, founder_intent_not_binding_on_scope).
narrative_ontology:cs_axiom_status(founder_intent_not_binding_on_scope, holdable).
narrative_ontology:cs_axiom_grounding('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', founder_intent_not_binding_on_scope, conventional).
narrative_ontology:cs_reference_frame('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', founding_declaration_universal_principle).
narrative_ontology:cs_drift_state('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', contemporary_equal_protection_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1b2b1d1c-c009-48a0-83bf-64d8115a9db3', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, expanded_citizenship_subjects).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, civil_rights_institutions).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, exclusionary_power_structures).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, originalist_institutional_interests).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, status_quo_beneficiaries_of_restricted_equality).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized groups (Black Americans, women, LGBTQ+, immigrants, disabled persons) who claim inclusion under the universalist principle. They gain rights, standing, and material benefits through each expansion phase. They bear coordination costs of mobilization, litigation, and political struggle. Their exit is identity_locked — they cannot cease being the subjects of exclusion without the constraint's success. They are both beneficiaries of expansion and payers of the struggle to achieve it.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, payer).

% Institutional and social structures that maintain hierarchy through restricted equality (historically: slaveholding class, Jim Crow regimes, patriarchal legal structures, nativist immigration regimes). They bear the costs of surrendered privileges, institutional restructuring, and redistributed resources when expansion occurs. Their exit is constrained — they can resist through courts, legislation, violence, but cannot escape the polity's territorial jurisdiction. They experience the constraint as extraction from their established position.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, exclusionary_power_structures, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, exclusionary_power_structures, payer).

% Courts (especially federal judiciary), Congress, executive agencies (DOJ Civil Rights Division, EEOC), and movement organizations that administer the expansion of equality. They set the agenda for which exclusions are addressed, through what mechanisms, and on what timeline. They benefit institutionally from the expansion mission (legitimacy, jurisdiction, resources). Their exit is arbitrage-grade — they could theoretically abandon the expansion function, but their institutional identity is constituted by it.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, civil_rights_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, civil_rights_institutions, beneficiary).

% Judicial, academic, and political actors committed to originalist interpretation who resist expansion beyond founder intent. They pay through lost jurisprudential ground, political capital, and institutional legitimacy when universalist expansions prevail. Their exit is constrained — they remain embedded in the legal system they seek to constrain. They are not merely observers; they actively contest the constraint's operation through judicial appointments, academic work, and political mobilization.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_institutional_interests, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, originalist_institutional_interests, payer).

% Individuals and groups who gain inclusion at each expansion phase (formerly enslaved persons after 13th/14th Amendments, women after 19th Amendment, racial minorities after Civil Rights Act, LGBTQ+ persons after Obergefell/Bostock). They receive the constraint's benefits directly — rights, protections, standing. Their exit is constrained: they cannot exit the polity, and their inclusion creates stakes in further expansion. They are the realized beneficiaries of past expansion and the potential victims of rollback.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, expanded_citizenship_subjects, beneficiary,
    moderate, biographical, constrained, national).

% Those who materially benefit from restricted equality without necessarily wielding institutional power: employers who exploit excluded labor, landlords who discriminate, political machines that rely on restricted franchises, cultural majorities that enjoy status hierarchy. They lose advantages when equality expands. Their exit is constrained — they benefit from the polity's economic and political system but resist the specific constraint of universal equality. They are distinct from exclusionary_power_structures in lacking direct institutional authority but sharing the extraction burden.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, status_quo_beneficiaries_of_restricted_equality, payer,
    powerful, biographical, constrained, national).

% Scholars, jurists, philosophers, and comparative constitutionalists who track the expansion dynamic from outside the conflict. They neither collect nor pay; they analyze. Their analytical seat provides the reference frame for measuring the constraint's drift, theater, and classification across readings. They have arbitrage-grade exit from the constraint's operation (they can study other systems) but civilizational time horizon (they track the kernel's centuries-long trajectory).
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a legitimate political community across deep diversity by providing a generative principle that requires iterative inclusion of those previously excluded. Solves the problem: how can a polity claim universal equality while practicing hierarchy? The universalist reading answers: by treating the claim as a binding commitment to expand, not a description of the status quo.
% TRANSFER_FUNCTION: Moves status, legal standing, political rights, and material resources from exclusionary power structures and status quo beneficiaries to marginalized groups claiming inclusion. Each expansion phase transfers the privileges of restricted citizenship (voting, property, marriage, employment, dignity) from those who monopolized them to those denied them.
% ABSENT_VOICES: Those excluded from the polity entirely at each phase — enslaved persons before 1865, women before 1920, colonized peoples, future generations, non-citizen residents — whose exclusion the universalist principle condemns but whose voices were structurally absent from the expansion decisions that notionally benefited them. Also: the founders themselves (dead, cannot consent to or contest the reading of their words), and those in jurisdictions where US equality jurisprudence is exported without democratic consent.
% DISAPPEARANCE_RATIONALE: If the universalist reading vanished overnight, the legal and institutional architecture built on iterative expansion (Reconstruction Amendments as living guarantees, Civil Rights Act, Voting Rights Act, Obergefell, Bostock, EEOC enforcement, disparate impact doctrine) would lose its animating principle. Courts would revert to originalist/textualist frameworks. Marginalized groups would lose the constitutional basis for claiming inclusion beyond explicit textual enumeration. The polity would reorganize around restricted equality — the world would rearrange profoundly.
% FOUNDING_PROBLEM: The American founding proclaimed universal equality ('all men are created equal') while simultaneously entrenching slavery, gender hierarchy, property qualifications, and Indigenous dispossession. The universalist reading was built to solve the legitimating contradiction: a republic founded on equality cannot stably maintain hierarchy; the principle must expand to resolve the contradiction, or the regime loses legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: Frederick Douglass ('the Constitution is a glorious liberty document' — reading the principle against the practice), Charles Sumner and Reconstruction Congress (equality as unfinished revolutionary promise), Martin Luther King Jr. ('promissory note' — the founding problem as live debt), Ruth Bader Ginsburg (constitutional interpretation as expanding 'we the people'), and contemporary movement lawyers. The founding problem is attested as live by those outside the original beneficiary set (founders, slaveholders) — by the excluded who became includers.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).
:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate because the constraint's operation transfers status, rights, and material resources from exclusionary structures to marginalized groups — this is not zero-sum extraction but coordination-cost extraction: the work of dismantling hierarchy and building inclusive institutions. Suppression (0.38) reflects active resistance to expansion (filibusters, massive resistance, court-packing threats, originalist jurisprudence) but is not total — expansion proceeds through legitimate channels. Theater ratio (0.28) is low-moderate: early periods were highly performative (founding era, Reconstruction's retreat), but the civil rights era saw genuine structural change; recent uptick reflects 'colorblind' formalism masking persistent structural inequality. Accessibility collapse (0.45) is partial: alternatives (hierarchy, caste, restricted citizenship) remain intellectually and politically available, championed by originalist and textualist-paradox readings. Resistance (0.55) is significant: each expansion phase meets organized opposition. The claimed type (tangled_rope) captures the dual character: genuine coordination of an expanding political community + asymmetric extraction from those who lose exclusionary privilege.
 *
 * PERSPECTIVAL GAP:
 *   From the marginalized groups' seat (identity_locked, organized): the constraint is a rope becoming less extractive over time — each expansion reduces the extraction they suffer from exclusion. From exclusionary power structures' seat (institutional, powerful): the constraint is a snare — it extracts their privileges through coercive state power (courts, federal enforcement). From civil rights institutions' seat (agenda_setter): it is a scaffold with no sunset — the expansion is the permanent function, not a transition to a steady state. From originalist interests' seat: it is a piton — a degraded constraint (founder intent) maintained theatrically to resist expansion. The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups claiming inclusion are structural beneficiaries (d ≈ 0.15–0.25): they receive rights, standing, and material benefits from expansion; their exit is identity_locked (they cannot exit their identity as claimants). Exclusionary power structures are structural targets (d ≈ 0.8–0.9): they bear the costs of surrendered privilege and institutional restructuring; their exit is constrained (they can resist but cannot escape the polity). Civil rights institutions are agenda_setters with arbitrage-grade exit (d ≈ 0.1): they administer the constraint and benefit from its expansion (institutional mission, legitimacy) but could theoretically abandon it. Originalist institutional interests are payers with constrained exit (d ≈ 0.7): they pay through lost jurisprudential ground and political capital but remain embedded in the legal system. Expanded citizenship subjects are beneficiaries with constrained exit (d ≈ 0.3): they gain inclusion but remain subject to the constraint's further expansion. Status quo beneficiaries are victims with constrained exit (d ≈ 0.75): they lose exclusionary advantages but cannot easily exit the polity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating a republic proclaiming equality while practicing slavery/hierarchy) was live at founding, became dead for the original exclusionary settlement (slavery ended, formal hierarchy dismantled), but the universalist reading keeps it live by treating each new exclusion as a reiteration of the founding contradiction. The mandate has NOT atrophied — the expansion function remains the constraint's active purpose. This is not a piton: the constraint is not maintained theatrically while its function decays; its function IS the expansion, and that function remains live. The coordination (legitimate political community) and extraction (from exclusionary privilege) remain structurally coupled — neither has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermination,
    'Does the universalist reading''s claim that equality requires iterative expansion represent the only defensible framing of the kernel, or does the originalist framing (founder intent as binding) constitute an equally coherent commitment-system structure?',
    'Comparative analysis of the two framings'' internal coherence, historical corroboration, and institutional uptake. If originalist framing produces a stable commitment system with its own authority structure, the kernel is genuinely underdetermined.',
    'If originalist framing is equally coherent, the universalist reading''s claim to unique legitimacy is contested; the constraint family would have two live readings with different extraction profiles rather than one true reading and one deviation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel admits multiple coherent framings with different structural profiles').

omega_variable(
    expansion_coordination_vs_extraction_boundary,
    'At what point does the coordination cost of iterative expansion become extractive overhead rather than necessary coordination investment?',
    'Longitudinal analysis of expansion episodes: measure institutional transformation costs against inclusion gains. Episodes where costs vastly exceed measurable inclusion gains (e.g., resistance-driven delays, symbolic expansions without material change) indicate extraction creep.',
    'If recent expansions show rising cost-to-gain ratios, the constraint may be drifting from tangled_rope toward snare (extraction dominating coordination). If ratios are stable or declining, tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expansion_coordination_vs_extraction_boundary, empirical, 'Whether the constraint''s coordination function is being displaced by extractive creep in later expansion phases').

omega_variable(
    textualist_paradox_collapse,
    'Does the textualist_paradox_reading''s claim (universal language vs. restricted application = performative contradiction) structurally foreclose the universalist reading, or does it merely coexist as a critical observer position?',
    'Analyze whether the paradox reading generates its own institutional program or remains purely diagnostic. If it produces a rival expansion program, it competes; if it only critiques, it coexists.',
    'If the paradox reading forecloses universalism (by showing universal language CANNOT be sincerely implemented without founder intent), the universalist reading''s legitimacy collapses. If it coexists, the universalist reading remains a live expansion program alongside a critical sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_paradox_collapse, conceptual, 'Structural relationship between universalist and textualist-paradox readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amce_ur_tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.65).
narrative_ontology:measurement(amce_ur_tr_t1791, all_men_created_equal__universalist_reading, theater_ratio, 1791, 0.6).
narrative_ontology:measurement(amce_ur_tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.45).
narrative_ontology:measurement(amce_ur_tr_t1868, all_men_created_equal__universalist_reading, theater_ratio, 1868, 0.4).
narrative_ontology:measurement(amce_ur_tr_t1870, all_men_created_equal__universalist_reading, theater_ratio, 1870, 0.38).
narrative_ontology:measurement(amce_ur_tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(amce_ur_tr_t1954, all_men_created_equal__universalist_reading, theater_ratio, 1954, 0.3).
narrative_ontology:measurement(amce_ur_tr_t1964, all_men_created_equal__universalist_reading, theater_ratio, 1964, 0.28).
narrative_ontology:measurement(amce_ur_tr_t2015, all_men_created_equal__universalist_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(amce_ur_tr_t2025, all_men_created_equal__universalist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(amce_ur_be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(amce_ur_be_t1791, all_men_created_equal__universalist_reading, base_extractiveness, 1791, 0.18).
narrative_ontology:measurement(amce_ur_be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.32).
narrative_ontology:measurement(amce_ur_be_t1868, all_men_created_equal__universalist_reading, base_extractiveness, 1868, 0.38).
narrative_ontology:measurement(amce_ur_be_t1870, all_men_created_equal__universalist_reading, base_extractiveness, 1870, 0.4).
narrative_ontology:measurement(amce_ur_be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(amce_ur_be_t1954, all_men_created_equal__universalist_reading, base_extractiveness, 1954, 0.42).
narrative_ontology:measurement(amce_ur_be_t1964, all_men_created_equal__universalist_reading, base_extractiveness, 1964, 0.45).
narrative_ontology:measurement(amce_ur_be_t2015, all_men_created_equal__universalist_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(amce_ur_be_t2025, all_men_created_equal__universalist_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(amce_ur_su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.25).
narrative_ontology:measurement(amce_ur_su_t1791, all_men_created_equal__universalist_reading, suppression_requirement, 1791, 0.3).
narrative_ontology:measurement(amce_ur_su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.45).
narrative_ontology:measurement(amce_ur_su_t1868, all_men_created_equal__universalist_reading, suppression_requirement, 1868, 0.4).
narrative_ontology:measurement(amce_ur_su_t1870, all_men_created_equal__universalist_reading, suppression_requirement, 1870, 0.38).
narrative_ontology:measurement(amce_ur_su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement(amce_ur_su_t1954, all_men_created_equal__universalist_reading, suppression_requirement, 1954, 0.32).
narrative_ontology:measurement(amce_ur_su_t1964, all_men_created_equal__universalist_reading, suppression_requirement, 1964, 0.3).
narrative_ontology:measurement(amce_ur_su_t2015, all_men_created_equal__universalist_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(amce_ur_su_t2025, all_men_created_equal__universalist_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__universalist_reading, 0.08).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, reconstruction_amendments_enforcement).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, civil_rights_act_1964).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, equal_protection_jurisprudence).

% DUAL FORMULATION NOTE:
% The 'all men are created equal' kernel decomposes into three structurally distinct readings with different extraction profiles and stakeholder structures. This universalist_reading treats the universal language as a generative principle requiring iterative expansion (moderate extractiveness, tangled_rope). The originalist_reading treats founder intent as binding scope (low extractiveness, mountain-claimed but FSM-candidate). The textualist_paradox_reading treats the language-application gap as performative contradiction (high extractiveness, snare-claimed). All three share the same kernel_id but instantiate different constraints with different ε values, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, powerful, 0.8).
constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, organized, 0.25).
constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
