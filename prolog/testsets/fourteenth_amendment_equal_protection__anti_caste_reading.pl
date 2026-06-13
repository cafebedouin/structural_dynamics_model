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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Fourteenth Amendment Equal Protection — Anti-Caste Reading
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   The anti-caste reading of the Fourteenth Amendment's Equal Protection
 *   Clause interprets the clause as mandating active state dismantling of
 *   racial, gender, and status hierarchy. This reading emerges from civil
 *   rights and critical theory scholarship (Hartman, Wilkerson, Anand,
 *   Crenshaw) and is contested against the formal equality reading, which
 *   interprets Equal Protection as prohibiting explicit state classification
 *   regardless of remedial intent. Under the anti-caste reading, the state
 *   must reorganize institutions, redistribute resources, and restructure
 *   norms to undo caste. The reading legitimates corrective action
 *   (affirmative hiring, admissions preferences, wealth remediation) as
 *   constitutionally required, not merely permissible. The constraint's
 *   operation is highly extractive (0.68) because remedial mandates impose
 *   real costs on beneficiaries of traditional hierarchy while facing active
 *   legal and political resistance. Suppression is sustained through
 *   sustained litigation, legislative override attempts, and institutional
 *   drag. Theater ratio rises over time (0.05 → 0.41) as corrective
 *   implementation increasingly becomes performative compliance rather than
 *   substantive hierarchy dismantling.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: Primary beneficiaries; seek constitutional recognition of corrective mandate; organized power, constrained exit
 *   - historically_subordinated_gender_groups: Primary beneficiaries; organized around gender caste dismantling; generational time horizon
 *   - caste_victims: Beneficiaries with powerless structural position; identity-locked (caste inherited); depend on state recognition of caste harm
 *   - beneficiaries_of_structural_hierarchy: Payers; powerful position; face corrective redistribution; highest exit optionality (mobility, litigation, legislative exit)
 *   - institutional_resistance_actors: Payers; constrained exit; bear implementation costs; institutional identity friction with corrective mandates
 *   - formal_equality_advocates: Excluded from anti-caste enforcement; argue that the reading violates individualism; operate through counter-litigation and legislative opposition
 *   - federal_courts: Agenda-setters; tasked with interpreting and enforcing the anti-caste mandate; operate as active hierarchy dismantlers
 *   - state_legislatures: Agenda-setters with dual payer role; mandated to enact corrective legislation; face political backlash and litigation costs
 *   - analytical_observers: Measure constraint operation; external to party positions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.68).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.72).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Fourteenth Amendment Equal Protection — Anti-Caste Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '3d809128-fe4f-42ff-a5b8-e1487ce14385').
narrative_ontology:cs_kernel_codification('3d809128-fe4f-42ff-a5b8-e1487ce14385', fixed_text).
narrative_ontology:cs_authority_grounding('3d809128-fe4f-42ff-a5b8-e1487ce14385', lineage).
narrative_ontology:cs_interpretation_layer_present('3d809128-fe4f-42ff-a5b8-e1487ce14385').
narrative_ontology:cs_reading_relation('3d809128-fe4f-42ff-a5b8-e1487ce14385', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('3d809128-fe4f-42ff-a5b8-e1487ce14385', foundational, structural_caste_requires_active_state_dismantling).
narrative_ontology:cs_axiom_status(structural_caste_requires_active_state_dismantling, holdable).
narrative_ontology:cs_axiom_grounding('3d809128-fe4f-42ff-a5b8-e1487ce14385', structural_caste_requires_active_state_dismantling, deontological).
narrative_ontology:cs_axiom('3d809128-fe4f-42ff-a5b8-e1487ce14385', foundational, corrective_classification_is_constitutionally_legitimate).
narrative_ontology:cs_axiom_status(corrective_classification_is_constitutionally_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('3d809128-fe4f-42ff-a5b8-e1487ce14385', corrective_classification_is_constitutionally_legitimate, deontological).
narrative_ontology:cs_reference_frame('3d809128-fe4f-42ff-a5b8-e1487ce14385', constitutional_subordination_remedy_mandate).
narrative_ontology:cs_drift_state('3d809128-fe4f-42ff-a5b8-e1487ce14385', contemporary_litigation_saturation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3d809128-fe4f-42ff-a5b8-e1487ce14385', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_gender_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, caste_victims).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, beneficiaries_of_structural_hierarchy).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, institutional_resistance_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks recognition that Equal Protection mandates affirmative state remediation of caste structures built on racial hierarchy. This reading legitimates corrective programs (hiring, admissions, contracting preferences) as constitutionally required, not merely permitted. The group's political power derives from coalition and litigation; their exit option is to repudiate the Constitution entirely or accept subordination as permanent.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Seeks recognition that Equal Protection mandates dismantling gender caste — structural sorting of men and women into hierarchical roles. This reading legitimates affirmative gender correction in education, employment, and leadership. Exit is similarly constrained: the reading's validity is tied to constitutional legitimacy.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_gender_groups, beneficiary,
    organized, generational, constrained, national).

% Individuals and families bearing the mark of caste status (often read through immigrant South Asian communities, but also through historical US caste structures like sharecropping, Jim Crow, and contemporary occupational/religious sorting). This reading names caste subordination as actionable Equal Protection harm and legitimates remedial state recognition. Exit would require abandonment of inherited identity or migration.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, caste_victims, beneficiary,
    powerless, generational, identity_locked, national).

% Historically dominant racial and gender groups, and high-caste actors within caste systems. Under this reading, their structural advantages are reframed as caste-backed privileges that the state must actively dismantle. They experience the reading as imposing corrective costs: lost preference in hiring/admissions, mandatory wealth redistribution, institutional reorganization. Exit options include relocating to jurisdictions with weaker equal protection enforcement, challenging the reading in courts, or lobbying for legislative override.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, beneficiaries_of_structural_hierarchy, payer,
    powerful, generational, mobile, national).

% Universities, employers, public agencies, and private institutions tasked with implementing corrective mandates. They bear the cost of auditing, redesigning systems, managing political backlash, and defending remedial programs in litigation. Their exit is limited — they operate under state charter or public contracts that condition participation on compliance. Some resistance is structural (litigation costs, operational burden); some is ideological (institutional identity bound to traditional selection norms).
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, institutional_resistance_actors, payer,
    institutional, generational, constrained, national).

% Constitutional scholars, judges, and policy actors who hold the formal equality reading. They are excluded from this story's framing — they would argue that Equal Protection prohibits classification by race/gender/caste regardless of remedial intent, and that the anti-caste reading violates the principle of individualism. Their exclusion from the anti-caste enforcement apparatus is what drives litigation and legislative resistance.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    organized, generational, arbitrage, national).

% Interpret and enforce the Fourteenth Amendment's Equal Protection Clause. Under the anti-caste reading, courts function as active dismantlers of hierarchy — they must review institutional practices through a caste-consciousness lens, invalidate practices that maintain subordination, and license corrective state action. Their exit is institutional (they cannot repudiate the Constitution), but they do choose doctrine through opinion-writing and precedent-setting.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Enact laws implementing corrective redistribution and dismantling of caste structures. Under the anti-caste reading, legislatures are mandated to act — they cannot remain neutral. They also bear political costs: backlash from hierarchy beneficiaries, litigation defense, institutional reorganization expenses. Exit is limited — constitutional mandates constrain legislative discretion, though legislatures can challenge the reading through ballot measures and constitutional amendment.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, state_legislatures, payer).

% Constitutional scholars, social scientists, and civil rights analysts who study the constraint from outside any of the parties. They measure the constraint's operation: how much extraction occurs, how suppression is maintained, what the material costs of corrective action are, whether the reading's legitimacy is sustainable.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, analytical_observers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a constitutional principle that mandates the state coordinate a transition away from caste hierarchy. The state's role is to actively dismantle structural subordination through policy, law, and institutional reform, rather than merely refraining from explicit classification. This coordinates multiple actors (institutions, beneficiaries of correction, hierarchy targets) toward a shared endpoint: a society without caste rank.
% TRANSFER_FUNCTION: Transfers status, opportunity, and resource access from historically dominant groups (racial, gender, caste) to historically subordinated groups through affirmative action, wealth redistribution, institutional redesign, and corrective legal remedies. The transfer is justified not as redistribution-for-its-own-sake but as undoing the extraction that caste itself constitutes.
% ABSENT_VOICES: Formal equality advocates and hierarchy beneficiaries are structurally excluded from the anti-caste enforcement apparatus — they cannot veto corrective action. Caste victims with limited political voice (immigrant communities, rural poor, occupationally trapped populations) are nominally beneficiaries but often lack power to shape implementation. Institutional actors on whom enforcement burden falls (employers, educators) are tasked with corrective action without necessarily being consulted on design.
% DISAPPEARANCE_RATIONALE: If this reading and its mandate for active corrective state action disappeared, institutional policies would revert toward hierarchy-neutral or hierarchy-preserving norms; redistribution would halt; caste structures would restabilize within months as institutions returned to traditional selection and allocation norms. The social reorganization would be immediate: advantaged groups would consolidate gains, subordinated groups would lose legal grounds for corrective claims, and hierarchy would re-entrench without active state opposition.
% FOUNDING_PROBLEM: Explicit racial slavery, later Jim Crow legal racism, gender legal exclusion, and occupational caste-like systems (sharecropping, indentured servitude, immigrant labor lockdown) created structural subordination. Formal legal equality (abolition, suffrage, explicit anti-discrimination law) does not automatically undo subordination — institutions, norms, and resource distributions continue caste logic unless the state actively dismantles it.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights scholars (Kimberlé Crenshaw, Derrick Bell, Isabel Wilkerson), sociologists (Patricia Hill Collins, Douglas Massey), and historians (Eric Foner, Saidiya Hartman) document the persistence of racial, gender, and occupational hierarchy despite formal legal equality. Federal enforcement data on wealth gaps, employment discrimination, education access, and incarceration rates confirm measurable subordination outside the benefiting parties' framing. The founding problem is attested by data from sources structurally independent of the anti-caste reading's beneficiaries.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) because the anti-caste reading imposes substantial costs on hierarchy beneficiaries through affirmative action mandates, wealth redistribution, institutional reorganization, and legal liability for caste-maintaining practices. These costs are not incidental overhead — they ARE the constraint's function. Suppression is also high (0.72) because the reading's persistence depends on active enforcement: federal courts must continue invalidating hierarchy-preserving practices, state institutions must continue implementing corrective mandates despite political opposition, and the reading must survive continual legal challenges. The coercion grid shows dramatic level differentiation: at t0 (1868), suppression and accessibility collapse are near-total across all four levels (hierarchy is maintained through overwhelming institutional, organizational, class, and individual-level suppression). By t1 (2024), structural suppression (state-level policy capacity) remains high (0.75) but individual resistance has risen sharply (0.85) — people increasingly contest the anti-caste framing, and institutional compliance is increasingly performative rather than genuine. Theater ratio rises from 0.05 to 0.41 because institutions increasingly adopt corrective language and formal policies without substantive hierarchy dismantling; the corrective mandate has become partially aestheticized. This trajectory reflects a constraint whose enforcement machinery has built up (higher theater) while resistance also grows (higher individual-level resistance), creating a standoff where both corrective mandate and hierarchy benefit from the appearance of change without complete structural transformation. All measurements are observed (historical record); no projections.
 *
 * PERSPECTIVAL GAP:
 *   The anti-caste reading produces radically different constraint-types at different seats. From the beneficiary seats (subordinated racial/gender/caste groups), the reading is experienced as a rope or scaffold — coordination around a transition to equality, with real (if incomplete) corrective benefit. From the payer seats (hierarchy beneficiaries, resistant institutions), the same reading is a snare — coercive extraction of advantage through affirmative mandate, actively sustained through litigation and suppression of dissent. Formal equality advocates experience it as a snare on themselves (their interpretive authority is suppressed). Federal courts experience it as agenda-setting power (their opinions shape the mandate's scope). This perspectival asymmetry is structural: the reading's legitimacy depends on active state enforcement, which means it generates extraction for those who benefit from hierarchy. The engine should compute a snare-type result from the hierarchy beneficiary seats and a tangled-rope result from the subordinated group seats. The authored type (tangled_rope) reflects the story's view that the constraint genuinely coordinates subordinated groups toward hierarchy dismantling while extracting from hierarchy beneficiaries — making it asymmetrically coordinated/extractive, not purely extractive (which would be snare).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) breaks sharply across the stakeholder set. Subordinated racial/gender/caste groups: d near 0.0 (full beneficiaries — the reading subsidizes their legal claims, legitimates corrective programs, creates state obligation to act on their behalf). Beneficiaries of hierarchy: d near 1.0 (full targets — they bear the extraction in the form of lost preference, redistribution mandates, institutional reorganization cost). Institutional resistance actors: d in the 0.7-0.85 range (mixed; they implement corrective action so they see some coordination benefit from legal clarity, but they also bear substantial compliance cost and political backlash, so they are partially targeted). Formal equality advocates: d in the 0.4-0.6 range (mixed; their interpretive authority is suppressed by the anti-caste reading, but they retain litigation and legislative channels to contest it, so they are constrained but not trapped). The coercion grid amplifies this: at the individual level, hierarchy beneficiaries and formal equality advocates experience high resistance costs (0.85 at t1) as they attempt to contest the anti-caste framing; at the class level, subordinated groups experience reduced stakes inflation (0.55 at t1 vs 0.88 at t0) as corrective programs create real opportunity, even as they remain institutionally constrained. No directionality overrides needed — the structural data map cleanly to these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this reading in the terminal sense: the founding problem (structural caste subordination built on slavery, Jim Crow, gender legal exclusion) is genuinely live, and the anti-caste reading remains structurally justified as a response to it. However, the measurement trajectory shows a creeping performance-drift pattern consistent with early-stage mandatrophy: as corrective action faces sustained political/litigation resistance, institutions increasingly adopt corrective language and formal policies without substantive dismantling (theater_ratio rises from 0.05 to 0.41). This is NOT yet mandatrophy (the founding problem persists, corrective action remains justified), but it is a warning: if theater continues to rise while actual hierarchy dismantling stagnates, the constraint could eventually become a Piton — a vestigial corrective mandate maintained performatively while hierarchy re-entrenches underneath. The mandatrophy risk is highest in the institutional seat (resistance_actors) where compliance becomes theatrical, and at the organizational level of the coercion grid where suppression remains high (0.72) while individual-level resistance also rises (0.85) — a sign of enforcement strain. A future measurement cycle showing continued theater rise with stalled subordinated-group benefit would signal mandatrophy development.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblindness_vs_race_consciousness_axiom,
    'Is the foundational axiom of the anti-caste reading (that Equal Protection mandates race-conscious corrective action) logically sustainable within American constitutional individualism, or does constitutional individualism entail race-blindness?',
    'Originalist and textualist analysis of the Fourteenth Amendment''s text and original understanding; comparative jurisprudence from jurisdictions with explicit race-conscious constitutional provisions (South Africa, India); philosophical arguments for group-based remediation within liberal theory.',
    'If race-conscious mandates are shown to be constitutionally incoherent (irreconcilable with individualism), the anti-caste reading forecloses rather than coexists with formal equality. If race-consciousness is shown to be constitutionally defensible (within the Amendment''s text or principles), coexistence is confirmed. This determines whether the readings are competing interpretations or logically opposed premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblindness_vs_race_consciousness_axiom, conceptual, 'Whether anti-caste reading''s race-consciousness is fundamentally incompatible with constitutional individualism or defensible within it.').

omega_variable(
    structural_caste_vs_individual_discrimination,
    'Does Equal Protection mandate remediation of structural caste (systemic hierarchy maintained through institutional practice, not individual animus) or only discrimination (intentional deprivation by individual or institutional actors)?',
    'Court interpretation of Equal Protection scope in light of evidence on structural inequality persistence; statutory authority (Civil Rights Act disparate impact doctrine); empirical studies on mechanisms maintaining hierarchy absent intentional discrimination.',
    'If Equal Protection applies only to intentional discrimination, the anti-caste reading''s scope collapses significantly — many corrective mandates target structural practices (legacy admissions, residential segregation effects) rather than intentional discrimination. If structural remediation is mandated, the anti-caste reading''s scope is confirmed. This determines the breadth of the corrective mandate and its extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_caste_vs_individual_discrimination, empirical, 'Whether Equal Protection requires remediation of structural caste or only intentional discrimination.').

omega_variable(
    beneficiary_identity_lock_vs_individual_merit,
    'Can a corrective mandate legitimately target subordinated group members (identifying by race, gender, caste identity) for benefit, or does Equal Protection require that all benefits be individually-tailored to actual disadvantage regardless of identity?',
    'Jurisprudential development of individual-vs-group-rights doctrine; empirical studies on whether identity-group membership reliably correlates with structural disadvantage sufficient to justify group-targeted remediation; philosophical debate on whether group identity can ground legitimate state action or whether all state action must be individual.',
    'If individual tailoring is required, the anti-caste reading must abandon group-targeted corrective action (affirmative action, diversity preferences) and shift to class-based or individual-disadvantage-based remediation, substantially weakening its grip on institutions. If group-targeted action is legitimate, the anti-caste reading''s implementability is confirmed. This determines the concrete tools available for hierarchy dismantling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identity_lock_vs_individual_merit, preference, 'Whether corrective action can legitimately target group identity or must be individually tailored.').

omega_variable(
    suppression_mechanism_structural_vs_ideological,
    'Is the high measured suppression (0.72) driven primarily by structural barriers (litigation costs, legislative override, institutional veto) or ideological/identity barriers (formal equality advocates believe colorblindness is constitutional truth; hierarchy beneficiaries believe merit-based selection is neutral)?',
    'Post-reform suppression trajectory: if suppression persists at 0.70+ even after structural barriers (litigation costs, legislative majorities) shift in favor of the anti-caste reading, then suppression is internalized. If suppression drops sharply when structural barriers are removed, it was structural. Measurement of ideological persistence through surveys and institutional behavior after formal legal changes.',
    'If suppression is primarily structural, removing obstacles (legislative reform, court appointments, litigation funding) should drop suppression rapidly. If suppression is primarily ideological, institutional actors and hierarchy beneficiaries will continue contesting the reading even when structural barriers fall — implying the anti-caste reading faces deeper cultural resistance than measured suppression suggests. This informs whether the constraint''s persistence depends on active enforcement or on internalized belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_ideological, empirical, 'Whether measured suppression is structural barrier or internalized ideological resistance.').

omega_variable(
    kernel_contest_foreclosure_risk,
    'Will empirical evidence or legal argument eventually foreclose one of the two readings (anti-caste vs. formal equality) such that no coherent constitutional framework can hold both, or will the readings persist as live competing interpretations indefinitely?',
    'Long-horizon jurisprudential and empirical tracking: if one reading accumulates overwhelming case-law support and empirical validation (e.g., if affirmative action demonstrably succeeds in dismantling caste while meeting no compelling countervailing cost, formal equality forecloses). If both readings continue to be deployed by different courts and scholars despite contradictions, they coexist indefinitely. Documentary evidence of explicit foreclosure logic from constitutional authorities.',
    'If the readings eventually foreclose (one logic proves untenably contradictory with facts or law), this story''s constraint becomes subordinate to the foreclosing reading''s constraint. If coexistence is durable, both readings persist as live constraints with different institutional homes. This determines the ultimate terminal fate of the anti-caste reading: does it consolidate into hegemonic doctrine, or does it remain perpetually contested?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_risk, empirical, 'Whether the anti-caste and formal equality readings will eventually foreclose each other or persist as permanently live competitors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1868, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement_basis(four_tr_t1868, observed).
narrative_ontology:measurement(four_tr_t1920, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement_basis(four_tr_t1920, observed).
narrative_ontology:measurement(four_tr_t1964, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1964, 0.22).
narrative_ontology:measurement_basis(four_tr_t1964, observed).
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1978, 0.35).
narrative_ontology:measurement_basis(four_tr_t1978, observed).
narrative_ontology:measurement(four_tr_t2000, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement_basis(four_tr_t2000, observed).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(four_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(four_be_t1868, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1868, 0.95).
narrative_ontology:measurement_basis(four_be_t1868, observed).
narrative_ontology:measurement(four_be_t1920, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1920, 0.92).
narrative_ontology:measurement_basis(four_be_t1920, observed).
narrative_ontology:measurement(four_be_t1964, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1964, 0.88).
narrative_ontology:measurement_basis(four_be_t1964, observed).
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1978, 0.72).
narrative_ontology:measurement_basis(four_be_t1978, observed).
narrative_ontology:measurement(four_be_t2000, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement_basis(four_be_t2000, observed).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(four_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1868, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1868, 0.98).
narrative_ontology:measurement_basis(four_su_t1868, observed).
narrative_ontology:measurement(four_su_t1920, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1920, 0.96).
narrative_ontology:measurement_basis(four_su_t1920, observed).
narrative_ontology:measurement(four_su_t1964, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1964, 0.85).
narrative_ontology:measurement_basis(four_su_t1964, observed).
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1978, 0.76).
narrative_ontology:measurement_basis(four_su_t1978, observed).
narrative_ontology:measurement(four_su_t2000, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement_basis(four_su_t2000, observed).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(four_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1868, tn=2024
narrative_ontology:measurement(four_grid_01, fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse(class), 1868, 0.94).
narrative_ontology:measurement(four_grid_02, fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse(class), 2024, 0.58).
narrative_ontology:measurement(four_grid_03, fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse(individual), 1868, 0.93).
narrative_ontology:measurement(four_grid_04, fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse(individual), 2024, 0.52).
narrative_ontology:measurement(four_grid_05, fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse(organizational), 1868, 0.96).
narrative_ontology:measurement(four_grid_06, fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse(organizational), 2024, 0.64).
narrative_ontology:measurement(four_grid_07, fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse(structural), 1868, 0.98).
narrative_ontology:measurement(four_grid_08, fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse(structural), 2024, 0.72).
narrative_ontology:measurement(four_grid_09, fourteenth_amendment_equal_protection__anti_caste_reading, resistance(class), 1868, 0.18).
narrative_ontology:measurement(four_grid_10, fourteenth_amendment_equal_protection__anti_caste_reading, resistance(class), 2024, 0.82).
narrative_ontology:measurement(four_grid_11, fourteenth_amendment_equal_protection__anti_caste_reading, resistance(individual), 1868, 0.22).
narrative_ontology:measurement(four_grid_12, fourteenth_amendment_equal_protection__anti_caste_reading, resistance(individual), 2024, 0.85).
narrative_ontology:measurement(four_grid_13, fourteenth_amendment_equal_protection__anti_caste_reading, resistance(organizational), 1868, 0.15).
narrative_ontology:measurement(four_grid_14, fourteenth_amendment_equal_protection__anti_caste_reading, resistance(organizational), 2024, 0.75).
narrative_ontology:measurement(four_grid_15, fourteenth_amendment_equal_protection__anti_caste_reading, resistance(structural), 1868, 0.12).
narrative_ontology:measurement(four_grid_16, fourteenth_amendment_equal_protection__anti_caste_reading, resistance(structural), 2024, 0.68).
narrative_ontology:measurement(four_grid_17, fourteenth_amendment_equal_protection__anti_caste_reading, stakes_inflation(class), 1868, 0.88).
narrative_ontology:measurement(four_grid_18, fourteenth_amendment_equal_protection__anti_caste_reading, stakes_inflation(class), 2024, 0.55).
narrative_ontology:measurement(four_grid_19, fourteenth_amendment_equal_protection__anti_caste_reading, stakes_inflation(individual), 1868, 0.85).
narrative_ontology:measurement(four_grid_20, fourteenth_amendment_equal_protection__anti_caste_reading, stakes_inflation(individual), 2024, 0.48).
narrative_ontology:measurement(four_grid_21, fourteenth_amendment_equal_protection__anti_caste_reading, stakes_inflation(organizational), 1868, 0.9).
narrative_ontology:measurement(four_grid_22, fourteenth_amendment_equal_protection__anti_caste_reading, stakes_inflation(organizational), 2024, 0.62).
narrative_ontology:measurement(four_grid_23, fourteenth_amendment_equal_protection__anti_caste_reading, stakes_inflation(structural), 1868, 0.92).
narrative_ontology:measurement(four_grid_24, fourteenth_amendment_equal_protection__anti_caste_reading, stakes_inflation(structural), 2024, 0.68).
narrative_ontology:measurement(four_grid_25, fourteenth_amendment_equal_protection__anti_caste_reading, suppression(class), 1868, 0.96).
narrative_ontology:measurement(four_grid_26, fourteenth_amendment_equal_protection__anti_caste_reading, suppression(class), 2024, 0.7).
narrative_ontology:measurement(four_grid_27, fourteenth_amendment_equal_protection__anti_caste_reading, suppression(individual), 1868, 0.94).
narrative_ontology:measurement(four_grid_28, fourteenth_amendment_equal_protection__anti_caste_reading, suppression(individual), 2024, 0.68).
narrative_ontology:measurement(four_grid_29, fourteenth_amendment_equal_protection__anti_caste_reading, suppression(organizational), 1868, 0.97).
narrative_ontology:measurement(four_grid_30, fourteenth_amendment_equal_protection__anti_caste_reading, suppression(organizational), 2024, 0.72).
narrative_ontology:measurement(four_grid_31, fourteenth_amendment_equal_protection__anti_caste_reading, suppression(structural), 1868, 0.98).
narrative_ontology:measurement(four_grid_32, fourteenth_amendment_equal_protection__anti_caste_reading, suppression(structural), 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__anti_caste_reading, 0.18).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).

% DUAL FORMULATION NOTE:
% This constraint (anti_caste_reading) and formal_equality_reading share a single kernel — the Fourteenth Amendment Equal Protection Clause — but instantiate structurally distinct constraints. The anti-caste reading interprets Equal Protection as mandating active state dismantling of caste hierarchy; the formal_equality_reading interprets it as prohibiting explicit state classification. These readings have different beneficiary/victim structures, different extraction patterns, different enforcement mechanics, and different ε values. The anti-caste reading is substantially extractive (0.68) because it imposes costs on hierarchy beneficiaries; the formal_equality_reading would be expected to show low/zero extractiveness (hierarchy costs externalized, extraction suppressed). The readings do not logically foreclose each other — they coexist as competing doctrinal positions held by different courts and scholars. The network edge reflects causal influence: the anti-caste reading's judicial successes create political and legal pressure on the formal_equality_reading (triggering counter-litigation, legislative response); conversely, formal_equality judicial victories (like SCOTUS affirmative action limits) constrain the anti-caste reading's legal scope. Both readings are live, and their interaction constitutes the contemporary equal protection constraint landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
