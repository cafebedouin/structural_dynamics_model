% ============================================================================
% CONSTRAINT STORY: indian_constitution_1950__fundamental_rights_part_iii
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_constitution_1950__fundamental_rights_part_iii, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: indian_constitution_1950__fundamental_rights_part_iii
 *   human_readable: Indian Constitution Part III: Justiciable Fundamental Rights (1950)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   Part III of the Indian Constitution (Articles 12-35) guarantees
 *   fundamental rights with a justiciable remedy through Article 32. The
 *   reading instantiated here is of Part III as the engine of transition from
 *   colonial subjection to republican citizenship: overnight (August 15,
 *   1950), the legal status of the Indian subject changed from colonial
 *   subject under Crown authority to rights-bearing citizen with enforceable
 *   remedies against the state. This is one reading of the contested kernel
 *   'indian_constitution_1950.' Other readings emphasize the amendment power
 *   and the judge-made basic structure doctrine
 *   (amendment_and_basic_structure), the non-justiciable directive principles
 *   as the Constitution's conscience (directive_principles_part_iv), the
 *   federal asymmetry that qualifies the pan-Indian reach
 *   (federal_asymmetry), or the social revolution provisions that use the
 *   Constitution to legislate systemic change (social_revolution_provisions).
 *   This reading focuses on Part III as the mechanism of rupture: the
 *   normative claim that the Constitution achieved in one moment what
 *   political mobilization had not achieved — a legal floor of rights binding
 *   on all state actors, enforceable by every citizen through the courts.
 *
 * KEY AGENTS:
 *   - Rights-bearing citizens: Beneficiary (powerless/mobile → Rope) — every person with Article 32 standing. The constraint solves the coordination problem: how to check arbitrary state power without vigilantism. Nominal beneficiary at independence; actual beneficiary requires consciousness and organizing.
 *   - Marginalized groups (SCs, religious minorities, women): Secondary beneficiary (moderate/constrained → Tangled Rope) — protected from the worst violence, yet constrained by slow courts and persistent structural discrimination. The rights floor is real but incomplete.
 *   - State enforcement machinery (police, administrators): Victim (moderate/trapped → Snare) — constrained from arbitrary action; cannot exit; experiences full force of justiciability without power to revise.
 *   - High Court Judiciary: Institutional beneficiary (institutional/mobile → Rope) — gains authority and dispute-resolution function from Article 32 jurisdiction.
 *   - Civil rights movement coalition (PUCL, PUDR, IUCN): Organized beneficiary (organized/constrained → Scaffold) — Part III enables organized legal action; has sunset logic as citizen consciousness and mass organizing grow.
 *   - Constitutional jurisprudence apparatus: Institutional maintainer (institutional/arbitrage → Piton) — sustains the constraint through continuous reinterpretation and creative judicial expansion of Article 21, maintaining the original engine through increasingly theatrical application.
 *   - Analytical observer (republican constitutionalism): (analytical/analytical → Rope) — the constraint solves the foundational problem of bounded government in a republic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_constitution_1950__fundamental_rights_part_iii, 0.18).
domain_priors:suppression_score(indian_constitution_1950__fundamental_rights_part_iii, 0.08).
domain_priors:theater_ratio(indian_constitution_1950__fundamental_rights_part_iii, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_constitution_1950__fundamental_rights_part_iii, extractiveness, 0.18).
narrative_ontology:constraint_metric(indian_constitution_1950__fundamental_rights_part_iii, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(indian_constitution_1950__fundamental_rights_part_iii, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_constitution_1950__fundamental_rights_part_iii, rope).
narrative_ontology:human_readable(indian_constitution_1950__fundamental_rights_part_iii, "Indian Constitution Part III: Justiciable Fundamental Rights (1950)").
narrative_ontology:topic_domain(indian_constitution_1950__fundamental_rights_part_iii, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(indian_constitution_1950__fundamental_rights_part_iii).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(indian_constitution_1950__fundamental_rights_part_iii, 'de32674f-e7df-424a-8403-b940cc429cd7').
narrative_ontology:cs_kernel_codification('de32674f-e7df-424a-8403-b940cc429cd7', formalized).
narrative_ontology:cs_authority_grounding('de32674f-e7df-424a-8403-b940cc429cd7', lineage).
narrative_ontology:cs_interpretation_layer_present('de32674f-e7df-424a-8403-b940cc429cd7').
narrative_ontology:cs_reading_relation('de32674f-e7df-424a-8403-b940cc429cd7', indian_constitution_1950__amendment_and_basic_structure, influences).
narrative_ontology:cs_reading_relation('de32674f-e7df-424a-8403-b940cc429cd7', indian_constitution_1950__directive_principles_part_iv, coexists_with).
narrative_ontology:cs_reading_relation('de32674f-e7df-424a-8403-b940cc429cd7', indian_constitution_1950__federal_asymmetry, influences).
narrative_ontology:cs_reading_relation('de32674f-e7df-424a-8403-b940cc429cd7', indian_constitution_1950__social_revolution_provisions, coexists_with).
narrative_ontology:cs_axiom('de32674f-e7df-424a-8403-b940cc429cd7', foundational, justiciable_rights_suppress_arbitrary_state_action).
narrative_ontology:cs_axiom_status(justiciable_rights_suppress_arbitrary_state_action, holdable).
narrative_ontology:cs_axiom_grounding('de32674f-e7df-424a-8403-b940cc429cd7', justiciable_rights_suppress_arbitrary_state_action, deontological).
narrative_ontology:cs_axiom('de32674f-e7df-424a-8403-b940cc429cd7', secondary, overnight_legal_status_change_sufficient_for_citizenship).
narrative_ontology:cs_axiom_status(overnight_legal_status_change_sufficient_for_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('de32674f-e7df-424a-8403-b940cc429cd7', overnight_legal_status_change_sufficient_for_citizenship, conventional).
narrative_ontology:cs_reference_frame('de32674f-e7df-424a-8403-b940cc429cd7', justiciable_fundamental_rights_floor).
narrative_ontology:cs_drift_state('de32674f-e7df-424a-8403-b940cc429cd7', contemporary_1975_emergency_onwards, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('de32674f-e7df-424a-8403-b940cc429cd7', '').
narrative_ontology:cs_kernel_id(indian_constitution_1950__fundamental_rights_part_iii, indian_constitution_1950).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_constitution_1950__fundamental_rights_part_iii, rights_bearing_citizens).
narrative_ontology:constraint_beneficiary(indian_constitution_1950__fundamental_rights_part_iii, judicial_review_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CITIZEN (ROPE) — The citizen with Article 32 standing experiences Part III as genuine coordination: a mechanism that translates their grievance into justiciable remedy. Low suppression; high mobility (can petition courts, organize, exit to other jurisdictions). The constraint solves a coordination problem: how to check arbitrary state action without individual vigilantism. Beneficiary of the rights floor.
constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED GROUP (TANGLED ROPE) — Groups subject to systemic subordination (Scheduled Castes, religious minorities, women under pre-amendment law) experience Part III as both coordination and extraction. The rights floor protects them from the worst violence, enabling exit from some oppressions — yet the courts' slow pace, remedies' limited reach, and persistence of structural discrimination mean extraction continues despite the nominal protection. Access to Article 32 is constrained by literacy, geographic distance, resource barriers, and the risk of state retaliation.
constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH COURT JUDICIARY (ROPE) — Courts experience Part III as coordination mechanism for dispute resolution. Article 32 petitions enable courts to prevent arbitrary state action and adjudicate rights. Low extraction experienced — the courts gain authority and legitimacy from the rights architecture. Mobile exit (judicial independence doctrine). The constraint enables the judiciary's function.
constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ENFORCEMENT MACHINERY (SNARE) — Police, administrators, lower-level officials experience Part III as suppression: a constraint on their operational freedom to enforce security, order, or policy without external review. They face the full force of justiciability without the power to revise the framework. Trapped: cannot exit state service easily; cannot ignore rights requirements. High extraction flows away from this agent (toward the beneficiary citizens).
constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS MOVEMENT COALITION (SCAFFOLD) — Organized civil rights actors (PUCL, PUDR, IUCN) see Part III as a temporary scaffolding that enabled the transition from colonial subjection to republican citizenship. The architecture has a sunset logic: as citizens become conscious of rights and organize for enforcement, the dependency on courts decreases and direct political action increases. The constraint is performative at the mass level (few citizens actually file Article 32 petitions) but structurally crucial for enabling organized action. Theater high relative to the small percentage of citizens who use formal courts.
constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL TEXT INTERPRETERS (PITON) — The institutional apparatus of constitutional jurisprudence (law faculties, bar associations, constitutional courts) maintains the Part III framework through continuous reinterpretation. The original engine has become theatrical: the specific scope of 'fundamental rights' has been radically revised upward through judicial creativity (implied rights, expansive interpretation of Article 21), meaning the constraint's function is now sustained not by the text's original force but by the judges' commitment to keep rights jurisprudence alive. The piton classification reflects that the textual constraint persists through institutional inertia and interpretive expansion, not because the original formulation remains sufficient.
constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REPUBLICAN CONSTITUTIONALISM (ROPE) — From a civilizational view, Part III instantiates a core principle of liberal republicanism: the rule of law requires a floor of individual rights enforceable against arbitrary state power. This is not extraction or coercion — it is the constitutive mechanism that enables the republican structure itself. The constraint solves the fundamental coordination problem of bounded government. Low suppression; low theater; genuine coordination function.
constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_constitution_1950__fundamental_rights_part_iii_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_constitution_1950__fundamental_rights_part_iii, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_constitution_1950__fundamental_rights_part_iii, TR),
    TR >= 0.70.

:- end_tests(indian_constitution_1950__fundamental_rights_part_iii_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.18): Very low, consistent with Rope classification. Part III is not primarily extractive — it establishes a rights floor that protects citizens from state violence and arbitrary action. The constraint solves a genuine coordination problem: how to prevent power abuse without requiring every citizen to have capacity for violent resistance. The small extractiveness value reflects that the courts themselves extract some institutional power and prestige through the Article 32 jurisdiction, but this extraction is not the primary function of the constraint. The beneficiaries (citizens with rights; the judiciary with jurisdiction) gain far more than they lose. SUPPRESSION (0.08): Very low. Part III explicitly reduces suppression by guaranteeing remedies — it is anti-suppressive in design. Alternatives are nominally available (political mobilization, appeal to other branches, exit via emigration). The low suppression value reflects the constraint's genuine function as an alternative to violence. THEATER RATIO (0.22): Low-moderate. The constraint begins with high functional content (Article 32 petitions are genuine remedy mechanisms; courts do prevent some state violations). Over 25 years, the ratio rises slightly as the apparatus becomes more institutionalized and some original dynamism is absorbed into bureaucratic process. The piton perspective notes that interpretive expansion (especially of Article 21) is becoming necessary to maintain the original engine's force against entropic reduction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's structural multiplicity. The citizen sees Rope (genuine coordination, protection from arbitrary action). The marginalized group sees Tangled Rope (coordination + incomplete extraction, slow remedies amid persistent discrimination). The state enforcement machinery sees Snare (constrained without power to revise). The judiciary sees Rope (gain of function). The organized coalition sees Scaffold (temporary structure enabling transition, with sunset as organizing deepens). The jurisprudence apparatus sees Piton (original engine maintained through interpretive theater). The analytical observer sees Rope (fundamental solution to bounded government). No single type is 'correct' — the indexical variation shows that the constraint's force depends on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is determined by the structural position relative to the constraint. Citizens with Article 32 standing are beneficiaries with mobile exit — low d (not full beneficiaries of extraction, but protected by the floor). State enforcement machinery bears extraction costs while trapped in state service — high d. Marginalized groups benefit from the rights floor but are constrained by court delays and structural discrimination — moderate d. The judiciary gains function and prestige while retaining mobility — low d (beneficiary). The analytical observer at civilizational scope sees the constraint as solving the foundational coordination problem of republican governance — d approaches the canonical value for institutional beneficiary with mobile exit (low d), yielding low effective extraction chi.
 *
 * MANDATROPHY ANALYSIS:
 *   Part III avoids mandatrophy through its genuine coordination function. The constraint is not pretending to be something it is not: it is protecting rights-bearing citizens from arbitrary state action, and it genuinely does this (extent varies by jurisdiction and agent class, but the function is real). The small extractiveness value reflects that the constraint solves a genuine problem rather than create an extractive apparatus disguised as coordination. The piton perspective's observation that interpretation is becoming necessary to maintain the original function indicates incipient theater rise, but does not suggest mandatrophy — the constraint remains functional, just increasingly dependent on judicial creativity to sustain force against entropic reduction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rights_effectiveness_versus_justiciability,
    'Does the justiciability of fundamental rights in Part III actually prevent state violations, or merely provide remedy after the fact? Are the rights themselves the constraint, or the remedy mechanism?',
    'Comparative analysis of state behavior before/after Article 32 availability; measurement of preventive effect vs. remedial effect; examination of police compliance rates with rights norms',
    'If preventive: Part III is genuine rights protection (Rope remains valid). If merely remedial: the constraint is less about suppressing state action and more about organizing post-hoc compensation (classification shifts toward Tangled Rope or Piton). Core structural question: does Article 32 availability change state behavior ex ante, or only ex post?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_effectiveness_versus_justiciability, empirical, 'Whether justiciability prevents violations or merely remedies them').

omega_variable(
    colonial_subject_remade_timing,
    'The kernel claim is ''colonial subject remade as rights-bearing citizen overnight'' (August 15, 1950). Did the legal status change instantaneously create the empirical reality of rights-bearing citizenship, or did consciousness and practice lag by decades?',
    'Historical analysis of rights claims and court petitions in years 0-10 post-Constitution; measurement of citizenship consciousness in surveys and memoirs; identification of when active rights-claiming became a mass phenomenon vs. elite phenomenon',
    'If instantaneous: the constraint is a genuine constitutional rupture (Rope classification holds). If lagged: the overnight transformation was nominally legal but practically gradual (suggests Piton or Scaffold classification — theater disguising incomplete transition). The empirical gap reveals whether the constraint''s force was instant or required decades of social organizing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_subject_remade_timing, empirical, 'Timing of legal status change vs. empirical citizenship consciousness').

omega_variable(
    basic_structure_foreclosure,
    'Does Part III''s justiciable rights architecture foreclose or merely influence the ''basic structure doctrine'' (the judge-made rule that certain amendments cannot touch the Constitution''s core)? Is rights-justiciability itself part of the basic structure, or only protected by it?',
    'Constitutional jurisprudence analysis: does the Supreme Court treat Article 32 (right to constitutional remedy) as unamendable? What would happen if Parliament attempted to repeal Article 32? Historical precedent from emergency periods (1975-1977) when rights were suspended.',
    'If Part III is itself basic structure: the constraint is inviolable even by amendment (Rope classification reinforced). If Part III is amendable: later amendments could theoretically narrow rights (suggests Tangled Rope or weaker position). This is the core sibling relationship: amendment_and_basic_structure reading directly engages whether Part III''s force is permanent or revisable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_structure_foreclosure, conceptual, 'Whether Part III rights are themselves part of the unamendable basic structure').

omega_variable(
    social_revolution_versus_rights_floor,
    'Is Part III fundamentally about individual rights against the state (negative liberty framing), or about enabling the social revolution provisions of Part IV (positive obligation framing)? Do fundamental rights serve as a floor to prevent collapse into subordination, or as a ladder toward the directive principles'' vision?',
    'Jurisprudential mapping: examine whether courts use Part III rights primarily to block state action (negative) or to compel state action toward social policy goals (positive). Track evolution from narrow rights interpretation (1950s) to expansive reading via Article 21 (1970s onward).',
    'Negative rights framing: Part III is constraint on arbitrary state power (pure Rope). Positive obligations framing: Part III becomes engine for social transformation toward Part IV goals (suggests hybrid or Tangled Rope with social beneficiary). This distinction maps directly to the sibling reading social_revolution_provisions: they coexist or influence depending on which framing dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_revolution_versus_rights_floor, conceptual, 'Whether Part III is negative rights floor or positive obligation engine').

omega_variable(
    federal_asymmetry_interaction,
    'Part III rights are nominally pan-Indian, but federal asymmetry reading notes state reorganization and special statuses (Jammu & Kashmir Article 370, tribal areas, special northeast provisions). Do Part III rights apply equally in all jurisdictions, or is their force modulated by federal structure?',
    'Constitutional text analysis: trace carve-outs and jurisdictional limits in Part III itself (e.g., Article 19 exceptions ''in the interests of public order''). Historical analysis: do J&K, tribal areas, and special jurisdictions have different effective rights regimes? Jurisprudence analysis: has the Supreme Court applied Part III uniformly across asymmetric federal zones?',
    'Uniform application: Part III is genuinely pan-Indian (Rope holds). Modulated by asymmetry: Part III''s force varies by jurisdiction (suggests Tangled Rope or Piton — the universal text is theater while actual rights regimes are asymmetric). This directly influences the federal_asymmetry sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_asymmetry_interaction, empirical, 'Uniformity of Part III application across federal asymmetries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_constitution_1950__fundamental_rights_part_iii, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indconst_fr_theater_1950, indian_constitution_1950__fundamental_rights_part_iii, theater_ratio, 0, 0.15).
narrative_ontology:measurement(indconst_fr_theater_1960, indian_constitution_1950__fundamental_rights_part_iii, theater_ratio, 10, 0.2).
narrative_ontology:measurement(indconst_fr_theater_1975, indian_constitution_1950__fundamental_rights_part_iii, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(indconst_fr_extractiveness_1950, indian_constitution_1950__fundamental_rights_part_iii, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(indconst_fr_extractiveness_1960, indian_constitution_1950__fundamental_rights_part_iii, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(indconst_fr_extractiveness_1975, indian_constitution_1950__fundamental_rights_part_iii, base_extractiveness, 25, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_constitution_1950__fundamental_rights_part_iii, enforcement_mechanism).
narrative_ontology:affects_constraint(indian_constitution_1950__fundamental_rights_part_iii, indian_constitution_1950__amendment_and_basic_structure).
narrative_ontology:affects_constraint(indian_constitution_1950__fundamental_rights_part_iii, indian_constitution_1950__directive_principles_part_iv).
narrative_ontology:affects_constraint(indian_constitution_1950__fundamental_rights_part_iii, indian_constitution_1950__federal_asymmetry).
narrative_ontology:affects_constraint(indian_constitution_1950__fundamental_rights_part_iii, indian_constitution_1950__social_revolution_provisions).

% DUAL FORMULATION NOTE:
% The Indian Constitution (kernel: 'indian_constitution_1950') decomposes into at least five structurally distinct constraint readings. Part III is one reading: justiciable fundamental rights as the engine of citizenship. Each reading has a distinct ε and distinct beneficiary/victim structure. Part III (this file) models the rights floor and its enforcement mechanism. Amendment_and_basic_structure reads the Constitution as a fight over its own revision (distinct ε reflecting the meta-constitutional constraint). Directive_principles_part_iv reads the non-justiciable social vision (distinct ε reflecting non-enforceability). Federal_asymmetry reads the uneven territorial reach. Social_revolution_provisions reads the Constitution as legislating transformation. Each story must be authored separately with its own metrics and perspectives; the network links show how they interact and influence each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
