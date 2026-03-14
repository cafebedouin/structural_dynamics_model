% ============================================================================
% CONSTRAINT STORY: medical_model_pathologization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medical_model_pathologization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: medical_model_pathologization
 *   human_readable: Medical Model Pathologization of Non-Medical Phenomena
 *   domain: healthcare/epistemology/social_control
 *
 * SUMMARY:
 *   The medical model pathologization constraint describes how biomedical
 *   frameworks for understanding human variation function simultaneously as
 *   coordination mechanisms and extraction systems. The constraint operates
 *   across multiple institutional layers: diagnostic nosology (DSM/ICD),
 *   pharmaceutical markets, clinical gatekeeping, insurance systems, and
 *   identity formation. For powerless individuals newly diagnosed, the
 *   constraint appears as pure extraction (snare) — permanent stigma,
 *   medication dependency, blocked life opportunities, and internalized
 *   deficit identity with no exit. For institutional actors (psychiatric
 *   establishment, pharma), the same constraint appears as pure coordination
 *   (rope) — organizing knowledge, enabling research, justifying treatment,
 *   providing career structure. For organized advocates (neurodiversity
 *   movement), it appears as mixed extraction and coordination (tangled rope)
 *   — genuine access benefits coexist with confinement to disease narrative.
 *   For reform movements, it appears as temporary (scaffold) — alternative
 *   frameworks (biopsychosocial, neurodiversity, difference-based) are
 *   maturing and will eventually replace pure medical pathologization. The
 *   constraint's theater ratio (0.68, rising to 0.68 over the interval)
 *   reflects that diagnostic classification rituals maintain appearance of
 *   scientific objectivity while primarily sorting people into billing and
 *   legal categories. The extractiveness has risen from 0.35 to 0.58 over the
 *   measurement interval, indicating that diagnostic expansion and
 *   medicalization deepening have increased the proportion of human variation
 *   subjected to pathology framing, while therapeutic theater has become more
 *   elaborate to justify this expansion.
 *
 * KEY AGENTS:
 *   - Diagnosed Individual: Primary victim (powerless/trapped) — bears stigma, medication effects, blocked opportunities, identity lock; permanent extraction with no exit
 *   - Patient Population Aggregate: Secondary victim (moderate/constrained) — benefits from medical framework (access, accommodation, research) but locked into disease identity; modestly constrained exit
 *   - Psychiatric Medical Establishment: Primary beneficiary (institutional/arbitrage) — controls diagnostic authority, organizes knowledge, justifies research; full exit options; sees constraint as pure coordination
 *   - Pharmaceutical Industry: Primary beneficiary (institutional/arbitrage) — diagnostic categories = markets; expands pathology definitions; full institutional power; sees constraint as enabling market coordination
 *   - Neurodiversity Advocacy Movement: Organized secondary actor (organized/constrained) — uses medical framework to secure accommodations but constrained from exiting it; genuine mixed benefit and extraction
 *   - Diagnostic Reform Movement: Organized tertiary actor (organized/constrained) — building alternative frameworks; sees constraint as temporary/solvable; exit path visible but blocked by institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent medical framework as inherent feature of knowledge about health
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_model_pathologization, 0.58).
domain_priors:suppression_score(medical_model_pathologization, 0.65).
domain_priors:theater_ratio(medical_model_pathologization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_model_pathologization, extractiveness, 0.58).
narrative_ontology:constraint_metric(medical_model_pathologization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(medical_model_pathologization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_model_pathologization, tangled_rope).
narrative_ontology:human_readable(medical_model_pathologization, "Medical Model Pathologization of Non-Medical Phenomena").
narrative_ontology:topic_domain(medical_model_pathologization, "healthcare/epistemology/social_control").

domain_priors:requires_active_enforcement(medical_model_pathologization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_model_pathologization, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(medical_model_pathologization, psychiatric_medical_establishment).
narrative_ontology:constraint_beneficiary(medical_model_pathologization, diagnostic_nomenclature_authority).
narrative_ontology:constraint_victim(medical_model_pathologization, neurodivergent_populations).
narrative_ontology:constraint_victim(medical_model_pathologization, trauma_survivors).
narrative_ontology:constraint_victim(medical_model_pathologization, social_variance_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIAGNOSED INDIVIDUAL (SNARE) — Once labeled with a medical diagnosis, the individual bears the permanent extraction cost: stigma, medication side effects, exclusion from opportunities, internalized deficit framing. Exit is blocked by legal/medical gatekeeping (requires physician recertification), social barriers (diagnosis follows the individual), and identity lock (the diagnosis becomes integrated into self-concept). No coordination benefit; pure extraction experienced as inevitable natural pathology.
constraint_indexing:constraint_classification(medical_model_pathologization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PATIENT POPULATION AGGREGATE (TANGLED ROPE) — Modestly constrained exit (can challenge diagnosis but faces career/insurance consequences). Genuine coordination benefit: medical frameworks enable access to support, medication, workplace accommodation, research participation. Asymmetric extraction: diagnosis provides access to care BUT also locks individuals into disease identity and pharmaceutical dependency. Suppression high because dissent carries material costs (insurance denial, professional credibility loss).
constraint_indexing:constraint_classification(medical_model_pathologization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PSYCHIATRIC MEDICAL ESTABLISHMENT (ROPE) — Experiences the medical model as pure coordination: nosology organizes treatment, enables knowledge accumulation, justifies research funding, structures clinical practice. Institutional actors benefit from diagnostic frameworks (career paths, research authority, treatment protocols). Exit options abundant (shift paradigms, reframe, empirically validate). Experiences minimal suppression because institutional position is defended by professional gatekeeping and epistemic authority.
constraint_indexing:constraint_classification(medical_model_pathologization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (ROPE) — Diagnostic categories create markets: each diagnosis maps to treatable condition maps to billable intervention. Experiences constraint as enabling coordination (organizing research, justifying treatment, structuring clinical decision-making). High exit optionality (can develop new indications, pivot to new markets, lobby for expanded definitions). Suppression minimal; institutional power fully mobilized to defend and expand diagnostic framework.
constraint_indexing:constraint_classification(medical_model_pathologization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NEURODIVERSITY ADVOCACY MOVEMENT (TANGLED ROPE) — Organized but constrained exit (dissent from medical model carries credibility costs, funding threats, institutional marginalization). Mixed benefit: medical framework enables accommodation laws, research funding, clinical support; simultaneously locks individuals into deficit narrative and medicalizes natural variation. Suppression high: challenging psychiatric authority risks loss of research funding, clinical legitimacy, insurance coverage. Active coordination function (advocating for accommodations, resource allocation) coexists with extraction (confinement to disease identity).
constraint_indexing:constraint_classification(medical_model_pathologization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: DIAGNOSTIC REFORM MOVEMENT (SCAFFOLD) — Organized movement (ICD revision, DSM-5 critiques, neurodiversity frameworks) proposing sunset: shift from medical model pathologization to social/relational/difference models. Low effective extraction because organized agents have exit options and see a pathway to alternative frameworks. Theater ratio modest (actual descriptive work on criteria) compared to pure enforcement theater. Constraint is temporary — new nosologies and biopsychosocial models are maturing to replace pure medical pathologization.
constraint_indexing:constraint_classification(medical_model_pathologization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: DIAGNOSTIC MANUAL INSTITUTIONAL LEGACY (PITON) — The DSM/ICD themselves persist through institutional inertia: they are the default reference for insurance, legal accountability, research communication. But their actual descriptive power for complex human experiences is increasingly contested. The ritual of diagnostic classification (symptom checklist, duration criteria, impairment threshold) is largely performative — it sorts people into billing categories more reliably than it identifies underlying mechanisms. Theater ratio high (0.68) because diagnostic process maintains the appearance of scientific objectivity while actually enforcing social/economic categorization.
constraint_indexing:constraint_classification(medical_model_pathologization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From a civilizational view, some form of disease/disorder distinction appears inevitable: every medical system requires ways to distinguish normal variation from pathology requiring intervention. This perspective naturalizes the medical model as an inherent feature of human knowledge about health. However, this is a false summit: the boundaries of pathology are contingent on social/economic needs, not discovered facts. The engine's false summit detector will identify this as naturalization of a constructed institutional framework.
constraint_indexing:constraint_classification(medical_model_pathologization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_model_pathologization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_model_pathologization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_model_pathologization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(medical_model_pathologization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(medical_model_pathologization, TR),
    TR >= 0.70.

:- end_tests(medical_model_pathologization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, rising over interval. The medical model extracts value from diagnosed populations (career opportunity costs, pharmaceutical dependency, stigma internalization) while providing genuine coordination benefits (access to research, legal accommodation, treatment options). The rising trajectory reflects diagnostic expansion: DSM-III (1980) contained ~300 diagnoses; DSM-5 (2013) ~500; pharmaceutical marketing has continuously widened disease boundaries (ADHD, bipolar disorder, depression, anxiety, PTSD definitions have all expanded). The extraction increases because more human variation is pathologized, not because individual diagnosis has become more severe. Suppression (0.65): High. Multiple mechanisms suppress exit: legal barriers (diagnosis required for certain accommodations), financial barriers (insurance tied to diagnosis), social barriers (stigma and reduced credibility), epistemic barriers (medical authority gatekeeping). Identity-locked suppression is also significant — many diagnosed individuals internalize medical framing as identity truth rather than constructed label. Theater ratio (0.68): High and rising. Diagnostic assessment uses symptom checklists, duration thresholds, and impairment criteria that mimic scientific precision but actually perform categorical sorting for billing/legal purposes. The ritual of psychiatric diagnosis maintains appearance of objective clinical judgment while actually enforcing social/economic categorization. Theater has increased as diagnosis-driven pharma marketing has elaborated the theatrical scaffolding around diagnostic categories.
 *
 * PERSPECTIVAL GAP:
 *   Institutional psychiatry sees medical model as successful scientific framework (rope). Diagnosed individuals see it as permanent stigma (snare). Advocacy movements see it as flawed but necessary (tangled rope). Reform movements see it as replaceable (scaffold). The analytical observer risks seeing it as inevitable (mountain — false summit). This perspectival gap is not about disagreement on facts but about structural position: those with institutional power and exit options genuinely do experience the framework as enabling coordination; those with neither genuinely experience it as extraction. Both observations are structurally true. The gap reveals that framework legitimacy depends on which agent's experience is treated as ground truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from agent power, exit options, and beneficiary/victim relationship. Diagnosed individuals: powerless power + trapped exit + victim status → high d (0.92+) → high f(d) (1.38+) → maximum experienced extraction. Institutional beneficiaries: institutional power + arbitrage exit + beneficiary status → low d (0.05-0.15) → negative f(d) (-0.12 to 0.02) → negative/minimal experienced extraction (they benefit). Organized advocates: organized power + constrained exit + mixed victim-beneficiary status → moderate d (0.40-0.50) → moderate f(d) (0.40-0.65) → moderate experienced extraction. Reform movements: organized power + constrained exit but visible pathway → moderate d (0.35-0.45) → moderate f(d) (0.30-0.55) → moderate extraction with declining trajectory. The directionality computation reveals why institutional actors experience the constraint as coordination (low d) while victims experience it as extraction (high d) — the same structural mechanism produces opposite experienced effects based on power/exit position.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE DIAGNOSTIC: Medical model pathologization exhibits all three mandatrophy-resolving features: (1) Genuine coordination function — diagnostic frameworks organize knowledge, enable research, structure clinical practice, provide language for accessing accommodations. (2) Asymmetric extraction — benefits flow to institutional actors (psychiatry, pharma); costs fall on diagnosed populations (stigma, medication effects, identity confinement). (3) Active enforcement mechanism — diagnosis requires physician gatekeeping, insurance systems enforce diagnostic categories, legal systems tie accommodations to diagnosis, social enforcement through medical authority. The constraint cannot be classified as pure Rope (the extraction is substantial and asymmetric) or pure Snare (the coordination benefit is genuine — accommodations, research access, treatment options exist and matter). Tangled Rope correctly captures that this is neither coordination pretending to be extraction nor extraction pretending to be coordination, but actual simultaneous operation of both, with asymmetric distribution. The rising extractiveness over time reflects diagnostic expansion — more variation pathologized, more people trapped, but coordination benefits remain constant. The theater ratio tracks enforcement elaboration — more diagnostic theater deployed to justify expanded boundaries. Mandate (institutional enforcement) is high because medical system actively maintains diagnostic framework through gatekeeping, insurance ties, and professional authority. The constraint resolves mandatrophy by showing that legitimacy as coordination does not eliminate the extraction function — both are real, both are structural, both affect different agents differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pathology_boundary_contingency,
    'Is the line between pathology and normal variation discovered through science or constructed through social need?',
    'Historical analysis of diagnostic boundary shifts (e.g., homosexuality removal from DSM, ADHD expansion, autism spectrum broadening) correlating with social/economic drivers vs empirical discovery; cross-cultural comparison of pathology definitions; examination of whether diagnostic thresholds track to clinical outcomes or to insurance/pharmaceutical markets',
    'If discovered: medical model is natural/inevitable (mountain perspective justified). If constructed: boundaries are contingent institutional arrangements (mountain is false summit); classification shifts toward snare/tangled_rope across all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pathology_boundary_contingency, conceptual, 'Whether pathology boundaries are discovered or socially constructed').

omega_variable(
    neurodiversity_framework_viability,
    'Can neurodiversity/difference frameworks provide functional alternative to medical pathologization while maintaining access to medication, accommodation, and research resources?',
    'Pilot programs (schools, workplaces, clinical settings) using non-pathologizing frameworks; measurement of access equality (medication availability, accommodation rates, research funding) under alternative models; tracking of stigma/self-concept outcomes 5-10 years post-transition',
    'If viable: scaffold perspective confirmed — sunset is structurally real and achievable. If not viable: scaffold is aspirational; alternative model faces inertia and structural barriers; tangled_rope/piton perspectives dominant long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodiversity_framework_viability, empirical, 'Viability of non-medical difference frameworks as functional alternatives').

omega_variable(
    medication_necessity_independence,
    'How much pharmaceutical benefit is due to genuine mechanism vs. placebo, therapeutic attention, and diagnostic labeling effects?',
    'Meta-analysis of placebo-controlled trials; comparison of outcomes in double-blind vs. open-label conditions; tracking of medication efficacy when diagnosis is removed/reframed; assessment of whether pharmacological benefit persists when diagnosis-related stigma is controlled',
    'If mechanism-dominant: medical model provides genuine coordination (rope perspective strengthened). If placebo/attention-dominant: coordination benefit is largely theatrical (piton/scaffold perspectives; suppression through unnecessary medication labeled as treatment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medication_necessity_independence, empirical, 'Pharmacological benefit independence from diagnosis framing').

omega_variable(
    identity_lock_vs_structural_trap,
    'For trapped individuals, is the binding mechanism primarily identity-locked (internalized disease narrative) or structurally trapped (legal/medical gatekeeping, insurance dependency)?',
    'Longitudinal tracking of exit attempts; analysis of agent statements pre/post-diagnosis; measurement of exit barriers (legal, economic, social knowledge) vs. agent-reported perceived barriers; post-exit suppression trajectory (does suppression persist after escaping structural barriers?)',
    'If identity-locked: constraint is perceptually changeable even if structurally mobile (identity frame shift enables exit); treatment should address cognitive capture. If structurally trapped: exit requires institutional change, not personal reframing; policies must remove legal/financial barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Identity-lock vs. structural trapping mechanism in diagnosed individuals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_model_pathologization, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medpath_tr_t0, medical_model_pathologization, theater_ratio, 0, 0.45).
narrative_ontology:measurement(medpath_tr_t15, medical_model_pathologization, theater_ratio, 15, 0.58).
narrative_ontology:measurement(medpath_tr_t30, medical_model_pathologization, theater_ratio, 30, 0.68).
narrative_ontology:measurement(medpath_tr_t45, medical_model_pathologization, theater_ratio, 45, 0.62).

% Extraction over time
narrative_ontology:measurement(medpath_be_t0, medical_model_pathologization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(medpath_be_t15, medical_model_pathologization, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(medpath_be_t30, medical_model_pathologization, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(medpath_be_t45, medical_model_pathologization, base_extractiveness, 45, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_model_pathologization, resource_allocation).
narrative_ontology:affects_constraint(medical_model_pathologization, pharmaceutical_market_expansion).
narrative_ontology:affects_constraint(medical_model_pathologization, psychiatric_diagnostic_expansion).
narrative_ontology:affects_constraint(medical_model_pathologization, disability_identity_lock).
narrative_ontology:affects_constraint(medical_model_pathologization, mental_health_destigmatization).

% DUAL FORMULATION NOTE:
% Medical model pathologization decomposes into multiple structurally distinct constraints: (1) diagnostic authority gatekeeping (epistemic control), (2) pharmaceutical market creation (economic extraction), (3) identity lock through diagnosis (psychological capture), (4) legal/insurance coupling to diagnosis (structural dependency). Each has distinct ε and mechanisms. This story captures the integrated constraint system; downstream constraints track specific mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
