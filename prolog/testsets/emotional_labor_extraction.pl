% ============================================================================
% CONSTRAINT STORY: emotional_labor_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emotional_labor_extraction, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emotional_labor_extraction
 *   human_readable: Emotional Labor Extraction in Care and Service Work
 *   domain: labor/social/interpersonal
 *
 * SUMMARY:
 *   Emotional labor extraction in care and service work represents a hybrid
 *   constraint where genuine coordination coexists with systematic asymmetric
 *   extraction. Care workers (nurses, teachers, therapists, elder care
 *   workers, customer service representatives) are required to produce
 *   emotional performance — attentiveness, empathy, patience, authenticity —
 *   as a non-negotiable job requirement. This emotional labor produces
 *   genuine value: therapeutic alliance, client satisfaction, and care
 *   quality depend on emotional engagement. Yet the extraction is severe:
 *   workers bear the psychological and physical cost of emotional performance
 *   while organizations capture the market value. The constraint is
 *   maintained through multiple mechanisms: economic dependence (limited
 *   alternative employment), identity fusion (professional identity
 *   constituted through caring role), cultural mythology (patriarchal framing
 *   of care as natural feminine duty), and institutional theater (celebratory
 *   narratives of selfless care workers masking systemic burnout). The
 *   measurement interval shows increasing theater_ratio as the constraint
 *   ages — performative recognition of care worker 'sacrifices' rises as
 *   actual compensation and support decline, indicating piton-type
 *   degradation. The extractiveness rise from 0.42 to 0.58 reflects
 *   accumulating organizational demands without corresponding compensation
 *   increases.
 *
 * KEY AGENTS:
 *   - Emotional laborers (nurses, teachers, therapists): Primary victims (powerless/trapped and moderate/identity_locked) — bear psychological cost of emotional performance
 *   - Employing organizations (hospitals, schools, care facilities): Primary beneficiaries (powerful/mobile) — capture market value from emotional labor without proportional cost absorption
 *   - Care recipients (patients, students, elderly, disabled): Secondary beneficiaries and secondary victims (powerless/constrained) — benefit from emotional labor coordination but pay through system costs; often unable to exit dependency
 *   - Labor unions and advocacy coalitions: Organized agents (organized/constrained) — building sunset pathways through unionization and fair-wage frameworks
 *   - Patriarchal care ethic narrative: Institutional mechanism (institutional/arbitrage) — maintains constraint through cultural mythology and performative celebration
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing extraction as inherent to human care rather than institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emotional_labor_extraction, 0.58).
domain_priors:suppression_score(emotional_labor_extraction, 0.68).
domain_priors:theater_ratio(emotional_labor_extraction, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emotional_labor_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(emotional_labor_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(emotional_labor_extraction, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emotional_labor_extraction, tangled_rope).
narrative_ontology:human_readable(emotional_labor_extraction, "Emotional Labor Extraction in Care and Service Work").
narrative_ontology:topic_domain(emotional_labor_extraction, "labor/social/interpersonal").

domain_priors:requires_active_enforcement(emotional_labor_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emotional_labor_extraction, employing_organizations).
narrative_ontology:constraint_beneficiary(emotional_labor_extraction, care_recipients).
narrative_ontology:constraint_beneficiary(emotional_labor_extraction, service_consumers).
narrative_ontology:constraint_victim(emotional_labor_extraction, emotional_laborers).
narrative_ontology:constraint_victim(emotional_labor_extraction, worker_wellbeing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EMOTIONAL LABORER (SNARE) — Trapped by economic dependency; lacks realistic exit alternatives. Faces maximal extraction: organizational demands for emotional performance (smile, patience, attentiveness) are non-negotiable job requirements enforced through surveillance and termination threats. Burnout and emotional depletion are normalized as personal weakness rather than organizational extraction. No exit option; full directional target.
constraint_indexing:constraint_classification(emotional_labor_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE INTERNALIZED CARE PROVIDER (SNARE) — Structurally mobile (could seek other employment) but identity-fused with the caring role. Professional identity and self-concept are constituted through emotional labor: 'I am a nurse,' 'I am a teacher,' 'I am a therapist.' Exit would require becoming a different person. The binding is cognitive rather than material, but the extraction is total. Theater ratio high: performative emotional availability masks burnout and depletion.
constraint_indexing:constraint_classification(emotional_labor_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: THE EMPLOYING ORGANIZATION (TANGLED ROPE) — Genuine coordination function: emotional labor produces client satisfaction, trust, and retention. Organizations benefit from workers' emotional performance and derive market value from it. But the extraction is asymmetric: workers bear the cost of emotional depletion while organizations capture the revenue. Requirements for emotional performance are active, enforced through management metrics and performance reviews. Benefits accrue to the organization; costs to workers.
constraint_indexing:constraint_classification(emotional_labor_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CARE RECIPIENT (TANGLED ROPE) — Genuine coordination benefit: emotional labor from care providers produces actual care value and therapeutic benefit. The care recipient benefits from attentive, empathetic emotional performance. But the system extracts from emotional laborers to subsidize this care. Care recipients are often structurally disadvantaged (elderly, disabled, poor) and face constrained exits. Some coordination, significant extraction layered onto it.
constraint_indexing:constraint_classification(emotional_labor_extraction, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LABOR UNION / ADVOCACY COALITION (SCAFFOLD) — Organized agents see emotional labor extraction as a temporary institutional arrangement with a sunset path: unionization, emotional labor recognition, mandatory rest standards, mental health resources, and professionalization can reduce extraction asymmetry. Fair-care wage scales that account for emotional labor cost are building alternative frameworks. Suppression remains high (antiunion organizing, gig-work fragmentation) but exit pathways are visible. Sunset estimated at 15-25 years as labor consciousness and regulatory frameworks mature.
constraint_indexing:constraint_classification(emotional_labor_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CARE PROFESSION INSTITUTIONAL STRUCTURE (ROPE) — From the profession's collective perspective, emotional labor is genuine coordination: members of a profession coordinate through shared emotional standards, ethical frameworks, and relational norms. The profession maintains boundary and membership control through these standards. Extraction exists (surplus value capture by employers, credential gatekeeping) but the primary function is coordination of professional identity and standards.
constraint_indexing:constraint_classification(emotional_labor_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PATRIARCHAL CARE ETHIC (PITON) — The normative framing that emotional labor is a natural outpouring of care rather than paid work persists through institutional inertia and cultural mythology. The narrative of 'women's natural nurturing instinct' and 'calling to serve' is performative cover for extraction. The constraint is maintained by constant theatrical reinforcement (stories of selfless nurses, grateful patients) despite high burnout and turnover. Functional verification mechanisms have atrophied; the care ethic persists through cultural theater, not through genuine coordination.
constraint_indexing:constraint_classification(emotional_labor_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / HUMAN RELATIONAL UNIVERSALITY (MOUNTAIN) — From a universal/civilizational perspective, emotional labor is an irreducible feature of all human interaction and care: any relationship requires emotional attunement, and care for dependents necessarily involves emotional availability. This perspective risks naturalizing the extraction by framing emotional labor as inherent to human nature rather than as an institutional arrangement. The engine will identify this as a false summit — the extraction is contingent on how emotional labor is valued, compensated, and enforced by institutions, not on the fact that care requires emotional engagement.
constraint_indexing:constraint_classification(emotional_labor_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emotional_labor_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emotional_labor_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emotional_labor_extraction, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emotional_labor_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emotional_labor_extraction, TR),
    TR >= 0.70.

:- end_tests(emotional_labor_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant emotional and psychological resources from workers without proportional compensation. Organizations capture the value produced (client satisfaction, therapeutic outcomes, revenue) while workers bear the depletion cost. The extractiveness is not maximal (0.70+) because coordination is genuine — care quality does depend on emotional labor, and some workers do experience relational meaning from care work. But the asymmetry is severe enough to produce widespread burnout and emotional depletion across care sectors. Suppression (0.68): High. Barriers to resistance are structural and internalized: economic dependence on care employment (limited alternative high-wage work for workers without advanced credentials), identity fusion with care roles (exit would require identity deconstruction), antiunion organizing and gig-work fragmentation (weaken collective power), and cultural narratives that frame emotional labor as duty/calling rather than paid work. Suppression is not total (0.80+) because some organizational slack and worker solidarity create partial escape routes. Theater ratio (0.61): Moderate-high. The constraint is maintained partly through genuine coordination (emotional labor does produce care value) and partly through performative narrative (stories of noble, selfless workers celebrating care work while conditions deteriorate). As the interval progresses, theater rises and extractiveness remains high, indicating piton-type degradation — the performative narrative compensates for declining actual support.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. Emotional laborers with identity-locked binding see pure snare (extraction, no exit, binding is cognitive). Organizations see tangled rope (genuine coordination plus extraction). Care recipients see mixed tangled rope (benefit from care quality but pay for extraction). Unions see scaffold (sunset through unionization). The patriarchal care ethic narrative sees rope-like coordination (natural caring bonds). The analytical observer risks seeing mountain (care requires emotional labor inherently). These gaps reveal that the constraint's classification depends entirely on which agent's structural position you measure from. The power + exit_options combination determines whether the same structural phenomenon reads as snare, tangled_rope, scaffold, rope, piton, or mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from each perspective's power level, exit options, and structural relationship to the extraction flow. Emotional laborers with trapped or identity_locked exits experience maximum or near-maximum d, producing high f(d) and high experienced extractiveness. Organizations capturing value while offloading costs experience low d as beneficiaries, producing negative or low f(d). Care recipients face constrained exits and mixed positions (both beneficiaries of care coordination and victims of extraction costs), producing moderate d. Labor unions with organized power and constrained exits see moderate d with low f(d) because they have exit options and leverage. The piton perspective sees the care ethic narrative as institutional mechanism — neither high chi nor high extraction, but theatrical maintenance of a degraded constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that emotional labor extraction exhibits all six constraint types because the institutional arrangement itself is unstable. At the powerless/trapped position, it is pure snare (no exit, maximum extraction). At the institutional/beneficiary position, it is tangled rope (extraction layered onto genuine coordination). At the organized/unionized position, it becomes scaffold (temporary, with sunset pathway). The piton classification indicates the constraint's lifecycle: it began as tangled rope (genuine care coordination with asymmetric extraction) but is degrading into piton (performative celebration replacing actual support) as worker conditions worsen and organizational demands increase. The mountain perspective is a false summit — the analytical observer risks naturalizing extraction by framing care as inherently emotional and therefore necessarily involving emotional depletion. The corrective is to recognize that emotional engagement in care is inevitable, but emotional depletion is institutional, not inevitable. Fair compensation, bounded emotional labor, and worker voice could preserve the coordination function while eliminating the extraction. The constraint persists in its extractive form because institutions benefit from the asymmetry and cultural narratives (patriarchal care ethic, calling metaphors) prevent recognition of the extraction as such.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'Where is the boundary between identity-locked binding (cognitive fusion with care role) and constrained binding (material economic dependence)?',
    'Post-exit trajectory analysis: if emotional laborers continue performing care-work identity behaviors (emotional support to family, community caregiving) after leaving paid employment, the binding is identity-locked; if they reject emotional labor entirely, the binding was primarily constrained',
    'If identity-locked: workers carry suppression with them; exit without identity deconstruction produces psychological harm. If constrained: exit from employment terminates extraction; supports redesign of emotional labor as bounded professional practice rather than identity trait',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Boundary between identity-locked and constrained emotional labor binding').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'What proportion of measured suppression is structural (economic barriers, surveillance, threat of termination) versus internalized (guilt about insufficient emotional giving, shame about burnout, internalized care ethic)?',
    'Comparative analysis: suppression levels in unionized vs non-unionized settings; suppression changes when material barriers are removed; presence of guilt and shame language in worker narratives post-exit',
    'If highly internalized: workers carry suppression mechanisms even after economic barriers removed; therapeutic reframing necessary. If structural: removal of material barriers enables rapid behavior change; policy-level interventions sufficient',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Ratio of internalized to structural suppression in emotional labor extraction').

omega_variable(
    coordination_function_genuine_or_narrative,
    'Is the coordination function in emotional labor (client satisfaction, therapeutic alliance) genuinely dependent on extraction asymmetry, or could equivalent coordination be achieved with fair compensation and voluntary emotional engagement?',
    'Natural experiments: compare care outcomes in high-pay/low-extraction settings vs low-pay/high-extraction settings; measure client satisfaction and therapeutic outcomes in unionized vs non-unionized care work',
    'If genuine dependence: tangled_rope classification is correct. If narrative: the constraint is primarily snare masquerading as tangled_rope; requires relabeling to reflect actual extraction without coordination function',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_genuine_or_narrative, empirical, 'Whether emotional labor coordination requires extraction asymmetry').

omega_variable(
    sunset_pathway_realism,
    'Are the unionization and fair-wage pathways in the scaffold perspective realistic, or does the patriarchal care ethic''s institutional inertia prevent their implementation?',
    'Historical trend analysis: unionization rates in care sectors over past 20 years; wage evolution for emotional labor occupations; effectiveness of fair-care wage proposals in policy context',
    'If realistic: scaffold perspective is grounded; 15-25 year sunset is plausible exit path. If inertia dominates: scaffold is aspirational; the constraint degrades into piton rather than sunset',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_pathway_realism, empirical, 'Viability of unionization and fair-wage sunset pathways for emotional labor extraction').

omega_variable(
    care_outcome_degradation_rate,
    'How quickly does care outcome quality degrade when emotional laborers are extracted to burnout? Is there a threshold at which suppression itself becomes the limiting factor in coordination?',
    'Longitudinal tracking: patient safety outcomes, care quality metrics, therapeutic effectiveness across settings with different burnout rates; correlation between worker suppression levels and care failure modes',
    'If threshold is reached: organizations face coordination failure from their own extraction mechanism; financial incentives for extraction reduction emerge. If no clear threshold: extraction can continue without triggering self-correction',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_outcome_degradation_rate, empirical, 'Critical burnout threshold for care outcome degradation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emotional_labor_extraction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emolabor_tr_t0, emotional_labor_extraction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(emolabor_tr_t10, emotional_labor_extraction, theater_ratio, 10, 0.58).
narrative_ontology:measurement(emolabor_tr_t20, emotional_labor_extraction, theater_ratio, 20, 0.61).
narrative_ontology:measurement(emolabor_tr_t30, emotional_labor_extraction, theater_ratio, 30, 0.59).

% Extraction over time
narrative_ontology:measurement(emolabor_be_t0, emotional_labor_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(emolabor_be_t10, emotional_labor_extraction, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(emolabor_be_t20, emotional_labor_extraction, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(emolabor_be_t30, emotional_labor_extraction, base_extractiveness, 30, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emotional_labor_extraction, attachment_coordination).
narrative_ontology:boltzmann_floor_override(emotional_labor_extraction, 0.12).
narrative_ontology:affects_constraint(emotional_labor_extraction, gender_wage_gap_care_sectors).
narrative_ontology:affects_constraint(emotional_labor_extraction, healthcare_burnout_cascade).
narrative_ontology:affects_constraint(emotional_labor_extraction, patriarchal_family_labor_norms).

% DUAL FORMULATION NOTE:
% Emotional labor extraction in paid care work is downstream of unpaid care labor norms in families and communities (patriarchal_family_labor_norms). The constraint family decomposes into: (1) family-level unpaid care labor (ε=0.65, snare), (2) paid care work extraction (this story, ε=0.58, tangled_rope), and (3) gendered wage compression in care sectors (ε=0.52, tangled_rope). Each has distinct measurement basis but shares the structural mechanism of extracting uncompensated emotional labor from (predominantly) women. The family-level constraint has higher extractiveness because exit barriers are stronger (legal, economic, identity fusion); the paid work constraint is partially reformable through unionization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emotional_labor_extraction, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
