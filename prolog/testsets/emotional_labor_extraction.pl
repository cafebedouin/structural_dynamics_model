% ============================================================================
% CONSTRAINT STORY: emotional_labor_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
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
 *   Emotional labor extraction in care and service work represents a
 *   paradigmatic tangled rope constraint where genuine coordination (patient
 *   care, therapeutic alliance, service quality) coexists structurally with
 *   systematic asymmetric extraction (emotional labor is unpaid beyond base
 *   wage, emotional depletion is not compensated, boundary violations are
 *   normalized). Care workers — nurses, therapists, teachers, elder care
 *   workers, customer service representatives — are required to produce
 *   emotional performance (attentiveness, empathy, patience, authenticity,
 *   care) as a non-negotiable job requirement, yet this labor is
 *   systematically undervalued and extracted. The constraint operates at
 *   multiple analytical levels: individual worker exploitation (snare from
 *   the powerless perspective), sectoral gender-wage gap (tangled rope at
 *   institutional scale), professional identity fusion (piton ideology
 *   naturalizing extraction), and policy reform opportunities (scaffold
 *   pathways). Extractiveness has risen from 0.35 to 0.62 over the interval
 *   (1970–1990 in relative time, corresponding to deskilling of care work and
 *   intensification of emotional labor demands), and suppression has
 *   correspondingly increased from 0.52 to 0.68 as care sector understaffing
 *   has intensified. Theater ratio has risen from 0.35 to 0.55, indicating
 *   that emotional labor's performative component has become more salient:
 *   care workers manage clients' emotions and organizational emotions while
 *   their own emotional depletion is invisible to the system. The constraint
 *   exhibits perspectival invariance across power positions — each agent sees
 *   legitimate coordination (patient safety, quality outcomes) AND sees
 *   extraction — making it a diagnostic exemplar for how tangled rope differs
 *   from pure rope or pure snare.
 *
 * KEY AGENTS:
 *   - Care workers (nurses, therapists, teachers, home care aides): Primary victims (powerless/trapped OR moderate/constrained depending on skill level and organizational context) — bear full cost of emotional depletion and experience identity fusion with the care role
 *   - Care organizations / employers (hospitals, schools, residential facilities): Primary beneficiaries (institutional/arbitrage) — extract emotional labor at below-market price via professional identity fusion and cultural scripts positioning emotional labor as inherent to the role, not separately compensable work
 *   - Clients / patients / customers: Secondary beneficiaries and co-exploiters (moderate/mobile OR powerful/arbitrage depending on client type) — benefit from worker emotional attunement while often unaware of the extraction mechanism; some clients also reinforce suppression (entitlement to worker emotional labor)
 *   - Care worker unions and professional associations: Organized agents (organized/constrained) — perceive constraint as hybrid, organize for explicit emotional labor compensation, boundary protection, and reduction of suppression mechanisms
 *   - Policy / regulatory reform coalition: Organized agents (organized/mobile) — seek to restructure extraction through mandatory staffing ratios, peer support time, boundary-violation accountability, emotional labor hazard pay (scaffold pathways)
 *   - Professional care ideology (cultural narrative of 'calling'): Institutional suppression mechanism (institutional/arbitrage) — naturalizes emotional labor extraction as inherent to care work; functions primarily as theater (narrows acceptable emotional expression, justifies low wages, prevents naming and compensating emotional labor as work)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emotional_labor_extraction, 0.62).
domain_priors:suppression_score(emotional_labor_extraction, 0.68).
domain_priors:theater_ratio(emotional_labor_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emotional_labor_extraction, extractiveness, 0.62).
narrative_ontology:constraint_metric(emotional_labor_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(emotional_labor_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emotional_labor_extraction, tangled_rope).
narrative_ontology:human_readable(emotional_labor_extraction, "Emotional Labor Extraction in Care and Service Work").
narrative_ontology:topic_domain(emotional_labor_extraction, "labor/social/interpersonal").

domain_priors:requires_active_enforcement(emotional_labor_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(emotional_labor_extraction, '47be198b-fb73-4fa8-89da-32386479cec5').
narrative_ontology:cs_kernel_codification('47be198b-fb73-4fa8-89da-32386479cec5', implicit).
narrative_ontology:cs_authority_grounding('47be198b-fb73-4fa8-89da-32386479cec5', extraction).
narrative_ontology:cs_interpretation_layer_present('47be198b-fb73-4fa8-89da-32386479cec5').
narrative_ontology:cs_created_at('47be198b-fb73-4fa8-89da-32386479cec5', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emotional_labor_extraction, employers_service_organizations).
narrative_ontology:constraint_beneficiary(emotional_labor_extraction, clients_customers).
narrative_ontology:constraint_victim(emotional_labor_extraction, care_workers_emotional_resource_depletion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXHAUSTED CARE WORKER (SNARE) — Trapped by economic dependency, credential lock-in, and the identity fusion of care work with selfhood. Cannot exit without losing livelihood and professional identity. Bears full cost of emotional depletion while suppression mechanisms (understaffing, low wages, limited break time) prevent recovery. Maximum extraction.
constraint_indexing:constraint_classification(emotional_labor_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SPECIALIZED CARE WORKER WITH PORTABLE SKILLS (TANGLED ROPE) — Constrained by geographic mobility, credential portability across regions, and retraining costs. Experiences genuine coordination function: therapeutic relationship, patient safety, quality of care require authentic emotional engagement. Also experiences asymmetric extraction: emotional labor is unpaid beyond base wage, emotional depletion is not compensated, boundary violations normalized. Mixed relationship to constraint.
constraint_indexing:constraint_classification(emotional_labor_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CARE ORGANIZATION / EMPLOYER (ROPE) — Genuine coordination function: patient trust, therapeutic alliance, safety outcomes depend on authentic emotional engagement by care workers. Employer benefits from this coordination and from extraction: emotional labor is obtained at below-market price via professional identity fusion ('real nurses care') and cultural scripts that position emotional labor as inherent to the role, not a separately compensable skill. Arbitrage options: can replace workers, shift emotional labor demands to remaining staff, externalize burnout costs.
constraint_indexing:constraint_classification(emotional_labor_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL CARE SECTOR (TANGLED ROPE) — At sector level, genuine coordination function (care requires emotional attunement) coexists with asymmetric extraction (emotional labor systematically underpaid, emotional depletion externalized to workers and families). Sector has mobility and alternatives but is locked into a low-wage equilibrium by path dependence and gender norms. Enforcement through cultural scripts ('caring is a calling') and credential specialization.
constraint_indexing:constraint_classification(emotional_labor_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CARE WORKER UNION / ORGANIZED RESISTANCE (TANGLED ROPE) — Organized agents with agency and voice but constrained by institutional power asymmetry and public-sector budget politics. Perceive the constraint as hybrid: coordination function genuine (quality care requires skilled emotional engagement), extraction asymmetric (emotional labor extraction is systematic and underpaid). Organizing strategy treats emotional labor as legitimate work deserving compensation and protection from boundary violation.
constraint_indexing:constraint_classification(emotional_labor_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POLICY / REGULATORY REFORM COALITION (SCAFFOLD) — Organized actors (professional associations, health advocates, labor-focused legislators) see emotional labor extraction as a temporary institutional failure addressable through regulation: mandatory staffing ratios, peer support time, boundary-violation accountability, emotional labor hazard pay. Reform creates explicit cost to employers for emotional labor extraction, restructures the extraction mechanism toward coordination. Scaffold classification: low effective extraction because reform is giving workers agency and alternative pathways; sunset assumes regulatory reform materially changes extraction dynamics over 10-15 year horizon.
constraint_indexing:constraint_classification(emotional_labor_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: PROFESSIONAL CARE IDEOLOGY (PITON) — The cultural narrative that 'real care workers are called to service' and 'emotional labor is not real work, it's love/duty' functions primarily as theater: it justifies low wages, normalizes boundary violations, and prevents emotional labor from being named and compensated as work. The ideology's functional role has degraded — it still suppresses wages but no longer secures genuine commitment or quality outcomes (burnout, turnover, and cynicism undermine care quality). Maintained through inertia and because no alternative legitimacy narrative has fully replaced it. Theater ratio reflects the gap between the care ideology's legitimating narrative and the actual emotionally depleted, cynical reality of care work at scale.
constraint_indexing:constraint_classification(emotional_labor_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some emotional labor is inherent to care work itself: authentic therapeutic relationships cannot be faked at scale without quality degradation. This view risks naturalizing the contingent institutional arrangement (unpaid emotional labor extraction) as an immutable feature of care work. The false summit detector will reveal that the 'inherent to care' framing obscures the structural choice to extract emotional labor without compensation, which is contingent on labor market power asymmetries and cultural gender norms, not on care's intrinsic nature.
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
    constraint_indexing:constraint_classification(emotional_labor_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.62): High-moderate, reflecting the core tension of the constraint. Care organizations genuinely require worker emotional engagement for therapeutic relationships and care quality — the coordination function is real. But the extraction is equally real: emotional labor is obtained at below-market price, emotional depletion is not compensated, and boundary violations are normalized ('real care workers don't clock out'). The 0.62 value reflects that both functions are structurally present. If the constraint were pure rope (no extraction), extractiveness would be ≤0.05. If pure snare (no coordination), extractiveness would be ≥0.46 without any coordination function. The 0.62 places this firmly in the tangled rope range (0.40 ≤ χ ≤ 0.90 effective extraction) where both are present and active. Suppression (0.68): High. Multiple mechanisms reinforce emotional labor extraction: (1) Economic dependency — care sector wages low, geographic mobility limited, retraining expensive; (2) Credential lock-in — professional credentials are care-sector-specific; (3) Identity fusion — care workers have internalized narrative that emotional labor is inherent selfhood; (4) Understaffing and time pressure — no capacity to maintain emotional boundaries; (5) Cultural scripts — 'real nurses care deeply' positions emotional labor as inherent trait, not work deserving compensation; (6) Institutional normalization of boundary violation — on-call requirements, expectation of availability beyond scheduled hours, guilt for setting limits. Rising suppression over the interval (0.52→0.68) reflects intensification of understaffing and emotional labor demands as care sector has been systematically underfunded. Theater ratio (0.55): Moderate-high. Emotional labor in care has both genuine and performative components. The genuine component: therapeutic authenticity is required for patient safety and outcomes. The performative component: care workers perform emotional attentiveness while managing their own emotional depletion, often masking burnout and cynicism to clients and supervisors. The rise from 0.35 to 0.55 reflects that the gap between performed emotional attunement and actual emotional state has widened as demands have intensified. Care workers are increasingly performing attentiveness they don't feel, managing clients' emotions while their own are depleted, and enacting 'caring' identities that are increasingly theater as underlying emotional resources are exhausted.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The exhausted care worker (powerless/trapped) sees pure snare: they cannot exit, bear full cost, and experience only extraction and suppression. The organization (institutional/arbitrage) sees rope: they experience the constraint as solving the coordination problem of obtaining patient trust and therapeutic alliance, and they arbitrage the extraction (can replace workers, shift demands). The union (organized/constrained) sees tangled rope: genuine coordination function (patients genuinely benefit from worker emotional engagement) AND systematic extraction (emotional labor is unpaid, extraction is predictable and structural). The reform coalition (organized/mobile) sees scaffold: the extraction is a temporary institutional failure addressable through regulation. The care ideology (institutional/arbitrage) sees its own narrative as natural law (mountain) — 'caring is inherent to humans, especially women' — but the false summit detector reveals this as naturalization of a contingent institutional arrangement. The analytical observer risks seeing mountain ('authentic care requires boundless emotional engagement') but the structural data contradicts this: care settings with explicit emotional labor boundaries produce equivalent or better outcomes, indicating that 'boundlessness is necessary' is ideological cover, not structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) tracks each agent's relationship to the extraction flow. Care workers classified as (powerless/trapped) derive d from victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42. Care workers classified as (moderate/constrained) derive d from victim status + constrained exit → d ≈ 0.65 → f(d) ≈ 1.00. Organizations classified as (institutional/arbitrage) derive d from beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 (they benefit; extraction runs toward them). Unions classified as (organized/constrained) derive d from victim status + organized power + constrained exit → d ≈ 0.55 → f(d) ≈ 0.75. The directionality divergence across perspectives directly reflects the perspectival gap: trapped workers experience maximum extraction (high d), while beneficiaries experience extraction as coordination and benefit (low d). The engine derives these automatically from beneficiary/victim declarations and exit options; the structural rationale is that beneficiaries have options to exit, replace workers, or shift demands, while victims have no such options.
 *
 * MANDATROPHY ANALYSIS:
 *   The emotional labor extraction constraint resolves the mandatrophy by demonstrating that tangled rope is the correct classification when BOTH a genuine coordination function AND systematic asymmetric extraction are structurally present and active. The mandatrophy question is: 'How can this be both coordination (rope) and extraction (snare) simultaneously?' The answer is that it IS both, structurally. Patient care genuinely requires emotional engagement (coordination function); care organizations systematically extract emotional labor without compensation (extraction function). The coordination is not a cover story for extraction, and the extraction is not incidental to coordination — they are both operative. The false summit in this constraint is the (analytical, mountain) perspective that naturalizes boundless emotional labor as inherent to care. The structural data shows this is false: care with explicit emotional labor boundaries produces equivalent outcomes, and the 'boundlessness required' narrative is ideological cover for extraction. The piton perspective correctly identifies the care ideology as degraded: it once functioned to organize care workers' commitment and intrinsic motivation; it now functions primarily to suppress wages and normalize boundary violations, while workers are increasingly cynical and depleted (the ideology no longer secures genuine commitment). The scaffold perspective is real: regulatory reform (mandatory ratios, peer support, boundary-violation accountability) has proven effective in reducing extraction and improving both worker wellbeing and care quality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emotional_labor_boundedness,
    'Is emotional labor that serves care quality fundamentally inseparable from the person, or can boundaries be established between emotional labor as work and the worker''s authentic self?',
    'Comparative study of care settings with explicit emotional labor boundaries (defined scope, protected off-hours, peer support) vs. those expecting boundless emotional availability. Measurement of burnout, care quality, and emotional depletion across these settings.',
    'If inseparable: emotional labor extraction may be structurally inevitable (moves toward mountain classification). If boundaries work: extraction is a design choice, not a natural law (confirms tangled_rope and snare classifications).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_labor_boundedness, empirical, 'Whether emotional labor in care can be bounded or must be boundless').

omega_variable(
    identity_fusion_mechanism,
    'Is the suppression of emotional labor compensation driven primarily by identity fusion (care workers have internalized the narrative that ''real care is selfless'') or by structural power asymmetries and labor market saturation?',
    'Qualitative analysis of care worker consciousness: coded interviews for identity-lock signals (inability to imagine themselves outside the role, framing emotional labor as inherent selfhood) vs. structural constraint signals (explicit recognition of power imbalance, belief that conditions could change with organizing but not with individual exit). Comparison with workers in similar constraints with weaker identity fusion.',
    'If identity fusion dominant: the constraint''s suppression is internalized and may persist despite wage increases (individuals carry suppression after exit). If structural power dominant: wage increases and union representation will materially reduce suppression. Affects directionality d values and interpretability of exit_options classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, conceptual, 'Mechanism of suppression: identity fusion vs. structural power asymmetry').

omega_variable(
    care_quality_emotional_authenticity_coupling,
    'How tightly is genuine care quality (patient outcomes, therapeutic effectiveness, safety) coupled to the worker''s actual emotional authenticity vs. performative emotional expression?',
    'Longitudinal outcome studies comparing care quality (patient satisfaction, clinical outcomes, safety events) in settings with high emotional authenticity demands vs. settings explicitly permitting performative emotional labor. Analysis of whether apparent emotional authenticity in high-burnout settings is genuinely authentic or skilled performance masking depletion.',
    'If tightly coupled (authenticity required): care quality genuinely depends on workers'' emotional availability, justifying some emotional labor burden but not its extraction without compensation (coordination needs enforcement, not exploitation). If loosely coupled (performance sufficient): extraction is purely predatory, no coordination function exists (reclassifies toward pure snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_quality_emotional_authenticity_coupling, empirical, 'Coupling between care quality and worker emotional authenticity').

omega_variable(
    gender_norm_naturalization,
    'To what extent is emotional labor extraction naturalized through gender norms (expectation that women will provide emotional labor for low pay as ''natural female nurture'') vs. through professional ideology specific to care work?',
    'Comparative analysis of emotional labor extraction in male-dominated vs. female-dominated care sectors (nurses vs. medical technicians, home care aides vs. facilities maintenance, early childhood educators vs. school custodians) controlling for skill requirements and outcomes sensitivity. Analysis of whether gender-mixed workplaces show different extraction patterns than gender-segregated ones.',
    'If gender norms dominant: the constraint may decompose into separate stories (gender norm constraint + care coordination constraint). If professional ideology dominant: the constraint is sector-specific and potentially addressable through sector-level reform. Affects network relationships and constraint family structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_norm_naturalization, conceptual, 'Role of gender norms vs. professional ideology in naturalizing emotional labor extraction').

omega_variable(
    therapeutic_relationship_authenticity_paradox,
    'Can a therapeutic relationship be ''authentic'' if the care worker is intentionally performing attentiveness to protect their own emotional resources? Does bounded emotional labor create an epistemological rupture that damages the coordination function?',
    'Patient and clinician self-report studies comparing therapeutic alliance in care settings with explicit emotional labor boundaries vs. boundless emotional availability settings. Analysis of whether patients detect or care about boundaries. Measurement of whether bounded settings produce different therapeutic outcomes.',
    'If patients detect boundaries as inauthenticity and outcomes suffer: coordination genuinely requires boundless emotional labor (extraction may be unavoidable). If patients don''t detect boundaries or outcomes are equivalent: the ''authenticity requires boundlessness'' narrative is ideological cover (extraction is purely extractive, not coordinating).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_relationship_authenticity_paradox, empirical, 'Whether bounded emotional labor can maintain therapeutic authenticity').

omega_variable(
    care_worker_organizational_exit_mobility,
    'What portion of care worker exit barriers are genuinely economic (transferable credentials, regional labor markets) vs. internalized suppression (belief that they cannot do other work, that care is their calling)?',
    'Longitudinal tracking of care workers who exit: analysis of post-exit employment, earnings, satisfaction, and reported reasons for exit vs. reasons they believed they couldn''t exit. Comparison with non-care workers facing similar economic constraints.',
    'If barriers largely economic: exit_options trapped is accurate; wage/union organizing should materially reduce extraction. If barriers largely internalized: exit_options should be identity_locked; individual wage increases won''t materially improve wellbeing (workers may reinvest improvements into boundless emotional labor); organizational culture change is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_worker_organizational_exit_mobility, empirical, 'Proportion of exit barriers that are economic vs. internalized suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emotional_labor_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emolabor_tr_t0, emotional_labor_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emolabor_tr_t10, emotional_labor_extraction, theater_ratio, 10, 0.45).
narrative_ontology:measurement(emolabor_tr_t20, emotional_labor_extraction, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(emolabor_be_t0, emotional_labor_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emolabor_be_t10, emotional_labor_extraction, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(emolabor_be_t20, emotional_labor_extraction, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(emolabor_su_t0, emotional_labor_extraction, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(emolabor_su_t10, emotional_labor_extraction, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(emolabor_su_t20, emotional_labor_extraction, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emotional_labor_extraction, attachment_coordination).
narrative_ontology:affects_constraint(emotional_labor_extraction, care_sector_gender_wage_gap).
narrative_ontology:affects_constraint(emotional_labor_extraction, therapeutic_boundary_violation).
narrative_ontology:affects_constraint(emotional_labor_extraction, care_worker_burnout_identity_fusion).
narrative_ontology:affects_constraint(emotional_labor_extraction, patient_emotional_autonomy).

% DUAL FORMULATION NOTE:
% Emotional labor extraction in care is the parent constraint. It links to downstream constraints: care sector gender wage gap (institutional-level manifestation of the same extraction mechanism), therapeutic boundary violation (the extraction dynamic at dyadic scale), care worker burnout driven by identity fusion (the suppression mechanism becoming pathological), and patient emotional autonomy (the constraint's impact on clients who may become dependent on workers' emotional availability). Each downstream constraint has its own ε value reflecting its specific structural dynamics; they share a common upstream extraction mechanism. The care ideology (piton perspective) affects all of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emotional_labor_extraction, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
