% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Hybrid Platform Worker Security Category
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_security_reading of the
 *   employment_boundary kernel. It treats the legal and social arrangement
 *   that classifies platform workers as a distinct 'third category'âneither
 *   full employees nor independent contractorsâas a standing constraint.
 *   The reading acknowledges partial protections (injury and medical
 *   coverage) as a genuine coordination function while identifying the
 *   institutionalized precarity (absent retirement, career development, and
 *   unemployment security) as asymmetric extraction. The moderate Îµ reflects
 *   this dual character. The claim is tangled_rope: a hybrid of coordination
 *   and extraction that requires active legal enforcement to maintain.
 *
 * KEY AGENTS:
 *   - platform_workers: Primary payer (powerless/constrained) â bear precarity and incomplete security despite partial protections.
 *   - platform_companies: Primary beneficiary (institutional/arbitrage) â capture cost avoidance and labor flexibility.
 *   - state_regulators: Agenda setter (institutional/constrained) â maintain the hybrid classification as a policy compromise.
 *   - worker_advocates: Observer (organized/analytical) â contest the adequacy of partial protections and advocate for full employment rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.58).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.5).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Hybrid Platform Worker Security Category").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '020b8875-ecc7-4af5-bddb-1156879079f6').
narrative_ontology:cs_kernel_codification('020b8875-ecc7-4af5-bddb-1156879079f6', formalized).
narrative_ontology:cs_authority_grounding('020b8875-ecc7-4af5-bddb-1156879079f6', lineage).
narrative_ontology:cs_interpretation_layer_present('020b8875-ecc7-4af5-bddb-1156879079f6').
narrative_ontology:cs_reading_relation('020b8875-ecc7-4af5-bddb-1156879079f6', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('020b8875-ecc7-4af5-bddb-1156879079f6', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('020b8875-ecc7-4af5-bddb-1156879079f6', foundational, valid_third_labor_category).
narrative_ontology:cs_axiom_status(valid_third_labor_category, holdable).
narrative_ontology:cs_axiom_grounding('020b8875-ecc7-4af5-bddb-1156879079f6', valid_third_labor_category, conventional).
narrative_ontology:cs_axiom('020b8875-ecc7-4af5-bddb-1156879079f6', foundational, platform_work_exceptionalism).
narrative_ontology:cs_axiom_status(platform_work_exceptionalism, holdable).
narrative_ontology:cs_axiom_grounding('020b8875-ecc7-4af5-bddb-1156879079f6', platform_work_exceptionalism, empirically_contingent).
narrative_ontology:cs_reference_frame('020b8875-ecc7-4af5-bddb-1156879079f6', tripartite_labor_market_framework).
narrative_ontology:cs_drift_state('020b8875-ecc7-4af5-bddb-1156879079f6', contemporary_platform_regulation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('020b8875-ecc7-4af5-bddb-1156879079f6', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform platform-mediated labor under a hybrid legal classification that provides basic injury and medical protections but withholds full employment benefits including retirement security, career development, and unemployment insurance. They receive partial safety-net coverage yet remain precarious, with limited ability to convert platform work into traditional employment or genuine independent contracting.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_workers, beneficiary).

% Operate digital labor platforms under a hybrid classification that mandates partial contributions for injury and medical coverage while avoiding full employment obligations such as pensions, severance, and career training. They capture labor cost savings and workforce flexibility that would be eroded under standard employment law.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Design and enforce the hybrid worker classification, mandating specific partial protections while deliberately excluding full employment coverage. They navigate political pressure from platform lobbies, worker advocacy, and fiscal constraints, treating the category as a compromise between labor market flexibility and social protection.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, state_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for full employment classification or comprehensive portable benefits, arguing that the hybrid category institutionalizes precarity by legitimizing incomplete protections. They contest the category in courts, legislatures, and public discourse, seeking to shift the boundary toward substantive employment rights.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, worker_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legally recognized labor category that grants platform workers basic protectionsâprincipally injury insurance and medical coverageâwithout requiring platforms to assume the full cost structure of traditional employment, thereby preventing a complete regulatory void.
% TRANSFER_FUNCTION: Transfers risk and long-term security costs from platform companies to workers: platforms avoid pension, severance, unemployment, and career-development obligations, while workers absorb precarity in exchange for immediate but partial protections.
% ABSENT_VOICES: Platform workers who would prefer either full contractor autonomy or full employment security are structurally collapsed into the hybrid compromise; their distinct preferences are not separately represented in the policy framework. International labor standard bodies that question the compatibility of hybrid categories with ILO conventions are also marginal to domestic policy design.
% DISAPPEARANCE_RATIONALE: If the hybrid classification vanished, platform workers would likely be pushed toward either full employment (triggering major cost restructuring, potential service contraction, and platform exit) or pure contractor status (stripping injury and medical protections). Platform business models, pricing, and worker livelihoods would reorganize around whichever binary pole dominated.
% FOUNDING_PROBLEM: The rapid scaling of algorithmically managed, task-based platform labor created a classification crisis: traditional employment tests based on direct supervision and fixed schedules did not fit gig work, while pure contractor status left workers without basic protections and exposed platforms to litigation and political backlash.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and some regulators attest the problem is addressed by the hybrid category. Worker advocates and independent labor economists attest the founding problem was misdiagnosed as classification ambiguity rather than power asymmetry; peer-reviewed labor studies and ILO reports from outside the benefiting party support the claim that the hybrid category perpetuates the underlying vulnerability.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the hybrid category delivers real but incomplete protections; the gap between what is provided and full employment security represents the extracted surplus. Suppression (0.50) reflects the legal and institutional enforcement needed to prevent classification collapse into either full employment or pure contracting. Theater ratio (0.40) captures the performative 'protection' narrative that legitimizes precarity. Accessibility collapse (0.50) is partial: alternatives (full employment, true independence) are visible but legally and economically difficult to access. Resistance (0.55) is moderate, reflecting ongoing worker organizing and litigation. Temporal measurements trace a gradual intensification as the hybrid category hardened from a gray-area practice into codified law with deepening enforcement infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   The platform_companies seat experiences the constraint as a cost-saving coordination mechanism that preserves labor supply and platform viability; the platform_workers seat experiences the same constraint as a ceiling on security and a floor on precarity. The state_regulator seat sees a pragmatic compromise, while worker_advocates see a legitimization of under-protection. These divergences are structurally encoded by directionality: platform_companies sit near the beneficiary pole, platform_workers near the target pole, and the divergence is the signal the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   platform_companies are declared beneficiaries because the hybrid classification allows them to avoid pension, severance, and career-training obligations they would face under full employment, directing their structural relationship toward the beneficiary end. platform_workers are declared victims because they bear the costs of precarity and long-term insecurity despite receiving partial protections, directing their structural relationship toward the target end. state_regulators administer the constraint but do not personally capture its extraction; their directionality is near symmetric. worker_advocates are analytical observers with no structural stake in the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâregulating platform labor that fell outside traditional categoriesâwas genuine. The constraint prevents mislabeling by preserving the coordination function (injury and medical protections) as a necessary condition for tangled_rope classification. A pure snare would lack these protections entirely; a pure rope would not institutionalize retirement and career insecurity. The moderate theater ratio (0.40) indicates some performative maintenance of the protection narrative, but the core coordination function is not fully atrophied, distinguishing it from piton. The R5 genealogy records a contested status: the underlying problem persists but the hybrid solution is disputed, preventing automatic mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_position,
    'Does the hybrid security reading represent a coherent and stable third category, or is it a temporary political compromise between formalist and substantive employment definitions?',
    'Comparative jurisdictional analysis over the next decade: if hybrid categories converge toward full employment, dissolve into contractor status, or are struck down by courts, the reading is unstable. If they stabilize with distinct legal doctrines and dedicated enforcement apparatus, the reading is coherent.',
    'If unstable, the constraint is better classified as a scaffold or piton rather than a tangled rope, and the extraction measure should be reinterpreted as transitional friction. If coherent, it persists as a genuine coordination-extraction hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_position, conceptual, 'Stability of the hybrid category as a distinct legal form versus a temporary compromise.').

omega_variable(
    protection_vs_precarity_tradeoff,
    'Do the basic protections provided under the hybrid category offset the extracted precarity, or do they function primarily as legitimacy cover for cost externalization?',
    'Longitudinal cohort studies comparing platform workers in hybrid regimes against matched full-employment and pure-contractor cohorts across income volatility, health outcomes, retirement savings, and career progression.',
    'If protections are net-positive, the extraction metric should be revised downward toward rope. If they serve primarily to legitimize externalization, the current tangled_rope framing is upheld and may shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_vs_precarity_tradeoff, empirical, 'Net welfare effect of hybrid protections versus extracted precarity.').

omega_variable(
    sibling_reading_boundary,
    'Can the hybrid security reading coexist indefinitely with formalist and substantive readings across jurisdictions, or will regulatory competition and judicial review force convergence toward a binary classification?',
    'Track high-court rulings, legislative harmonization efforts, and platform strategic reclassification across major economies over a ten-year window.',
    'Convergence toward one sibling reading would reclassify this constraint as a piton or scaffold. Continued pluralism supports the tangled_rope framing and confirms the third-category claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary, empirical, 'Long-term stability of the hybrid reading against its binary siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__hybrid_security_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__hybrid_security_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__hybrid_security_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__hybrid_security_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(empl_be_t4, employment_boundary__hybrid_security_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(empl_be_t8, employment_boundary__hybrid_security_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(empl_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(empl_be_t16, employment_boundary__hybrid_security_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(empl_be_t20, employment_boundary__hybrid_security_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(empl_su_t4, employment_boundary__hybrid_security_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(empl_su_t8, employment_boundary__hybrid_security_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(empl_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(empl_su_t16, employment_boundary__hybrid_security_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(empl_su_t20, employment_boundary__hybrid_security_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
