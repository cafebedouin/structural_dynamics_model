% ============================================================================
% CONSTRAINT STORY: institutional_knowledge_validation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_knowledge_validation, []).

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
 *   constraint_id: institutional_knowledge_validation
 *   human_readable: Institutional Knowledge Validation Mechanisms
 *   domain: epistemology/organizational_governance
 *
 * SUMMARY:
 *   Institutional knowledge validation creates a structural tension between
 *   the epistemic necessity of verification and the tendency of validation
 *   institutions to become extractive gatekeepers. Organizations must verify
 *   competence to function effectively, but credentialing institutions can
 *   capture this verification function and leverage it to extract rents,
 *   restrict access, and suppress informal knowledge transfer. This
 *   constraint exhibits all six DR types depending on observational position.
 *   From the perspective of undocumented practitioners, it is a snare—tacit
 *   knowledge is valuable but cannot be formalized without institutional
 *   credentials. From the perspective of smaller organizations, it is tangled
 *   rope—genuine coordination need coupled with asymmetric cost distribution.
 *   From the credentialing institution's perspective, it is pure coordination
 *   (rope). From the perspective of alternative validation movements, it is a
 *   temporary scaffold with a sunset clause—as skills-based hiring matures,
 *   traditional credentialism weakens. From the perspective of legacy
 *   credential systems, it is a piton—institutional inertia maintains
 *   performative validation theater despite declining predictive validity.
 *   The analytical observer risks naturalizing this as an immutable law
 *   (mountain) when the structural data reveals it as a contingent
 *   institutional arrangement. The measurement trajectory shows rising
 *   theater ratio (0.48→0.68) and rising extractiveness (0.32→0.52) over a
 *   15-year interval, indicating that the constraint is becoming increasingly
 *   performative while extracting more value—classic signs of institutional
 *   degradation and rent-seeking layering.
 *
 * KEY AGENTS:
 *   - Undocumented Practitioners: Primary victims (powerless/trapped) — possess valuable tacit knowledge but cannot professionalize without institutional credentials; cannot exit without abandoning accumulated expertise
 *   - Struggling Organizations: Secondary victims (moderate/constrained) — benefit from credential legitimacy but bear disproportionate validation costs and lose adaptive flexibility
 *   - Credentialing Institutions: Primary beneficiaries (institutional/arbitrage) — capture validation function and extract rents; have exit options (can shift standards or new institutions can emerge)
 *   - Competency-Based Alternative Movement: Organized agents (organized/constrained) — building parallel validation pathways with clear sunset logic; not yet fully mature but creating structural exit path
 *   - Legacy Credential System: Institutional piton (institutional/arbitrage) — traditional degree/certification mechanism; sees its own degradation (theater increasing, predictive validity declining) but persists through inertia
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangement as universal validation problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_knowledge_validation, 0.52).
domain_priors:suppression_score(institutional_knowledge_validation, 0.58).
domain_priors:theater_ratio(institutional_knowledge_validation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_knowledge_validation, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_knowledge_validation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(institutional_knowledge_validation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_knowledge_validation, tangled_rope).
narrative_ontology:human_readable(institutional_knowledge_validation, "Institutional Knowledge Validation Mechanisms").
narrative_ontology:topic_domain(institutional_knowledge_validation, "epistemology/organizational_governance").

domain_priors:requires_active_enforcement(institutional_knowledge_validation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_knowledge_validation, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(institutional_knowledge_validation, credentialed_experts).
narrative_ontology:constraint_victim(institutional_knowledge_validation, informal_knowledge_holders).
narrative_ontology:constraint_victim(institutional_knowledge_validation, organizational_memory).
narrative_ontology:constraint_victim(institutional_knowledge_validation, institutional_learning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDOCUMENTED PRACTITIONER (SNARE) — Tacit knowledge holders whose expertise cannot be formally validated. Trapped: cannot professionalize their knowledge without institutional credentials they cannot afford or access. Cannot exit without abandoning years of accumulated practice. Maximum extraction: institutional system captures value of their knowledge while denying them recognition or advancement.
constraint_indexing:constraint_classification(institutional_knowledge_validation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STRUGGLING ORGANIZATION (TANGLED ROPE) — Mid-sized firms benefit from institutional knowledge validation (legitimacy, reputation, access to credentialed talent) but bear significant costs (compliance overhead, process rigidity, loss of adaptive capacity). Mixed extraction: genuine coordination problem (need to validate competence) coupled with asymmetric extraction (certification costs fall disproportionately on smaller players).
constraint_indexing:constraint_classification(institutional_knowledge_validation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIALING INSTITUTION (ROPE) — Universities, professional associations, licensing boards. Experience the constraint as pure coordination: solving the legitimate problem of verifying competence. Net beneficiary but through arbitrage exit: can shift validation standards without losing institutional function. Low extraction because they have structural flexibility and exit options.
constraint_indexing:constraint_classification(institutional_knowledge_validation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETENCY-BASED ALTERNATIVE (SCAFFOLD) — Bootcamps, skills-based hiring, micro-credentials, work-sample portfolios. Organized agents building temporary alternative pathways that reduce validation theater. Low extraction because the movement has agency and sees a clear sunset: as skills-based hiring becomes mainstream (estimated 10-15 years), the credential monopoly weakens. Sunset clause: mature alternative validation systems reduce institutional gatekeeping power.
constraint_indexing:constraint_classification(institutional_knowledge_validation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CREDENTIAL SYSTEM (PITON) — Traditional degree/certification pathways persist through institutional inertia despite declining predictive validity for job performance. Theater ratio (0.68): much of the validation ritual is performative—thesis committees cannot assess real-world problem-solving, transcripts cannot measure tacit knowledge, credentials signal compliance rather than competence. The system sees its own degradation: employers increasingly supplement credentials with assessments; yet the system persists because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(institutional_knowledge_validation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal perspective, some validation mechanism is irreducible to human knowledge institutions: you cannot eliminate the verification problem in a division of labor (how do you know if someone actually knows what they claim?). This perspective sees institutional knowledge validation as a natural law—but the structural data reveals this as naturalization of contingent institutional arrangements. The universal verification problem exists, but institutional gatekeeping is one contingent solution.
constraint_indexing:constraint_classification(institutional_knowledge_validation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_knowledge_validation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_knowledge_validation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_knowledge_validation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_knowledge_validation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_knowledge_validation, TR),
    TR >= 0.70.

:- end_tests(institutional_knowledge_validation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. The constraint extracts through multiple mechanisms: credential cost barriers (tuition, certification fees), monopolistic control of validation standards, suppression of informal knowledge transfer pathways, and career gatekeeping. The rising trajectory (0.32→0.52) reflects increasing rent-seeking as institutions realize they can layer additional validation requirements without losing function. Suppression (0.58): Moderate-high. Significant barriers include cost of formal education/certification, time requirements, geographic/demographic access gaps, and institutional unwillingness to recognize alternative pathways. But suppression is not total—some alternative pathways exist (bootcamps, portfolios, work samples), and some organizations are moving toward skills-based hiring. Theater ratio (0.68): High and rising. Much of the institutional validation apparatus is performative: degree programs cannot comprehensively assess real-world problem-solving, certifications test compliance knowledge rather than adaptive capability, credentials often signal completion rather than competence. The rising trajectory (0.48→0.68) indicates that validation theater is increasing faster than actual validation efficacy—the ritual persists even as its predictive validity declines (classic piton signature).
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between beneficiary and victim perspectives. Credentialing institutions see the constraint as solving a legitimate problem (rope—pure coordination); undocumented practitioners see it as gatekeeping (snare—pure extraction). This gap reveals the constraint as primarily extractive rather than purely coordinative, but the coordination function is real—both perspectives are structurally accurate. The tangled rope classification at the analytical level (combining genuine coordination function with asymmetric extraction) reflects this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim status and exit options. Credentialing institutions benefit from validation monopoly and have arbitrage exit (can shift standards, can compete with new institutions); they derive low d, low experienced extraction, rope classification. Undocumented practitioners bear the cost (credential barriers) and have trapped exit (cannot professionalize without credentials); they derive high d, high experienced extraction, snare classification. Struggling organizations experience mixed benefits (legitimacy through credentials) and costs (compliance overhead); constrained exit (can transition toward skills-based hiring but only gradually) produces moderate d, moderate extraction, tangled rope classification. The competency-based alternative movement has organized agency and clear exit path (eventually displacing traditional validation); constrained exit but strategic agency produces low-moderate d, low extraction, scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Institutional knowledge validation resolves mandatrophy through recognition that the constraint serves genuine coordination function (verifying competence is necessary in division of labor) while enabling extraction (institutions capture monopolistic rents from validation). The constraint cannot be collapsed into 'pure coordination' (rope) because undocumented practitioners cannot exit the validation function—they must pay rents or abandon their expertise. It cannot be collapsed into 'pure extraction' (snare) because the coordination problem is real—some mechanism for verifying competence is structurally necessary. The tangled rope classification captures both aspects: the constraint solves a genuine coordination problem (rope function) while using institutional monopoly to extract asymmetric rents (snare function layered on top). The rising theater ratio (0.48→0.68) indicates that extraction is increasingly performative rather than functional—the validation ritual persists even as its actual verification capacity declines. This is the diagnostic signature of mandatrophy resolution: a constraint that combines real coordination function with increasing rentier theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_quantifiability,
    'Is tacit knowledge fundamentally unquantifiable, or does current validation methodology reflect institutional convenience rather than structural necessity?',
    'Comparison of work-sample validation predictiveness vs. credentialed hiring outcomes; analysis of tacit knowledge transfer in apprenticeship vs. formal education contexts',
    'If tacit knowledge is fundamentally unquantifiable: institutional validation has structural justification (mountain properties emerge). If quantifiable through alternative methods: the gatekeeping is contingent institutional extraction (snare properties confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_quantifiability, empirical, 'Whether tacit knowledge is fundamentally unquantifiable or amenable to alternative validation').

omega_variable(
    validation_cost_distribution,
    'Are validation costs (tuition, certification fees, compliance overhead) distributed as fair coordination expense or as extractive rent-seeking?',
    'Comparative analysis of validation costs across institutional vs. alternative pathways; correlation between validation cost and actual knowledge transfer; measurement of who benefits from cost barriers',
    'If fair coordination: suppression (0.58) represents legitimate complexity (rope or tangled rope). If extractive: suppression reflects intentional barrier-raising (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_cost_distribution, empirical, 'Cost distribution of validation mechanisms').

omega_variable(
    alternative_pathway_sustainability,
    'Can competency-based alternatives scale to provide validation for 90%+ of workforce without reverting to credentialist gatekeeping patterns?',
    'Longitudinal tracking of bootcamp graduate outcomes vs. traditional degree holders; measurement of employer acceptance saturation for alternative credentials; observation of whether alternative pathways develop their own gatekeeping hierarchies',
    'If sustainable at scale: scaffold classification confirmed—sunset is real (10-15 year transition). If reverting to gatekeeping: constraints cycle (snare → scaffold → piton → snare), no fundamental resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_pathway_sustainability, empirical, 'Scalability and sustainability of alternative validation pathways').

omega_variable(
    organizational_learning_trade_off,
    'Does institutional validation''s theater ratio (0.68) represent necessary quality control overhead or does it actively suppress the organizational learning that happens through informal knowledge transfer?',
    'Measurement of knowledge diffusion speed in organizations with high validation theater vs. low theater; correlation between documentation requirements and tacit knowledge loss; analysis of worker mobility patterns',
    'If necessary overhead: theater is coordination cost (rope/tangled rope properties). If suppressing learning: theater is extraction mechanism (snare properties—the system extracts adaptive capacity while providing validation appearance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_learning_trade_off, empirical, 'Whether validation theater overhead suppresses organizational learning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_knowledge_validation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ikv_tr_t0, institutional_knowledge_validation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ikv_tr_t5, institutional_knowledge_validation, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ikv_tr_t10, institutional_knowledge_validation, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ikv_tr_t15, institutional_knowledge_validation, theater_ratio, 15, 0.66).

% Extraction over time
narrative_ontology:measurement(ikv_be_t0, institutional_knowledge_validation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ikv_be_t5, institutional_knowledge_validation, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ikv_be_t10, institutional_knowledge_validation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ikv_be_t15, institutional_knowledge_validation, base_extractiveness, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_knowledge_validation, identity_coordination).
narrative_ontology:boltzmann_floor_override(institutional_knowledge_validation, 0.12).
narrative_ontology:affects_constraint(institutional_knowledge_validation, professional_licensing_gatekeeping).
narrative_ontology:affects_constraint(institutional_knowledge_validation, educational_access_inequality).
narrative_ontology:affects_constraint(institutional_knowledge_validation, organizational_knowledge_loss).

% DUAL FORMULATION NOTE:
% Institutional knowledge validation decomposes into distinct structural constraints: (1) The verification problem (genuine coordination need—how to verify competence in specialized fields), (2) Institutional gatekeeping (extractive monopoly over validation standards), (3) Tacit knowledge suppression (loss of informal knowledge transfer pathways due to formalization pressure). This story addresses the integrated constraint combining all three; decomposition into separate stories recommended for detailed analysis of each mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_knowledge_validation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
