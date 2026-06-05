% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: IP Category Emergence: Synchronic-Diachronic Seam (Thinkability/First-Holding Independence Test)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   Intellectual property law operates at the intersection of two
 *   incompatible temporal frames: the synchronic frame (what is thinkable at
 *   a given moment in time) and the diachronic frame (who was first to occupy
 *   a category in historical sequence). This constraint models the structural
 *   tension between category emergence (a synchronic question: 'Is this
 *   category coherent and valid as a subject of intellectual property?') and
 *   first-holding (a diachronic question: 'Who claimed this category
 *   first?'). The constraint exhibits this tension because patent law
 *   requires applicants to prove BOTH that the category they are claiming is
 *   novel (has just emerged as a valid IP subject) AND that they were the
 *   first to occupy it. The coupling of these requirements creates a
 *   structural trap: a claimant cannot establish themselves as first within a
 *   category that does not yet exist legally, so the burden of proof on both
 *   gates falls simultaneously on the applicant. This manifests as a
 *   tangled_rope constraint: there is genuine coordination function (patent
 *   offices must examine novelty and assess priority), but there is also
 *   asymmetric extraction (the coupling concentrates interpretive authority
 *   in patent offices and appellate courts, who can adjudicate both gates
 *   simultaneously and thus retain control over both category definition and
 *   occupancy rights). The synchronic-diachronic seam is the specific reading
 *   of the IP category emergence kernel that tests whether these two temporal
 *   frames can be decoupled (suggesting the coupling is contingent
 *   institutional design) or must remain coupled (suggesting it reflects
 *   logical necessity).
 *
 * KEY AGENTS:
 *   - Novel Claim Originator: Primary victim (powerless/trapped) — cannot prove category emergence without simultaneously proving first occupancy; bears full burden of proof on coupled gates
 *   - Boundary-Case Litigant: Secondary victim (moderate/constrained) — faces resource barriers to complex litigation; also benefits from appellate clarity on category boundaries
 *   - IP Doctrine Stabilizers (courts, jurisprudential authorities): Primary beneficiary (institutional/arbitrage) — benefit from maintaining coupling because it concentrates interpretive authority over both gates simultaneously
 *   - Patent Examiners: Institutional actor (institutional/constrained) — constrained by precedent and statutory language; benefit from coupling by retaining joint adjudicative authority
 *   - Patent Harmonization Movements (TRIPS bodies, harmonization initiatives): Organized agents (organized/mobile) — can engineer statutory decoupling; see coupling as remediable coordination failure with sunset potential
 *   - Traditional Common-Law Doctrine: Institutional actor (institutional/arbitrage) — maintains coupling through precedent weight and case-law entrenchment despite statutory decoupling attempts (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating the coupling as a logical necessity when it may be a false summit naturalized by doctrinal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.58).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.48).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "IP Category Emergence: Synchronic-Diachronic Seam (Thinkability/First-Holding Independence Test)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, 'fc81f863-2455-41ca-893c-75df23189f95').
narrative_ontology:cs_kernel_codification('fc81f863-2455-41ca-893c-75df23189f95', formalized).
narrative_ontology:cs_authority_grounding('fc81f863-2455-41ca-893c-75df23189f95', lineage).
narrative_ontology:cs_interpretation_layer_present('fc81f863-2455-41ca-893c-75df23189f95').
narrative_ontology:cs_reading_relation('fc81f863-2455-41ca-893c-75df23189f95', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc81f863-2455-41ca-893c-75df23189f95', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('fc81f863-2455-41ca-893c-75df23189f95', foundational, category_emergence_occupancy_temporal_independence_testable).
narrative_ontology:cs_axiom_status(category_emergence_occupancy_temporal_independence_testable, holdable).
narrative_ontology:cs_axiom_grounding('fc81f863-2455-41ca-893c-75df23189f95', category_emergence_occupancy_temporal_independence_testable, empirically_contingent).
narrative_ontology:cs_axiom('fc81f863-2455-41ca-893c-75df23189f95', secondary, doctrinal_coupling_is_institutional_choice_not_logical_necessity).
narrative_ontology:cs_axiom_status(doctrinal_coupling_is_institutional_choice_not_logical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('fc81f863-2455-41ca-893c-75df23189f95', doctrinal_coupling_is_institutional_choice_not_logical_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('fc81f863-2455-41ca-893c-75df23189f95', common_law_property_bundle_coherence).
narrative_ontology:cs_drift_state('fc81f863-2455-41ca-893c-75df23189f95', contemporary_statutory_separation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fc81f863-2455-41ca-893c-75df23189f95', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, doctrinal_stabilizers).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, category_gatekeepers).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, novel_claim_originators).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, conceptual_boundary_exploiters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL CLAIM ORIGINATOR (SNARE) — Agent attempting to establish intellectual property rights over a newly thinkable category discovers that occupancy rights cannot be established independently of prior category emergence. Trapped by the requirement to prove both that the category itself is novel AND that they were the first to occupy it. No exit: cannot claim the category without simultaneously having to defend category emergence; cannot establish themselves as first-holder without the category already being juridically recognized. Maximum experienced extraction.
constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BOUNDARY-CASE LITIGANT (TANGLED ROPE) — Agent whose intellectual property claim falls near the category boundary experiences both coordination and extraction. The system coordinates definition of novel categories (genuine function — needs to happen) but extracts value by requiring proof of category emergence AND first occupancy simultaneously. Constrained by litigation costs and precedent uncertainty; also benefits from appellate clarity when courts finally adjudicate the boundary.
constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IP DOCTRINE STABILIZERS (ROPE) — Courts and doctrinal authorities benefit from maintaining the coupled category-emergence/first-holding requirement. The coupling creates predictable doctrine and allows courts to preserve institutional authority over both category definition and occupancy rights. Experiences the constraint as coordination (enabling doctrine stability and institutional coherence). Net beneficiary through doctrinal authority and predictable case outcomes.
constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PATENT HARMONIZATION MOVEMENTS (SCAFFOLD) — Organized actors (patent law harmonization bodies, TRIPS signatories, regional patent offices) see the synchronic-diachronic coupling as a temporary coordination failure remediable by statutory clarification. Explicit decoupling (e.g., 'novelty of category is independent from first-holding of application') creates exit paths. See sunset logic: international harmonization moving toward explicit separation of category-novelty gates from occupancy-priority gates.
constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL COMMON-LAW APPARATUS (PITON) — Historical IP doctrine treated thinkability (category emergence) and first-holding as inseparable because occupancy law evolved from property law, where the bundle was structurally coherent. But modern statutory IP separates examination (category novelty) from priority (first filing). The coupling persists in doctrine through institutional inertia and precedent weight despite the statutory apparatus rendering it partly decorative. Theater ratio high (0.65+) because courts continue to intertwine category-emergence arguments in first-holding disputes even when statutory provisions could address them separately.
constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the coupling of thinkability and first-holding appears to be a logical necessity: a claim cannot be 'first' within a category that does not yet exist conceptually, so the emergence of the category and the occupation of it must be temporally/logically coupled. This perspective treats the coupling as a necessary feature of how property rights work. However, the structural data contradicts the mountain gate — statutory patent law can and does separate novelty gates from priority gates, revealing the coupling as contingent institutional design, not logical necessity. False-summit candidate.
constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: PATENT EXAMINER INSTITUTION (TANGLED ROPE) — Patent offices occupy a structurally ambiguous position. They coordinate the examination process (genuine function — need to assess novelty and nonobviousness) but extract value by requiring applicants to prove category emergence and first occupancy in synchronized documentary form. The examiner is constrained by existing precedent and office procedures; also benefits from the coupling because it concentrates interpretive authority (the examiner adjudicates both gates simultaneously). Different directionality than perspective 3 (doctrinal stabilizers) because the examiner's agency is more constrained by statute and prior art databases.
constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ip_category_emergence__synchronic_diachronic_seam, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, TR),
    TR >= 0.70.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The coupling creates genuine coordination function (patent offices must examine novelty and priority) but also extracts value by requiring simultaneous proof on both gates, concentrating interpretive authority in courts and examiners. The coupling is not as severe as pure extraction (0.72+) because statutory patent law does attempt to separate novelty and priority gates; the coupling is maintained partly by doctrinal entrenchment rather than purely by statutory requirement. Suppression (0.48): Moderate. Significant barriers to challenging the coupling include precedent weight, litigation costs, and the asymmetry of burden of proof (applicants must prove both gates simultaneously). But suppression is not total — appellate courts can reverse precedent, and statutory amendment is possible. Theater ratio (0.65): Moderate-high. The coupling involves significant performative content: patent offices require applicants to frame category-emergence arguments and first-occupancy arguments in synchronized legal narratives even when these questions are logically independent. The theater increases over time (from 0.52 to 0.65) as patent doctrine becomes more sophisticated at intertwining category arguments with priority disputes, creating apparent logical coherence where statutory provisions could address the questions separately.
 *
 * PERSPECTIVAL GAP:
 *   The synchronic-diachronic seam produces maximal perspectival divergence. The powerless claimant sees a snare — trapped by the requirement to prove both gates simultaneously with no exit. The moderate boundary-litigant sees tangled_rope — genuine coordination mixed with extraction. The doctrinal beneficiaries see rope — the coupling enables institutional coherence and predictable doctrine. The patent harmonization movements see a scaffold with a sunset — statutory decoupling is possible and underway. The traditional common-law apparatus sees piton — the coupling is maintained through institutional inertia despite statutory provisions that could separate it. The analytical observer risks seeing mountain — the coupling appears to be a logical necessity (you cannot claim to be first in a category that does not exist), but this naturalizes a contingent institutional arrangement. The structural data suggests the coupling is a false summit: it benefits doctrinal stabilizers and can be decoupled by statute.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the coupling mechanism. Novel claimants are victims (high d → high χ) because they bear the burden of simultaneous proof. Doctrinal stabilizers are beneficiaries (low d → low/negative χ) because they retain interpretive authority. Patent examiners are constrained beneficiaries (moderate d) because they benefit from the coupling but are partially bound by statute. Patent harmonization movements are organized agents with exit paths (lower d) because statutory decoupling is achievable. The analytical observer derives d from the 'observer' canonical fallback (d=0.73) because they occupy no structural position in the coupling. The key diagnostic: beneficiaries maintain the coupling by treating it as natural law (mountain), while victims experience it as extraction (snare). The false summit signature fires when beneficiaries are identified and the mountain classification is challenged.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the six classifications reflect genuine perspectival differences in structural position, not measurement ambiguity. The powerless claimant (snare) and doctrinal beneficiary (rope) disagree on classification because they occupy different structural positions: the claimant bears the burden of proof; the beneficiary retains interpretive authority. The scaffold perspective shows that statutory decoupling is structurally possible—the constraint is not immutable, which undermines the mountain reading. The piton perspective shows that the coupling is maintained by institutional inertia despite statutory provisions that could separate it—the theater ratio shows increasing performative content as doctrine becomes more sophisticated at intertwining arguments. The false summit signature (mountain + beneficiaries identified) triggers reclassification toward tangled_rope. The mandatrophy resolves through structural analysis of power differentials and exit options, not through measurement re-definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synchronic_diachronic_independence_test,
    'Can a category emerge (become thinkable) at a different time than when it is first occupied (claimed), or are these events necessarily temporally synchronized in practice?',
    'Historical analysis of specific IP categories (e.g., business methods, biotechnology, software): identify historical moment of category emergence vs first successful claim; compare emergence date (when courts first recognized the category as valid) vs occupancy date (when a claim on the category succeeded). If dates differ systematically by > 5 years for >50% of studied categories, independence is demonstrated. If dates cluster within < 2 years, coupling is structural.',
    'If independent: the constraint is a contingent institutional coupling remediable by statutory separation (supports scaffold and piton readings, undermines mountain reading). If synchronous: the coupling reflects genuine logical/temporal necessity (supports mountain reading, undermines scaffold reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(synchronic_diachronic_independence_test, empirical, 'Empirical test for independence of category emergence from first-holding occupancy').

omega_variable(
    statutory_decoupling_feasibility,
    'Can patent statutes explicitly separate novelty examination (category emergence) from priority assessment (first occupancy) without creating legal inconsistency or reduced patent office efficiency?',
    'Analysis of existing statutory schemes that attempt decoupling (e.g., TRIPS article 27.1 on patent-eligible subject matter vs first-to-file priority rules). Documentation of whether separated examinations create conflicts, inconsistencies, or applicant harms.',
    'If feasible: decoupling is technically possible and the coupling is choice (supports tangled_rope/scaffold). If infeasible: the coupling may be structurally necessary despite appearing contingent (undermines scaffold, supports mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_decoupling_feasibility, empirical, 'Feasibility of statutory separation of novelty and priority gates').

omega_variable(
    doctrinal_entanglement_vs_statutory_clarity,
    'Is the coupling maintained primarily by appellate doctrine (case-law precedent treating category emergence and first-holding as inseparable) or by statutory language, and can doctrinal coupling persist even when statutes attempt separation?',
    'Comparison of patent statutes across jurisdictions on explicit language about category-novelty vs applicant-priority. Analysis of appellate decisions that maintain coupling despite statutory separation (e.g., courts re-entangling category arguments in priority disputes). Measure: proportion of recent patent cases that argue category emergence when the statutory claim can succeed on priority alone.',
    'If doctrinal entrenchment: the coupling is maintained by institutional inertia (piton/tangled_rope reading). If statutory requirement: the coupling is legislatively enforced (mountain or tangled_rope reading). Distinguishes remedies: doctrinal entrenchment is curable by case-law reversal; statutory coupling requires legislative change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_entanglement_vs_statutory_clarity, empirical, 'Source of coupling: statute vs doctrine').

omega_variable(
    false_summit_natural_law_candidate,
    'Is the coupling treated as a natural law of property/logic because it genuinely is necessary, or because doctrinal authority benefits from maintaining the coupling and treats it as natural to preserve institutional control?',
    'Identify beneficiaries of the coupling (judges, patent offices, doctrinal stabilizers who retain adjudicative authority) and compare their treatment of the coupling (natural law vs contingent choice) to jurisdictions where decoupling has been attempted. If beneficiaries block or reframe decoupling attempts as ''logically impossible,'' the coupling is a false summit. If decoupling occurs without legal catastrophe, the mountain classification is refuted.',
    'Determines whether the mountain perspective is a genuine natural law or a naturalized institutional arrangement (false summit). If false summit: triggers FSM signature override and reclassification toward tangled_rope/snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'False summit detection: is the coupling a natural law or a naturalized institutional arrangement?').

omega_variable(
    thinkability_definition_ambiguity,
    'What constitutes ''thinkability'' or ''category emergence'' for IP purposes: (a) logical conceivability, (b) prior non-IP art recognition, (c) first explicit legal claim, or (d) appellate validation of legal claim?',
    'Review patent office guidelines, case law, and doctrinal texts for definition of ''patentable subject matter'' and when a category is deemed to have ''emerged.'' Identify conflicting definitions across jurisdictions or time periods. If definition shifts, establish when it shifted and what triggered the shift.',
    'Definitional ambiguity allows courts to move the category-emergence date retrospectively to match first-holder preferences. If definition is stable: category emergence is determinable independently. If definition shifts: the coupling is manipulable (supports snare reading, shows extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_definition_ambiguity, conceptual, 'Definitional ambiguity in ''category emergence'' / ''thinkability''').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipcateg_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ipcateg_tr_t5, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ipcateg_tr_t10, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(ipcateg_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ipcateg_be_t5, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ipcateg_be_t10, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ipcateg_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ipcateg_su_t5, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 5, 0.43).
narrative_ontology:measurement(ipcateg_su_t10, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% The IP category emergence kernel decomposes into three structurally distinct constraints corresponding to three readings: (1) synchronic-diachronic seam (THIS constraint) tests whether category emergence and occupancy can vary independently; (2) thinkability reading focuses on whether 'thinkability' is a property of logical/conceptual space prior to legal recognition; (3) first-holding reading focuses on priority and occupancy prior to category definition. Each reading has its own ε and beneficiary/victim structure. The synchronic-diachronic seam is upstream of the other two because its resolution (whether the frames decouple) constrains what can be claimed in the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
