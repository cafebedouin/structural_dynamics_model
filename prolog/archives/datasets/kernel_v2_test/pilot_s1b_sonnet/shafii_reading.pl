% ============================================================================
% CONSTRAINT STORY: shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shafii_reading, []).

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
 *   constraint_id: shafii_reading
 *   human_readable: Shafi'i Usul al-Fiqh: Hadith Authentication as Gatekeeper
 *   domain: islamic_jurisprudence/legal_theory/methodological_frameworks
 *
 * SUMMARY:
 *   The Shafi'i reading of usul al-fiqh (Islamic legal theory) represents one
 *   of four canonical Sunni methodological frameworks for deriving law from
 *   sources. Systematized by Imam al-Shafi'i (d. 820 CE) in his Risala, this
 *   reading establishes a strict source hierarchy: Qur'an, then authenticated
 *   Sunnah (prophetic practice transmitted via hadith), then ijma (consensus,
 *   restricted to the Companions' generation), then qiyas (analogical
 *   reasoning, permitted only when no authenticated hadith addresses the
 *   issue). This hierarchy is not presented as one option among many but as
 *   the methodologically correct derivation of Islamic law. The constraint
 *   creates a permanent institutional role for hadith authentication
 *   specialists (muhaddithin) who control the gateway to legal authority: no
 *   legal ruling can bypass hadith authentication even when analogical
 *   reasoning would be more efficient or when local customary practice (urf)
 *   provides a workable solution. The Shafi'i reading solves a genuine
 *   coordination problem — legal authority was fragmenting across the early
 *   Islamic world as geographic expansion and the Prophet's death created
 *   uncertainty about which sources were authoritative — but embeds
 *   asymmetric extraction by granting gatekeeping power to transmission
 *   specialists over all other legal methodologies. This is ONE reading of a
 *   contested kernel: the other major madhahib (Hanafi, Maliki, Hanbali)
 *   accept the same four sources but disagree on their hierarchy,
 *   authentication stringency, and the scope of rationalist autonomy. The
 *   kernel (usul al-fiqh method) is stable; the readings contest its internal
 *   structure.
 *
 * KEY AGENTS:
 *   - Hadith Transmission Specialists (muhaddithin): Primary beneficiary (institutional/arbitrage) — gatekeepers who authenticate hadith chains; permanent demand for their certification
 *   - Shafi'i Institutional Hierarchy: Secondary beneficiary but also constrained (institutional/constrained) — madhhab structure coordinates legal production but must maintain authentication apparatus
 *   - Rationalist Jurists: Primary victim (moderate/constrained) — trained in qiyas but systematically subordinated to hadith specialists; extraction is substantial but not total (still have a role)
 *   - Local Customary Legal Traditions: Secondary victim (powerless/trapped) — regional urf delegitimized by authentication requirement; no pathway to authority without textual grounding
 *   - Reform-Oriented Jurists: Organized coalition (organized/mobile) — modernist scholars working to subordinate authentication to purposive reasoning (maqasid); see current hierarchy as transitional
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination function (authority grounding) and extraction mechanism (gatekeeping consolidation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shafii_reading, 0.48).
domain_priors:suppression_score(shafii_reading, 0.62).
domain_priors:theater_ratio(shafii_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shafii_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(shafii_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shafii_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shafii_reading, tangled_rope).
narrative_ontology:human_readable(shafii_reading, "Shafi'i Usul al-Fiqh: Hadith Authentication as Gatekeeper").
narrative_ontology:topic_domain(shafii_reading, "islamic_jurisprudence/legal_theory/methodological_frameworks").

domain_priors:requires_active_enforcement(shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shafii_reading, 'a14793d3-00f4-4550-aa81-ffd6f426959a').
narrative_ontology:cs_kernel_codification('a14793d3-00f4-4550-aa81-ffd6f426959a', formalized).
narrative_ontology:cs_authority_grounding('a14793d3-00f4-4550-aa81-ffd6f426959a', lineage).
narrative_ontology:cs_interpretation_layer_present('a14793d3-00f4-4550-aa81-ffd6f426959a').
narrative_ontology:cs_reading_relation('a14793d3-00f4-4550-aa81-ffd6f426959a', shafii_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('a14793d3-00f4-4550-aa81-ffd6f426959a', shafii_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('a14793d3-00f4-4550-aa81-ffd6f426959a', shafii_reading__hanbali_reading, influences).
narrative_ontology:cs_axiom('a14793d3-00f4-4550-aa81-ffd6f426959a', foundational, authenticated_hadith_supersedes_qiyas).
narrative_ontology:cs_axiom_status(authenticated_hadith_supersedes_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('a14793d3-00f4-4550-aa81-ffd6f426959a', authenticated_hadith_supersedes_qiyas, deontological).
narrative_ontology:cs_axiom('a14793d3-00f4-4550-aa81-ffd6f426959a', foundational, ijma_restricted_to_companions).
narrative_ontology:cs_axiom_status(ijma_restricted_to_companions, holdable).
narrative_ontology:cs_axiom_grounding('a14793d3-00f4-4550-aa81-ffd6f426959a', ijma_restricted_to_companions, deontological).
narrative_ontology:cs_axiom('a14793d3-00f4-4550-aa81-ffd6f426959a', secondary, isnad_authentication_prerequisite).
narrative_ontology:cs_axiom_status(isnad_authentication_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('a14793d3-00f4-4550-aa81-ffd6f426959a', isnad_authentication_prerequisite, empirically_contingent).
narrative_ontology:cs_reference_frame('a14793d3-00f4-4550-aa81-ffd6f426959a', prophetic_era_companions_authority).
narrative_ontology:cs_drift_state('a14793d3-00f4-4550-aa81-ffd6f426959a', contemporary_post_colonial, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a14793d3-00f4-4550-aa81-ffd6f426959a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(shafii_reading, shafii_institutional_hierarchy).
narrative_ontology:constraint_victim(shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(shafii_reading, local_customary_legal_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL CUSTOMARY TRADITION (SNARE) — Regional customary legal practices (urf) that lack hadith authentication are systematically delegitimized. Cannot exit the authentication requirement; no pathway to establish authority without hadith specialists' certification. Maximum extraction from those whose legal knowledge is embedded in practice rather than textual chains.
constraint_indexing:constraint_classification(shafii_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RATIONALIST JURIST (TANGLED ROPE) — Jurists trained in analogical reasoning (qiyas) face subordination to hadith authentication requirements but also benefit from the systematized usul al-fiqh framework that legitimizes their derivative role. Constrained by the hierarchy but not excluded entirely — extraction is substantial but not total.
constraint_indexing:constraint_classification(shafii_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HADITH SPECIALIST (ROPE) — Primary beneficiary. The authentication requirement creates permanent demand for specialists in isnad criticism and rijal evaluation. Experiences the constraint as pure coordination: systematizing source hierarchy solves the genuine problem of legal authority grounding. Net extraction flows toward this agent.
constraint_indexing:constraint_classification(shafii_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized modernist jurists see the hadith-first hierarchy as a transitional framework whose rigidity will necessarily relax as the historical distance from the Prophetic era increases and the authentication chains become unsustainable. They work within the system while anticipating its eventual transformation through maqasid al-sharia (objectives of Islamic law) methodologies that subordinate textual authentication to purposive reasoning.
constraint_indexing:constraint_classification(shafii_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: SHAFI'I INSTITUTION (TANGLED ROPE) — The madhhab institutional structure both coordinates legal production (genuine function) and extracts authority from subordinate jurists through certification requirements. Benefits from the system but also constrained by it — must maintain hadith authentication apparatus even when analogical reasoning would be more efficient.
constraint_indexing:constraint_classification(shafii_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the Shafi'i reading solves a genuine coordination problem (grounding legal authority in an era of geographic dispersion and proliferating opinions) while embedding asymmetric extraction (hadith specialists gain gatekeeping power over all legal derivation, subordinating rationalist and customary methods). The authentication requirement is neither purely natural (other readings exist) nor purely extractive (genuine epistemic benefits). This is the claimed type.
constraint_indexing:constraint_classification(shafii_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shafii_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shafii_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shafii_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Hadith specialists capture gatekeeping authority over all legal derivation; rationalist jurists and customary practitioners are systematically subordinated. But extraction is not maximal — the authentication requirement does solve a genuine epistemic problem (distinguishing reliable from unreliable transmission), and rationalist methods retain a subordinate but real role. The value reflects that the coordination and extraction functions are genuinely intertwined. Suppression (0.62): Moderate-high. Alternative methodologies (Hanafi rationalism, Maliki customary integration) exist but are institutionally marginalized in Shafi'i-dominated regions. Rationalist jurists cannot bypass authentication; local customary law cannot establish authority without hadith grounding. But suppression is not total — other madhahib survive, and geographic mobility allows some exit. Theater ratio (0.35): Moderate-low. The authentication apparatus is substantially functional, not performative. Isnad criticism and rijal evaluation are genuine scholarly practices with real epistemic content. Theater increases over time as some authentication debates become ritualized, but the core practice remains functional. Much lower than peer review theater in quantum materials (0.72) because hadith authentication, whatever its extractive effects, actually performs its stated function.
 *
 * PERSPECTIVAL GAP:
 *   Hadith specialists see pure coordination (Rope) — they are solving the legitimate problem of grounding legal authority in authenticated sources. Rationalist jurists see tangled rope — they benefit from the systematized framework but are subordinated by it. Local customary traditions see pure extraction (Snare) — their knowledge is delegitimized with no exit. The Shafi'i institution sees tangled rope from its own position — coordinates legal production but is also constrained by authentication maintenance requirements. The reform coalition sees scaffold — the current hierarchy is transitional, to be superseded by purposive reasoning. The analytical observer sees tangled rope as the structural reality — genuine coordination embedding asymmetric extraction. The gap is widest between the hadith specialists (beneficiaries experiencing coordination) and the customary traditions (victims experiencing pure extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (hadith_transmission_specialists, shafii_institutional_hierarchy) experience low directionality → low effective extraction → constraint appears as coordination or mild burden. Victims (rationalist_jurists, local_customary_legal_traditions) experience high directionality → high effective extraction → constraint appears as snare or heavy extraction. The rationalist_jurists are moderate power with constrained exit (can practice but only in subordinate role) → mid-range directionality → tangled rope classification (mixed experience). Local customary traditions are powerless with trapped exit (no pathway to authority) → maximum directionality → snare classification. The institutional hierarchy is institutional power with constrained exit (benefits but also bound by maintenance requirements) → mid-range directionality → tangled rope. Hadith specialists are institutional power with arbitrage exit (control the bottleneck and can move between madhahib) → minimum directionality → rope classification. The perspectival gap derives directly from these structural differences in experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This constraint instantiates the Shafi'i reading of the contested usul al-fiqh kernel. The mandate (grounding legal authority in authenticated prophetic practice) is live and has not outlived its function — the constraint operates as designed and serves its coordination purpose. Mandatrophy is not present. However, the reform coalition perspective sees the mandate as transitional — they argue that as historical distance from the prophetic era increases and authentication chains become unsustainable, the hierarchy must shift toward purposive reasoning (maqasid). This is a contested sunset claim (scaffold perspective) rather than resolved mandatrophy. The other sibling readings (Hanafi, Maliki, Hanbali) represent alternative framings of the same kernel, each with its own beneficiary/victim structure. The kernel itself (that Islamic law must be derived from authoritative sources) is not contested; the readings contest the source hierarchy and authentication stringency. This constraint resolves the mandatrophy analytically by showing that the classification depends on which reading you inhabit — there is no single 'correct' type, only perspectival measurements from different structural positions within and across readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_authentication_necessity,
    'Is hadith authentication a necessary epistemic requirement for legal derivation, or a constructed gatekeeping mechanism that benefits transmission specialists?',
    'Comparison of legal outcomes across madhahib: do Hanafi rulings (which permit greater rationalist autonomy) produce systematically different jurisprudence than Shafi''i rulings, or do they converge despite methodological differences? Historical analysis of authentication controversies where specialist consensus conflicted with widespread practice.',
    'If authentication is epistemically necessary: constraint is genuine coordination (Rope from more perspectives). If authentication standards are constructed: constraint is extraction mechanism (Snare from more perspectives). The kernel''s contested status is that different readings disagree on this necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authentication_necessity, conceptual, 'Whether hadith authentication is epistemically necessary or constructed gatekeeping').

omega_variable(
    ijma_restriction_motivation,
    'Does restricting ijma (consensus) to the Companions'' generation reflect epistemic privilege (closer to the source) or institutional consolidation (limiting who can claim consensus authority)?',
    'Textual analysis of al-Shafi''i''s Risala: what arguments does he give for temporal restriction? Historical context: what institutional conflicts was this restriction resolving? Comparison to other readings'' ijma concepts.',
    'If epistemic: the restriction is coordination (preserving authenticity). If institutional: the restriction is extraction (concentrating authority in historical figures whose positions must be interpreted by present specialists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ijma_restriction_motivation, conceptual, 'Whether ijma restriction reflects epistemic privilege or institutional consolidation').

omega_variable(
    qiyas_subordination_impact,
    'Does subordinating qiyas to authenticated hadith improve legal reliability or suppress rationalist legal autonomy?',
    'Longitudinal analysis of Shafi''i vs Hanafi jurisprudence: error rates, adaptability to novel situations, geographic portability. Survey of historical jurist career paths: did the qiyas subordination create barriers to entry for non-specialist jurists?',
    'If reliability improves: coordination function dominates. If autonomy suppressed without reliability gain: extraction function dominates. The perspectival gap between hadith specialists (who see coordination) and rationalist jurists (who see extraction) depends on which effect is empirically stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_subordination_impact, empirical, 'Whether qiyas subordination improves reliability or suppresses autonomy').

omega_variable(
    sibling_reading_commensurability,
    'Are the four canonical readings (Shafi''i, Hanafi, Maliki, Hanbali) genuinely different legal epistemologies, or variations on a shared framework with different emphasis?',
    'Cross-madhhab comparison of legal outcomes on contested issues. Analysis of inter-madhhab recognition: do practitioners treat sibling readings as legitimate alternatives or as errors to be corrected? Historical analysis of madhhab boundaries: were they permeable or fixed?',
    'If genuinely different epistemologies: reading_relations should include ''forecloses'' edges (incompatible frameworks). If variations on shared framework: reading_relations are all ''coexists_with'' (compatible alternatives). This omega addresses whether the kernel itself is stable or contested at the meta-level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_commensurability, conceptual, 'Whether sibling readings are incommensurable epistemologies or compatible variations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shafii_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shafii_tr_t0, shafii_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shafii_tr_t3, shafii_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(shafii_tr_t6, shafii_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(shafii_tr_t9, shafii_reading, theater_ratio, 9, 0.4).
narrative_ontology:measurement(shafii_tr_t10, shafii_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(shafii_be_t0, shafii_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shafii_be_t3, shafii_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(shafii_be_t6, shafii_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(shafii_be_t9, shafii_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(shafii_be_t10, shafii_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(shafii_su_t0, shafii_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(shafii_su_t3, shafii_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(shafii_su_t6, shafii_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(shafii_su_t9, shafii_reading, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(shafii_su_t10, shafii_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shafii_reading, identity_coordination).
narrative_ontology:affects_constraint(shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the usul_al_fiqh_method kernel family. The four madhahib readings are structurally distinct constraints (different ε values, different beneficiary/victim structures) linked by shared commitment to the kernel. The Shafi'i reading is the most hadith-centric; it influences the other readings by setting the authentication standard that they must respond to (either by matching its stringency or by justifying departures from it). The network edges represent institutional competition and methodological dialogue rather than causal determination — each reading is independently coherent but exists in relation to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
