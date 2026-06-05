% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Literalism: Textual Constraint on Juristic Innovation
 *   domain: islamic_jurisprudence/legal_methodology/institutional_pluralism
 *
 * SUMMARY:
 *   The Hanbali literalist reading of jurisprudential method represents the
 *   most restrictive pole in Islamic legal methodology across the classical
 *   schools (madhabs). This constraint institutionalizes textual conservatism
 *   through explicit rejection of juristic discretion (ra'y), analogical
 *   reasoning (qiyas), and consensus of post-Companion generations (ijma
 *   al-mutaakhkhirin). The reading creates a binary classification: textual
 *   positions derivable from the Quran and authenticated hadith are licit
 *   (halal); positions requiring analogical extension, juristic preference
 *   (istihsan), or reinterpretation through later consensus are illicit
 *   innovation (bid'ah). The structural consequence is the highest
 *   suppression of alternative interpretive methods among the four classical
 *   schools, accompanied by the broadest victim set — anyone seeking to adapt
 *   Islamic jurisprudence to novel circumstances faces doctrinal illegitimacy
 *   charges unless their position can be established through explicit textual
 *   derivation. The extractiveness value (0.58) reflects moderate but
 *   substantial asymmetry: the literalist framework benefits textual
 *   authority maintainers (low exit costs via arbitrage to canonical texts)
 *   while imposing adaptation costs on those seeking contextual flexibility
 *   (high exit costs via school switching or doctrinal reframing). The
 *   theater ratio (0.38) indicates relatively low performativity — Hanbali
 *   jurisprudential practice genuinely constrains reasoning methods, not
 *   merely simulating constraint through ritual. The constraint is one
 *   reading of the jurisprudential_method_kernel, whose kernel codification
 *   is 'distributed' (no single authoritative text defines 'correct'
 *   methodology) and authority grounding is 'practice' (the methodological
 *   commitments are sustained through actual jurisprudential application
 *   across generations).
 *
 * KEY AGENTS:
 *   - Hanbali Textual Orthodoxy: Primary beneficiary (institutional/arbitrage) — controls the canonical interpretation and expands authority through literalist fidelity claims
 *   - Literalist Adherent Communities: Secondary beneficiary (organized/constrained) — gain doctrinal clarity and reduced uncertainty; bear cost of adaptability loss
 *   - Subordinate Jurists (Muqallid): Primary victim (powerless/trapped) — cannot invoke ra'y or qiyas; bound to transmitted positions without exit option
 *   - Analogical Reasoning Practitioners: Secondary victim (moderate/constrained or identity_locked) — excluded from legitimate jurisprudential method; some identity-fused with non-Hanbali schools
 *   - Innovative Jurists: Tertiary victim (powerful/mobile but organized resistance) — face delegitimacy and institutional exclusion despite intellectual capacity
 *   - Reformist Collective: Organized agents (organized/mobile) — treat literalism as temporary institutional arrangement with sunset clause; building alternative interpretive pathways
 *   - State-Aligned Islamic Authority: Institutional actor (powerful/arbitrage) — invokes literalism for legitimacy while operating through hybrid reasoning; maintains performance theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable legal principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.58).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.72).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, snare).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Literalism: Textual Constraint on Juristic Innovation").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "islamic_jurisprudence/legal_methodology/institutional_pluralism").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '00fc0f15-4bf7-4362-a7e8-a4895addc2f1').
narrative_ontology:cs_kernel_codification('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', distributed).
narrative_ontology:cs_authority_grounding('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', practice).
narrative_ontology:cs_interpretation_layer_present('00fc0f15-4bf7-4362-a7e8-a4895addc2f1').
narrative_ontology:cs_reading_relation('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', foundational, textual_derivation_exhaustive).
narrative_ontology:cs_axiom_status(textual_derivation_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', textual_derivation_exhaustive, deontological).
narrative_ontology:cs_axiom('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', foundational, analogical_reasoning_illegitimate).
narrative_ontology:cs_axiom_status(analogical_reasoning_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', analogical_reasoning_illegitimate, deontological).
narrative_ontology:cs_axiom('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', secondary, classical_authority_supreme).
narrative_ontology:cs_axiom_status(classical_authority_supreme, holdable).
narrative_ontology:cs_axiom_grounding('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', classical_authority_supreme, conventional).
narrative_ontology:cs_reference_frame('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', textual_fidelity_covenant).
narrative_ontology:cs_drift_state('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', contemporary_global_islam, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('00fc0f15-4bf7-4362-a7e8-a4895addc2f1', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_textual_orthodoxy).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, literalist_adherent_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, analogical_reasoning_practitioners).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, innovative_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, contextual_adaptation_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE JURIST (SNARE) — Bound by literalist textualism with no exit option. Cannot invoke ra'y (juristic discretion), qiyas (analogical reasoning), or istihsan (juristic preference). Trapped within the text as interpreted by the founding authority. Suppression is maximal — the constraint explicitly forecloses alternative interpretive methods. Extraction runs toward textual orthodoxy maintainers, not toward the jurist.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL ISLAMIC COMMUNITY (TANGLED ROPE) — Constrained by interpretive rigidity but also benefits from the stability and clarity of literalist application. Community members gain certainty and reduced uncertainty about legal obligations, but lose adaptability to local circumstances. Exit is costly (requires adopting an alternative school) but not impossible. The constraint coordinates legal clarity while extracting adaptability.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEXTUAL ORTHODOXY ESTABLISHMENT (ROPE) — Primary beneficiary (institutional/arbitrage). The literalist constraint preserves institutional authority of those who control the canonical text interpretation. Arbitrage comes from the ability to switch between textual authorities or invoke rare-but-permitted qiyas without loss of standing. The constraint is experienced as coordination — binding the community to a shared interpretive method.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HANBALI SCHOOL JURIST (SNARE, identity_locked) — Structurally mobile (could study under Hanafi masters, adopt analogical methods) but identity-fused with the Hanbali literalist tradition. Professional identity, scholarly reputation, and community standing are constituted through literalist rigor. Exit would require not just learning new methods but becoming a different kind of scholar. Suppression is high; extraction runs toward textual authority through professional legitimacy capture.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORMIST COLLECTIVE (SCAFFOLD) — Organized actors (modern Islamic reformists, contextualist scholars) see the literalist constraint as a temporary institutional arrangement with a sunset clause: as classical texts accumulate interpretation layers and new circumstances require novel applications, even literalists invoke rare qiyas or admit maslaha (public interest) reasoning. The reformist view treats Hanbali literalism as a pedagogical device rather than an ontological limit. Exit is available through reinterpretation of canonical authorities.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: STATE-ALIGNED ISLAMIC AUTHORITY (PITON) — Invokes literalist textualism to maintain doctrinal stability while actually operating through qiyas and maslaha-based reasoning when state interests require it. The literalist rhetoric persists through institutional inertia and legitimacy claims, but the actual juristic method is hybrid. Theater ratio high — the public commitment to literalism masks pragmatic flexibility.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the tension between textual fidelity and interpretive flexibility is presented as an immutable feature of legal interpretation itself: any legal system faces an irreducible gap between fixed text and changing circumstances. The literalist method appears as a natural response to this lawlike tension. However, structural analysis reveals this as a false summit — the constraint's beneficiaries and suppression mechanisms are identifiable, making it a contingent institutional arrangement rather than a law of interpretation.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanbali_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, TR),
    TR >= 0.70.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Hanbali literalist constraint extracts methodological control away from individual jurists and concentrates it in the textual canon and classical authorities. Beneficiaries (textual orthodoxy maintainers) capture legitimacy authority; victims (innovators, contextual adapters) bear the cost of doctrinal delegitimacy. The value 0.58 reflects that this is not maximal extraction (0.70+) because historically some qiyas was permitted in rare instances, creating a partial escape valve. Over the measurement interval (0 to 6 units, likely representing centuries), extractiveness rises from 0.42 to 0.58 as literalist enforcement hardens and the school's institutional establishment grows, making alternative reasoning costlier. Suppression (0.72): High. The constraint explicitly forbids ra'y, minimizes qiyas, and rejects post-classical consensus. Alternatives are not merely discouraged but declared illegitimate (bid'ah). Suppression rises from 0.65 to 0.72 over the interval as doctrinal gatekeeping becomes institutionalized. Theater ratio (0.38): Relatively low. The literalist constraint is functionally active — interpretations are genuinely constrained, not merely performatively labeled. The theater exists in the small gap where actual Hanbali jurisprudence invokes rare qiyas while the explicit rhetoric denies it. Theater ratio is lower than the piton (institutional authority) perspective would experience.
 *
 * PERSPECTIVAL GAP:
 *   The literalist constraint generates maximal perspectival divergence. The subordinate jurist experiences Snare with zero exit options — complete methodological constraint. The regional community experiences Tangled Rope — clear legal guidance (coordination benefit) but lost adaptability (extraction cost). The textual orthodoxy establishment experiences Rope — the constraint is a coordination mechanism that binds the community to their interpretive authority. The Hanbali jurist experiences Snare with identity-lock — structurally mobile but identity-fused with literalist rigor. The reformist collective experiences Scaffold with sunset — treating literalism as a temporary institutional arrangement being superseded by interpretive innovation. The state authority experiences Piton — invoking literalism publicly while operating through hybrid methods privately. The analytical observer risks Mountain (natural law of legal interpretation) but structural analysis reveals a false summit with clear beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from the agent's power level, exit options, and structural position relative to extraction flow. Textual orthodoxy maintainers (institutional/arbitrage) have d ≈ 0.10–0.15 (low extraction toward them, high benefit extraction from method). Subordinate jurists (powerless/trapped) have d ≈ 0.95 (maximum extraction toward them, no exit). Moderate agents (identity_locked exit) have d ≈ 0.85–0.90 (high extraction; identity fusion prevents exercise of structural mobility). Reformist collectives (organized/mobile) have d ≈ 0.45–0.55 (moderate extraction; exit available through reinterpretation and coalition building). State-aligned authorities (powerful/arbitrage) have d ≈ 0.25–0.35 (low experienced extraction due to ability to invoke hidden mechanisms and shift between readings). The analytical observer at universal scope has d ≈ 0.72 (canonical fallback for analytical position) — a false summit case where natural law framing masks contingent institutional structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literalism_scope_ambiguity,
    'Does Hanbali literalism constrain interpretation of the Quran''s explicit (nass) meanings only, or does it also preclude reinterpretation of hadith authenticity classifications and transmission chains?',
    'Historical analysis of classical Hanbali jurisprudence: examination of whether later scholars revised hadith authenticity assessments or transmission chain evaluations within the school. Comparison with how other schools handled hadith criticism and authentication.',
    'If scope limited to Quranic nass: extractiveness drops to 0.35–0.40 (Tangled Rope likely). If scope includes hadith methodology: extractiveness increases to 0.65+ (Snare confirmed). The victim set expands or contracts accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literalism_scope_ambiguity, empirical, 'Whether literalism constrains Quranic interpretation alone or includes hadith authenticity determination').

omega_variable(
    contextual_qiyas_permissibility,
    'Do classical Hanbali texts actually permit qiyas in rare, specified circumstances, or is the ''rare permission'' a modern reinterpretation by contextualist reformists seeking to soften literalism?',
    'Textual analysis of Ibn Hanbal''s foundational positions vs. later Hanbali jurisprudents (Ibn Qayyim, Ibn Taymiyyah). Examination of explicit statements authorizing or forbidding qiyas under any conditions. Cross-reference with biographical accounts of how early Hanbali jurists resolved novel cases.',
    'If rare qiyas is historically permitted: the scaffold perspective is legitimate (sunset clause is real institutional trend). If modern invention: the scaffold is aspirational, not structural; extractiveness remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contextual_qiyas_permissibility, empirical, 'Whether Hanbali school permits rare analogical reasoning or forbids it absolutely').

omega_variable(
    consensus_layering_mechanism,
    'Can consensus (ijma) of later generations of Hanbali scholars override or reinterpret earlier textual positions, or does the constraint explicitly foreclose post-classical consensus?',
    'Analysis of Hanbali positions on ijma temporality: whether consensus after the Prophet and Companions has binding force, and whether later Hanbali ijma can revise earlier understandings. Historical tracking of which innovations were accepted via consensus and how they were framed.',
    'If later consensus permitted: a hidden reinterpretation mechanism exists (extractiveness 0.45–0.50, Tangled Rope likely). If absolutely forbidden: the victim set (innovation seekers) has no escape path (extractiveness 0.65+, Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_layering_mechanism, empirical, 'Whether post-classical Hanbali consensus can reinterpret or override earlier textual positions').

omega_variable(
    reading_contest_instability,
    'Is the Hanbali literalist reading a stable, internally coherent interpretive framework, or does it depend on unstated hybrid mechanisms (rare qiyas, maslaha, implicit consensus) that its explicit rhetoric denies?',
    'Comparison of explicit Hanbali statements about interpretive method vs. actual jurisprudential practice across centuries. Tracking of innovations (urban law, commerce, governance) that required reasoning beyond literal text. Analysis of how Hanbali scholars framed these innovations (denial, reinterpretation, cosmetic qiyas).',
    'If internally coherent: the reading is a genuine alternative to Hanafi flexibility (reading_relations = coexists_with). If dependent on hidden mechanisms: the reading is a performative constraint (theater_ratio ≥ 0.50, classification shifts toward Piton or Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_instability, conceptual, 'Whether Hanbali literalism is internally coherent or depends on hidden interpretive mechanisms').

omega_variable(
    kernels_of_contest,
    'What distinguishes the Hanbali reading''s conception of textual authority from the Shafi''i hierarchy''s (Quran > Hadith > Ijma > Qiyas)? Is the difference in the ordering, the rejection of lower tiers, or the interpretation of what ''literal'' means?',
    'Close textual analysis of foundational jurisprudential texts (Ibn Hanbal''s Musnad and doctrinal summaries; al-Shafi''i''s Risala). Identification of which tier(s) of the classical hierarchy the Hanbali reading permits or forbids. Clarification of whether ''literalism'' means ''no analogical extension'' or ''only explicit text'' or ''no juristic preference.''',
    'If difference is primarily in analogical reasoning scope: readings coexist through different methodological choices (coexists_with). If difference is in authority hierarchy fundamentally: readings may foreclose each other. Axiom grounding (empirically_contingent vs deontological) depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernels_of_contest, conceptual, 'What structural distinguishes Hanbali literalism from competing school readings of legal authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanbali_jm_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanbali_jm_tr_t3, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(hanbali_jm_tr_t6, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(hanbali_jm_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hanbali_jm_be_t3, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(hanbali_jm_be_t6, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hanbali_jm_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(hanbali_jm_su_t3, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(hanbali_jm_su_t6, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__legal_pluralism_coordination).

% DUAL FORMULATION NOTE:
% The Hanbali literalist reading is one constraint within the jurisprudential_method_kernel family. The kernel itself (the distributed, contested claim about valid methodology) is not a constraint — it is the background against which the four school readings instantiate distinct constraints. Each reading has its own ε value, beneficiary/victim structure, and classification profile. The kernel operates at the meta-level (institutional pluralism permits coexisting methodologies); the readings operate at the object level (this specific jurisprudential method with its specific extraction and suppression properties).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
