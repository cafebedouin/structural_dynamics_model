% ============================================================================
% CONSTRAINT STORY: hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanbali_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanbali_reading
 *   human_readable: Hanbali Literalist Textualism: Minimal Analogical Reasoning and Juristic Discretion Rejection
 *   domain: islamic_jurisprudence/legal_theory/institutional_pluralism
 *
 * SUMMARY:
 *   Literalist textualism in Hanbali jurisprudence enforces a methodological
 *   constraint: scholars are bound to explicit scriptural text (Qur'an and
 *   hadith) and prohibited from analogical reasoning (qiyas), juristic
 *   discretion (ra'y), or consensus innovations of later generations (ijma'
 *   al-mutakhayyirin). This constraint embodies a reading of how Islamic
 *   jurisprudence preserves its foundational authority. The Hanbali reading
 *   claims maximum textual fidelity by rejecting methodological flexibility;
 *   competing schools (Hanafi, Maliki, Shafi'i) claim equal fidelity while
 *   exercising analogical extension and discretion. The constraint exhibits a
 *   perspectival structure: beneficiaries experience it as coordination
 *   mechanism maintaining doctrinal integrity; victims experience it as
 *   suppression mechanism foreclosing legitimate adaptation; the
 *   institutional authority experiences it as a boundary-preserving
 *   mechanism; embedded scholars may experience it as identity-constitutive
 *   rather than externally enforced; modern states that adopt it face tangled
 *   extraction-coordination dynamics; the pedagogical apparatus that enforces
 *   it has degraded into theater as actual jurisprudence operates through
 *   circumvention; and the analytical observer risks naturalizing the choice
 *   as inherent to scriptural fidelity itself.
 *
 * KEY AGENTS:
 *   - Hanbali Institutional Authority: Primary beneficiary (institutional/arbitrage) — preserves doctrinal distinctiveness and jurisdictional authority through literalist purity claim
 *   - Juristic Innovation Agents: Primary victim (powerless/trapped) — cannot propose extensions without incurring bid'ah accusation; faces reputational and institutional sanctions
 *   - Analogical Extension Community: Primary victim (moderate/identity_locked) — scholars trained in Hanbali tradition unable to exercise qiyas without cognitive rupture; identity fused with literalist framework
 *   - Tradition-Embedded Judge: Secondary victim (moderate/identity_locked) — has power to rule but cannot legitimate discretionary judgment within the framework; experiences suppression as internalized restriction
 *   - Competing Jurisprudential Schools: Secondary actor (organized/constrained) — face legitimacy pressure from Hanbali literalism's purity claim but benefit from methodological differentiation
 *   - Post-Colonial State Legal System: Tertiary actor (powerful/constrained) — experiences tangled dynamics: pressure to claim textual fidelity AND govern contemporary complexity
 *   - Historical Pedagogical Apparatus: Institutional maintenance structure (institutional/mobile) — perpetuates literalist teaching formality despite actual juristic flexibility in practice (piton)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as logical necessity (false summit candidate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanbali_reading, 0.58).
domain_priors:suppression_score(hanbali_reading, 0.72).
domain_priors:theater_ratio(hanbali_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanbali_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hanbali_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hanbali_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanbali_reading, tangled_rope).
narrative_ontology:human_readable(hanbali_reading, "Hanbali Literalist Textualism: Minimal Analogical Reasoning and Juristic Discretion Rejection").
narrative_ontology:topic_domain(hanbali_reading, "islamic_jurisprudence/legal_theory/institutional_pluralism").

domain_priors:requires_active_enforcement(hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hanbali_reading, fixed_text).
narrative_ontology:cs_authority_grounding(hanbali_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(hanbali_reading).
narrative_ontology:cs_kernel_id(hanbali_reading, jurisprudential_method_kernel).
narrative_ontology:cs_reading_relation(hanbali_reading, hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation(hanbali_reading, maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation(hanbali_reading, shafii_reading, coexists_with).
narrative_ontology:cs_axiom(hanbali_reading, foundational, textual_literalism_preserves_fidelity).
narrative_ontology:cs_axiom_status(textual_literalism_preserves_fidelity, holdable).
narrative_ontology:cs_axiom_grounding(hanbali_reading, textual_literalism_preserves_fidelity, deontological).
narrative_ontology:cs_axiom(hanbali_reading, foundational, analogical_reasoning_corrupts_authority).
narrative_ontology:cs_axiom_status(analogical_reasoning_corrupts_authority, holdable).
narrative_ontology:cs_axiom_grounding(hanbali_reading, analogical_reasoning_corrupts_authority, empirically_contingent).
narrative_ontology:cs_reference_frame(hanbali_reading, quranic_literal_meaning_binding).
narrative_ontology:cs_drift_state(hanbali_reading, contemporary_legal_pluralism, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanbali_reading, hanbali_tradition_guardians).
narrative_ontology:constraint_beneficiary(hanbali_reading, scriptural_authority_preservationists).
narrative_ontology:constraint_victim(hanbali_reading, juristic_innovation_agents).
narrative_ontology:constraint_victim(hanbali_reading, analogical_extension_community).
narrative_ontology:constraint_victim(hanbali_reading, contextual_adaptation_scholars).
narrative_ontology:constraint_victim(hanbali_reading, legal_pluralism_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INNOVATION-SEEKING SCHOLAR (SNARE) — Trapped within the literalist framework; cannot propose analogical extensions (qiyas) or exercise juristic discretion (ra'y) without incurring accusation of innovation (bid'ah). Bears maximum suppression. No exit available within the tradition's enforcement structures.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRADITION-EMBEDDED JUDGE (SNARE) — Structurally capable of exercising discretion (educated, empowered to rule), but identity fused with Hanbali literalism. Cannot imagine legitimate judgment outside the framework. Experiences suppression as internalized restriction, not external barrier. Extraction runs through the cognitive lock rather than material constraint.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMPETING JURISPRUDENTIAL SCHOOL (TANGLED ROPE) — Schools like Hanafi, Maliki, Shafi'i face resource competition and legitimacy pressure from Hanbali literalism's claim to textual purity, but also benefit from methodological differentiation and institutional survival. Constrained by the need to maintain doctrinal coherence while competing for authority. Mixed extraction and coordination.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HANBALI INSTITUTIONAL AUTHORITY (ROPE) — Experiences the constraint as coordination mechanism for maintaining doctrinal integrity and institutional boundaries. Arbitrage exit: can leverage literalist purity claim for legitimacy in competition with other schools. Net beneficiary from the enforcement of textual literalism.
constraint_indexing:constraint_classification(hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: POST-COLONIAL STATE LEGAL SYSTEM (TANGLED ROPE) — Modern state adopting or accommodating Hanbali literalism faces simultaneous coordination demands (consistency with scriptural authority; institutional legitimacy) and extraction pressures (inability to adapt law to contemporary circumstances without juristic discretion). Constrained by need to claim both textual fidelity and governance capacity.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL PEDAGOGICAL APPARATUS (PITON) — The medieval madrasah teaching structures that enforced literalist training persist through institutional inertia even when substantive juristic activity has migrated to flexibility (fatwas, administrative law, judicial discretion in fact-finding). Theater ratio high: formal adherence to literalism maintained in written law while actual juristic activity operates through circumvention. Mobile exit option because modern scholars can choose alternative methodologies despite formal institutional attachment.
constraint_indexing:constraint_classification(hanbali_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW READING (MOUNTAIN) — From a civilizational analytical view, the literalist constraint appears to derive from an immutable logical principle: fidelity to foundational texts and rejection of interpretive innovation ARE inherent to preserving the integrity of a scriptural tradition. From this view, the suppression of ra'y and qiyas is not extraction but a natural consequence of the tradition's own axioms. Engine false-summit detection: this naturalizes what is actually a contingent institutional reading choice.
constraint_indexing:constraint_classification(hanbali_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanbali_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanbali_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanbali_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanbali_reading, TR),
    TR >= 0.70.

:- end_tests(hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint restricts the methodological toolset available to jurists — qiyas and ra'y are not merely permitted but are foregone. This is not violent suppression but genuine cognitive restriction. The value reflects that the restriction is substantial (removes major jurisprudential mechanisms) but not total (literal text itself permits interpretation; necessity doctrine provides escape routes). The increase over the 400-year interval (0.42 to 0.63) reflects institutional accumulation: as the tradition becomes more formalized and the penalty for deviation increases, the effective extraction rises. Suppression (0.72): High. Multiple layers: (1) formal institutional enforcement — accusation of bid'ah carries social and professional cost; (2) cognitive/identity enforcement — scholars trained entirely within literalism cannot imagine legitimate alternatives without experiencing identity rupture; (3) distributed social enforcement — communities that adopted literalism as primary identity now police the boundaries. The suppression is not total (scholars can still exercise discretion through necessity or reinterpretation) but is severe enough to choke off most extension work. Theater ratio (0.38): Moderate-low. Unlike purely theatrical constraints, Hanbali literalism has genuine functional content — it does coordinate doctrinal boundary maintenance and does preserve textual authority claims. But the theater component exists: modern judges formally claim literal adherence while actually exercising discretion in fact-finding, interpretation of 'literal' meaning, and application of necessity doctrine. The theater increases over time (0.25 to 0.38) as the gap between formal literalism and actual jurisprudential flexibility widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival variance driven by the structural position of the observer relative to the restriction on analogical reasoning and juristic discretion. The Hanbali institutional authority sees coordination (Rope) — literalism solves the problem of preserving doctrinal boundaries and textual authority. The innovation-seeking scholar sees extraction (Snare) — trapped without exit from the restriction. The identity-locked judge sees extraction (Snare) — cognitively fused with the framework, unable to exercise legitimate discretion. The competing school sees mixed dynamics (Tangled Rope) — facing pressure from literalism's purity claim but benefiting from methodological differentiation. The post-colonial state sees tangled extraction-coordination (Tangled Rope) — pressure to claim textual fidelity while governing complexity. The pedagogical apparatus sees degraded function (Piton) — formal literalism persists through institutional inertia while actual jurisprudence operates through discretion. The analytical observer at civilizational scope risks seeing natural law (Mountain) — textual fidelity necessarily requires literalism — a false summit. The engine's false-summit detection exposes that literalism is a contingent institutional choice, not a logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by the agent's structural position relative to this specific constraint. Beneficiaries (institutional authority, scriptural preservationists) have low d values — they benefit from the constraint; the restriction on analogical reasoning consolidates their authority. Victims (innovation agents, embedded scholars) have high d values — they bear the cost of restricted methodology; the suppression constrains their work. The identity_locked scholar occupies an intermediate position: structurally capable of exercising discretion (has education and authority) but d-value is high because the constraint binds them cognitively. The competing school (organized power with constrained exit) experiences moderate d because the constraint both pressures them (legitimacy competition) and enables them (methodological differentiation). The state system experiences tangled d because it simultaneously benefits from legitimacy claims and bears extraction costs from inability to adapt. The pedagogical apparatus has moved toward mobile exit as scholars exercise de facto discretion, lowering d for institutional actors who maintain the tradition as formality. The false-summit mountain reading emerges from the analytical view where d approaches 0.72 — the 'natural law' reading naturalizes institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint arises from the ambiguity between two readings of what the constraint coordinates. READING A (Coordination): Literalism preserves scriptural fidelity and doctrinal integrity — the constraint is a Rope or Scaffold, justified by its coordination function. READING B (Extraction): Literalism suppresses legitimate juristic innovation and adaptation — the constraint is a Snare or Tangled Rope, justified by its suppression function. The constraint cannot be both pure coordination and pure extraction simultaneously. The resolution is PERSPECTIVAL: the constraint IS a coordination mechanism from the institutional beneficiary's view (Rope), AND it IS an extraction mechanism from the innovation victim's view (Snare). Both perspectives are structurally valid. The mandatrophy resolves by noting that the tangled_rope classification captures BOTH coordination function (doctrinal boundary maintenance) AND extraction function (suppression of analogical reasoning). The coordination is real; the extraction is real. The constraint is not misclassified as one when it should be the other — it is genuinely hybrid. The false-summit mountain reading represents the theoretical attempt to dissolve the hybrid by claiming literalism is a natural law of scriptural interpretation, not a contingent institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literalism_vs_scriptural_intent,
    'Does literalist textualism best preserve scriptural intent, or does analogical reasoning (qiyas) better fulfill the Qur''anic mandate for contextual adaptation?',
    'Historical-textual analysis of Qur''anic passages on legal reasoning; comparison of literalist outcomes against documented intent of legislated principles (maqasid); empirical assessment of whether literalism or analogical flexibility produces juristic conclusions closer to articulated scriptural purposes',
    'If literalism preserves intent: Hanbali constraint is justified as coordination mechanism for fidelity. If analogical reasoning better fulfills intent: constraint is revealed as extraction mechanism that sacrifices coherence for rigidity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literalism_vs_scriptural_intent, empirical, 'Whether literalism or analogical reasoning better fulfills scriptural intent').

omega_variable(
    bid_ah_taxonomy_precision,
    'Is the category of forbidden innovation (bid''ah) precise enough to distinguish genuinely illegitimate innovation from legitimate methodological extension?',
    'Historical case analysis of rulings classified as bid''ah in Hanbali tradition vs. those made via qiyas or ra''y in other schools on identical fact patterns; documentation of explicit criteria for bid''ah classification',
    'If precise: constraint reduces to coordination mechanism preventing genuine doctrinal corruption. If vague: constraint functions as suppressible category used to exclude rival methodologies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bid_ah_taxonomy_precision, empirical, 'Precision of bid''ah category for distinguishing legitimate from illegitimate innovation').

omega_variable(
    contextual_necessity_vs_juristic_discretion,
    'When literalist application of text produces unjust or impossible outcomes in substantially novel contexts, does the Hanbali reading acknowledge any permissible mechanism (necessity doctrine, maqasid reinterpretation) that functionally overlaps with ra''y, or is discretion categorically foreclosed?',
    'Doctrinal analysis of Hanbali jurisprudence on darurah (necessity), istihsan (juristic preference), and maqasid-based reinterpretation; mapping of whether these mechanisms are deployed when literalism fails pragmatically',
    'If functional overlaps exist: suppression is lower than measured; constraint is tangled rope. If discretion categorically foreclosed: suppression confirmed; constraint is snare from innovation perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contextual_necessity_vs_juristic_discretion, empirical, 'Whether Hanbali doctrine permits functional juristic discretion via alternative mechanisms').

omega_variable(
    reading_vs_contingent_institutional_choice,
    'Is literalist textualism a necessary logical consequence of Qur''anic fidelity, or is it a contingent institutional choice that other readings (Hanafi, Maliki) also preserve fidelity through different methodological paths?',
    'Comparative jurisprudential analysis: assessment of whether Hanafi and Maliki readings demonstrate equal or superior scriptural fidelity while exercising analogical reasoning and juristic discretion; historical documentation of whether literalism was doctrinally necessitated or chosen for institutional differentiation',
    'If necessary: constraint is natural law (mountain). If contingent choice: constraint is revealed as institutional positioning (tangled rope) rather than logical necessity. Triggers false-summit engine logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_contingent_institutional_choice, conceptual, 'Whether literalism is logically necessary or institutionally contingent').

omega_variable(
    identity_locked_mechanism_in_embedded_scholars,
    'For scholars trained entirely within Hanbali literalism, is the restriction on ra''y and qiyas an internalized identity-fusion mechanism or a material enforcement barrier?',
    'Biographical analysis of scholars who transitioned from Hanbali literalism to other methodologies or back; documentation of whether the transition required cognitive reframing (identity rupture) or merely cost acceptance; comparison with scholars trained in pluralistic jurisprudential environments',
    'If identity-fused: suppression experienced as internal coherence requirement; classification biases toward snare. If material barrier: suppression derives from external enforcement; classification reflects institutional power. Affects directionality calculation for identity_locked perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism_in_embedded_scholars, empirical, 'Whether restriction on discretion is internalized identity-fusion or external enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanbali_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanb_tr_t0, hanbali_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanb_tr_t200, hanbali_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement(hanb_tr_t400, hanbali_reading, theater_ratio, 400, 0.38).

% Extraction over time
narrative_ontology:measurement(hanb_be_t0, hanbali_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hanb_be_t200, hanbali_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(hanb_be_t400, hanbali_reading, base_extractiveness, 400, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four constraint stories, one per living school. Each story instantiates a different reading of how Islamic jurisprudence preserves authority while adapting to novel circumstances. The ε values differ: Hanbali literalism emphasizes fidelity and produces higher suppression (0.72); Hanafi flexibility produces lower suppression (0.55); Maliki custom-integration produces moderate suppression (0.60); Shafi'i systematization produces moderate-low suppression (0.50). These are not the same constraint viewed from different angles — they are structurally distinct institutional arrangements with different empirical signatures. They are linked through network.affects_constraints because each reading's institutional success creates structural pressure on the others' legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanbali_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
