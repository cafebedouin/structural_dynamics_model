% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Textual Restriction and Hadith Authority
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The Hanbali school of Islamic jurisprudence (usul al-fiqh) grounds legal
 *   authority in strict textual adherence: the Qur'an and Sunnah (hadith)
 *   provide complete guidance, and innovation through rationalist legal
 *   reasoning (qiyas/analogical reasoning, ra'y/independent opinion) is
 *   forbidden. Even weak hadith (daif) takes precedence over the absence of
 *   textual evidence. This reading of jurisprudential method concentrates
 *   epistemic authority in hadith scholars and textual orthodoxy, creating
 *   asymmetric extraction: those bound by the method lose access to the
 *   cognitive tools their discipline normally employs (analogy, contextual
 *   reasoning, jurisprudential innovation), while hadith specialists and
 *   textual authorities gain gatekeeping power over legal interpretation. The
 *   constraint exhibits Tangled Rope characteristics because it genuinely
 *   provides coordination (a stable, widely-recognized legal framework) while
 *   simultaneously suppressing alternative reasoning pathways and
 *   concentrating benefit in a specific scholarly constituency. Over the
 *   interval (roughly 800-1400 CE, modeled as 0-400 years), the method's
 *   theater ratio rose as institutional inertia and state sponsorship
 *   (particularly under the Saudi state from the 20th century onward)
 *   maintained the method's authority even as pragmatic legal reasoning
 *   increasingly occurred implicitly beneath the surface of explicit textual
 *   discourse. The extractiveness trajectory shows accumulation: early
 *   implementation maintained a relative balance between coordination and
 *   restriction, but as new jurisprudential problems emerged (commercial law,
 *   state administration, novel social practices), the method's inability to
 *   formally authorize innovation created increasing friction. Hadith
 *   scholars responded by intensifying authentication discourse and defensive
 *   scholarship — raising the theater ratio — rather than loosening the
 *   method. The constraint is currently contested: reform movements advocate
 *   for methodological softening while institutional centers maintain textual
 *   purity through theatrical authority.
 *
 * KEY AGENTS:
 *   - Hadith Scholars (muhaddithun): Primary beneficiary (institutional/arbitrage) — method concentrates epistemic authority in their hands; they control textual authentication and legal interpretation
 *   - Subordinated Jurists: Primary victim (powerless/identity_locked) — bound by the method yet denied access to rational innovation tools; identity constituted through juridical authority that the method constrains
 *   - Local Judges and Pragmatic Authorities: Secondary victim (moderate/constrained) — must apply the method despite friction with local custom and novel circumstances; face career costs for deviation
 *   - Mujtahid Rationalist Tradition: Victim (powerful/mobile) — the method forecloses the rationalist school's core premise; can exit but loses standing within orthodox Islam
 *   - Contextual Reform Movement: Organized agents (organized/mobile) — contemporary scholars advocating for measured loosening; see the constraint as a temporary phase with a sunset pathway
 *   - Hanbali Institutional Establishment: Institutional actor (institutional/arbitrage) — maintains the method through state sponsorship and institutional inertia; benefits from authority concentration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the method as an immutable requirement of textual authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.52).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.68).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Jurisprudential Method: Textual Restriction and Hadith Authority").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, 'ef8635a5-9394-4ba0-bca8-3957f3e4d5fc').
narrative_ontology:cs_kernel_codification('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', fixed_text).
narrative_ontology:cs_authority_grounding('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', extraction).
narrative_ontology:cs_interpretation_layer_present('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc').
narrative_ontology:cs_reading_relation('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_axiom('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', foundational, textual_canon_completeness).
narrative_ontology:cs_axiom_status(textual_canon_completeness, holdable).
narrative_ontology:cs_axiom_grounding('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', textual_canon_completeness, deontological).
narrative_ontology:cs_axiom('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', foundational, rationalist_innovation_forecloses_authority).
narrative_ontology:cs_axiom_status(rationalist_innovation_forecloses_authority, holdable).
narrative_ontology:cs_axiom_grounding('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', rationalist_innovation_forecloses_authority, instrumental).
narrative_ontology:cs_reference_frame('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', divine_textual_completeness_and_interpretive_constraint).
narrative_ontology:cs_drift_state('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', contemporary_legal_pluralism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef8635a5-9394-4ba0-bca8-3957f3e4d5fc', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textual_orthodox_tradition).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, mujtahid_rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, local_custom_adapters).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, jurisprudential_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED JURIST (SNARE) — A jurist trained in the Hanbali method who perceives the constraint as severely restrictive. Their identity is constituted through juridical authority, yet the method forecloses independent reasoning (qiyas rejected, ra'y forbidden). Exit would require abandoning their professional identity as a jurist. Maximum experienced extraction: locked into a framework that denies them the cognitive tools of their discipline. The constraint operates through internalized epistemic restriction, not external prohibition.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRAGMATIC LOCAL JUDGE (TANGLED ROPE) — A judicial authority applying the method in a region where custom ('urf) or local practice creates friction with strict textual rules. Experiences genuine coordination function (the method provides stable, widely-recognized legal guidance) alongside significant extraction (must suppress local accommodation and deny the legitimacy of reasonable adaptation). Can theoretically exit by adopting a different school, but faces career cost and community resistance. Moderate extraction with real agency.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanbali_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: HADITH-SCHOLAR ESTABLISHMENT (ROPE) — Primary beneficiary. The method concentrates jurisprudential authority in hadith specialists (muhaddithun) and textual scholars. Benefits flow directly: enhanced epistemic authority, control over legal interpretation, protection against rationalist innovation. Experiences the constraint as pure coordination: transmitting and authenticating the textual canon is the coordination problem the method solves. No experienced extraction — the method was designed for and continues to serve this constituency. Low effective extraction relative to beneficiary status.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTEXTUAL REFORM MOVEMENT (SCAFFOLD) — Organized agents (contemporary Hanbali jurisprudents, Islamic legal modernists) who recognize that the strict method generates pathologies when applied to novel circumstances (modern state law, corporate contracts, digital rights). These actors advocate for methodological loosening: higher authentication thresholds for weak hadith, limited space for maslahah (public interest), measured ijtihad on narrow grounds. They see the constraint as a temporary phase — the ethical principle (textual fidelity) can be preserved while the restrictive implementation is softened. Sunset logic: as scholars develop 'refined Hanbali methodology,' the stark restriction fades without formally rejecting the tradition.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanbali_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL HANBALI ESTABLISHMENT (PITON) — Historical Saudi and contemporary Hanbali institutional centers maintain the strict method primarily through institutional inertia and state sponsorship rather than because it functions optimally. Theater ratio is high: ritual affirmation of textual purity, elaborate authentication discourse around weak hadith, defensive scholarship against rationalist critique. The actual jurisprudential work increasingly incorporates implicit maslahah and contextual reasoning that the method formally denies. The institution persists because it controls resources and institutional legitimacy, not because the method generates superior legal outcomes. Theatrical maintenance of orthodoxy over functional legal reasoning.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanbali_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal epistemic perspective, strict adherence to textual precedent might appear as an irreducible structural principle: any legal system grounded in a fixed canon must restrict unauthorized innovation to maintain coherence. Rationalist reasoning (ra'y) would dissolve the authority of the founding text. This perspective naturalizes the method as a logical necessity — the price of textual authority. However, the beneficiary structure contradicts the mountain classification: the method demonstrably benefits hadith scholars and textual orthodoxy. The engine will detect this as a false summit, revealing that what appears to be epistemic necessity is actually a defended institutional arrangement.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanbali_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usul_al_fiqh_method__hanbali_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usul_al_fiqh_method__hanbali_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, TR),
    TR >= 0.70.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The method provides genuine coordination benefits (stable legal framework, widely recognized authority) but concentrates those benefits asymmetrically in hadith scholars while denying other jurists access to innovation tools. The baseline extraction reflects this mixed structure: not pure extraction (there is real coordination), not pure coordination (there is real suppression of alternatives). The 200-year trajectory (0.35→0.52) reflects accumulation: early implementation was more flexible, but as pragmatic pressures mounted without formal authorization for innovation, the method's suppressive effect intensified. Suppression (0.68): High. The method suppresses alternative reasoning pathways through formal prohibition (qiyas forbidden, ra'y rejected), internalized epistemic closure (jurists trained to see rationalist innovation as illegitimate), and institutional enforcement (orthodox authorities enforce textual purity). The 200-year trajectory (0.60→0.68) shows intensification: early periods had more implicit flexibility, but institutional hardening and state sponsorship (particularly 20th century Saudi consolidation) increased suppression capacity. Theater ratio (0.58): Moderate-high. The method increasingly relies on performative discourse (elaborate authentication frameworks, defensive scholarship, ritual affirmation of textual purity) precisely because pragmatic legal reasoning occurs implicitly despite formal prohibition. The 200-year trajectory (0.42→0.58) reflects rising theater: early implementation had stronger alignment between method and practice, but as novel legal problems accumulated, the gap between what the method allows and what judges actually do widened. Theater increased to bridge the gap.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the hadith-scholar perspective (Rope) and the subordinated-jurist perspective (Snare) is maximal, reflecting that the same constraint benefits one constituency while extracting from another. This gap is the diagnostic signal for Tangled Rope classification: coordination that benefits one group (hadith scholars) while suppressing another (rationalist jurists, pragmatic judges, local adapters). The piton perspective reveals that the institutional maintenance of the method increasingly relies on theater rather than functional necessity — judges apply reasoning the method forbids, creating a gap between declared orthodoxy and actual practice. The mountain perspective risks naturalizing this as an immutable law of textual authority, but the beneficiary structure contradicts the mountain gates, triggering the false summit detector.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from beneficiary/victim declarations plus power level plus exit options. Hadith scholars as institutional beneficiaries with arbitrage exit: d ≈ 0.15 → f(d) ≈ -0.01 → negative chi, indicating they benefit. Subordinated jurists as powerless victims with identity-locked exit: d ≈ 0.90 → f(d) ≈ 1.28 → maximum chi, indicating maximum extraction. Local judges as moderate victims with constrained exit: d ≈ 0.68 → f(d) ≈ 1.05 → substantial chi. Reform movement as organized victims with mobile exit: d ≈ 0.48 → f(d) ≈ 0.60 → moderate chi. These structural relationships, combined with extractiveness (0.52) and scope modifiers, produce the perspectival classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by demonstrating that 'Is this pure coordination or pure extraction?' is a false dichotomy. The Hanbali method is structurally both: it coordinates legal interpretation (genuine benefit) while asymmetrically concentrating that benefit in hadith scholars and suppressing alternative reasoning paths (genuine extraction). The Tangled Rope classification holds because all three Tangled Rope gates fire: (1) beneficiaries exist (hadith scholars), (2) victims exist (rationalist jurists, pragmatic judges), (3) active enforcement is required (the method depends on institutional maintenance and epistemic gatekeeping). The mandatrophy is resolved not by choosing coordination or extraction, but by recognizing that the method contains both in an asymmetric distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weak_hadith_authentication_standard,
    'At what threshold of hadith weakness does the method''s claim that ''dispositive hadith'' trumps absence of stronger evidence become internally contradictory?',
    'Systematic analysis of accepted weak hadith in Hanbali jurisprudence; comparison with rejected weak hadith; identification of consistency rules for authentication thresholds',
    'If internal contradictions exist: the ''textual restrictiveness'' is less absolute than the method claims, and rationalist innovation (ra''y) is covertly present. Method shifts toward Tangled Rope from multiple perspectives. If coherent: the method''s empirical authority base is narrower than claimed, and extraction concentrates on maintaining canon integrity rather than serving justice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(weak_hadith_authentication_standard, empirical, 'Consistency of weak hadith authentication thresholds').

omega_variable(
    qiyas_implicit_reasoning,
    'Does the prohibition on explicit qiyas (analogical reasoning) actually prevent implicit analogical reasoning disguised as textual exegesis or contextual hadith application?',
    'Comparative analysis of Hanbali jurisprudential reasoning in novel cases vs Maliki/Shafi''i explicit qiyas; identification of implicit analogical structure in supposedly textual decisions',
    'If qiyas is covertly present: the method is less restrictive than advertised, forecloses less, influences rather than forecloses sibling schools. Extraction value shifts because the suppression mechanism (denial of innovation tools) is partially illusory. If truly absent: the method enforces genuine cognitive restriction, and extraction through methodological foreclosure is structurally real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qiyas_implicit_reasoning, empirical, 'Whether qiyas reasoning occurs implicitly despite formal prohibition').

omega_variable(
    reading_vs_natural_law_ambiguity,
    'Is the Hanbali method a reading of the usul_al_fiqh_method kernel that could be revised by the tradition, or a fixed natural law of textual authority that cannot be revised without abandoning Islam itself?',
    'Historical analysis of Hanbali jurisprudential drift over centuries; identification of moments where the tradition explicitly softened or hardened its standards; examination of whether reform movements claim to revise the method or only to reinterpret its application',
    'If reading (revisable): the constraint is a Tangled Rope in the commitment-system sense — a defended institutional arrangement benefiting textual orthodoxy. The reform perspective is structurally real and the scaffold classification holds. If natural law (immutable): the method is genuinely mountain-like at the civilizational scale, and contemporary reformism is cosmetic. The kernel reading cannot be revised; only its interpretation can shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_natural_law_ambiguity, conceptual, 'Whether the Hanbali method is a contingent reading or an immutable natural law of textual authority').

omega_variable(
    hadith_scholar_beneficiary_mechanism,
    'Do hadith scholars benefit from the method because it genuinely produces superior legal outcomes, or because it concentrates epistemic authority in their hands?',
    'Comparative outcome analysis: Hanbali jurisprudence vs other schools on measures of legal clarity, adaptive capacity, justiciability, and alignment with Qur''anic principles. Attribution analysis: does benefit flow to hadith specialists qua specialists, or to the method''s functional superiority?',
    'If functional superiority: the method generates genuine coordination (Rope from hadith-scholar perspective), not extraction. Suppression and extractiveness values should be reassessed downward. If authority concentration: the method is structurally extractive even if it produces legal goods. The beneficiary designation holds and Tangled Rope classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_scholar_beneficiary_mechanism, empirical, 'Whether hadith-scholar benefit derives from method quality or epistemic gatekeeping').

omega_variable(
    identity_lock_vs_constrained_distinction,
    'For the subordinated jurist perspective: is the binding mechanism primarily identity-fusion (the jurist cannot conceive of themselves outside the method) or material constraint (career cost and social penalty)?',
    'Ethnographic and interview analysis of jurists who have adopted alternative schools or reformed their methodology; observation of whether identity crisis or practical damage is the reported primary barrier',
    'If identity-locked: the jurist is cognitively captured even when material barriers drop. The snare classification holds and the extraction is internalized (harder to escape). If constrained: the barrier is primarily material (career, reputation, institutional position). The classification might shift toward Tangled Rope with higher agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_distinction, empirical, 'Whether jurist constraint is identity-based or material-based').

omega_variable(
    false_summit_kernel_naturalization,
    'Is the mountain perspective (natural law view) an accurate recognition that textual authority requires restriction of rationalist innovation, or a naturalizing rhetorical move that obscures the contingent institutional arrangement?',
    'Comparative jurisprudence: examination of whether other legal systems with fixed foundational texts (Common Law with constitutional authority, Talmudic law with textual canon) require the same degree of rationalist restriction. Logical analysis: can textual authority be maintained while permitting bounded innovative reasoning?',
    'If naturalization: the mountain perspective is a false summit. The method is a Tangled Rope reading of the usul_al_fiqh_method kernel, defended by beneficiary groups, not an immutable epistemic principle. If genuine natural law: the method''s restrictions are universally required for any textually-grounded legal system, and the mountain classification is accurate. The false summit detector is incorrect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_kernel_naturalization, conceptual, 'Whether textual authority logically requires rationalist restriction or whether restriction is a defended institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanbali_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usul_hanbali_tr_t200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 200, 0.55).
narrative_ontology:measurement(usul_hanbali_tr_t400, usul_al_fiqh_method__hanbali_reading, theater_ratio, 400, 0.58).

% Extraction over time
narrative_ontology:measurement(usul_hanbali_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usul_hanbali_be_t200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(usul_hanbali_be_t400, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 400, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanbali_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(usul_hanbali_su_t200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(usul_hanbali_su_t400, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 400, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The usul_al_fiqh_method kernel decomposes into four structurally distinct constraints corresponding to the four major Islamic jurisprudential schools. Each school instantiates a different reading of how to balance textual authority against innovative reasoning, producing different ε values and different victim/beneficiary structures. The Hanbali reading (this file) maximizes textual restriction and generates the highest suppression and extractiveness among the four readings. The Hanafi reading minimizes textual restriction and incorporates pragmatic reasoning. The Maliki and Shafi'i readings occupy intermediate positions. All four stories must be linked via network.affects_constraints to represent the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
