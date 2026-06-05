% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Simultaneous Veneration as Ontological Fusion (Honji-Suijaku Reading)
 *   domain: religious_studies/comparative_religion/japanese_buddhism
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel of
 *   simultaneous veneration (kami and buddha worship coexisting in medieval
 *   and early-modern Japan). The ontological-fusion reading claims that
 *   honji-suijaku theory captures metaphysical truth: kami and buddhas are
 *   ontologically identical beings, with kami representing the 'original
 *   essence' (honji) of buddhas manifested (suijaku) through the indigenous
 *   Japanese context. This reading treats the fusion doctrine as a discovery
 *   of how reality is structured, not as an institutional arrangement.
 *   However, the structural analysis reveals the constraint as a tangled
 *   rope: the fusion doctrine does coordinate devotional practices
 *   (coordination function), but it simultaneously subordinates kami to
 *   Buddhist institutional authority and extracts interpretive monopoly from
 *   the religious marketplace (asymmetric extraction). The doctrine's
 *   institutional beneficiary is the Buddhist hierarchy, which consolidates
 *   power over shrine networks and devotional resources. The doctrine's
 *   victims are indigenous kami autonomy (ontologically subordinated) and lay
 *   practitioners (identity-locked into accepting the fusion as natural). The
 *   extractiveness curve (0.35 → 0.58 → 0.68) reflects the doctrine's
 *   maturation: early fusion (9th-10th century) was a loose interpretive
 *   claim; by the Muromachi period (14th-16th centuries) it had become the
 *   standard religious framework enforced through institutional structures
 *   and deeply internalized in devotional practice. The theater ratio rises
 *   (0.42 → 0.78) as the doctrine becomes increasingly performative: the
 *   philosophical machinery of honji-suijaku requires increasingly elaborate
 *   textual exegesis and ritual theater to maintain coherence as
 *   contradictions emerge between the doctrine and actual kami worship
 *   practices. The suppression requirement rises (0.38 → 0.68) as the
 *   institutional hierarchy must suppress alternative kami theologies
 *   (claiming kami autonomy or kami-buddha distinction) to maintain the
 *   fusion monopoly. This reading coexists with the domain-partition reading
 *   (which claims functional specialization between kami and buddhas) and
 *   influences the pragmatic-incoherence reading (which claims practitioners
 *   never actually believed in fusion and held contradictory beliefs
 *   simultaneously).
 *
 * KEY AGENTS:
 *   - Buddhist Institutional Hierarchy (Tendai, Shingon, other sects): Primary beneficiary (institutional/arbitrage) — consolidates control over shrine networks and devotional resources through monopoly on interpreting kami-buddha relationship
 *   - Indigenous Kami Tradition (pre-Buddhist Japanese religious practice): Primary victim (powerless/trapped) — ontologically subordinated to Buddhist framework; kami lose autonomous status and become expressions of buddha-nature
 *   - Village Kami Devotees (lay practitioners): Secondary victim (powerless/identity_locked) — structurally mobile but identity-locked into accepting fusion doctrine through family tradition and community affiliation; internalize extraction as metaphysical truth
 *   - Regional Shrine Networks: Mixed (moderate/constrained) — experience both coordination benefit (resource-sharing with Buddhist temples) and extraction cost (interpretive subordination to Buddhist authority)
 *   - Meiji State and Separationist Movement: Organized agent (organized/mobile) — sees fusion as temporary institutional arrangement to be dissolved; executes shinbutsu bunri (separation) edict with state power
 *   - Contemporary Folk Practice: Institutional observer (institutional/constrained) — maintains honji-suijaku language and fusion practices through cultural inertia despite institutional enforcement collapse after 1868
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.58).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.62).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Simultaneous Veneration as Ontological Fusion (Honji-Suijaku Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/comparative_religion/japanese_buddhism").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'f1e1f411-5935-4a06-906a-cc1c511b11a1').
narrative_ontology:cs_kernel_codification('f1e1f411-5935-4a06-906a-cc1c511b11a1', fixed_text).
narrative_ontology:cs_authority_grounding('f1e1f411-5935-4a06-906a-cc1c511b11a1', lineage).
narrative_ontology:cs_interpretation_layer_present('f1e1f411-5935-4a06-906a-cc1c511b11a1').
narrative_ontology:cs_reading_relation('f1e1f411-5935-4a06-906a-cc1c511b11a1', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1e1f411-5935-4a06-906a-cc1c511b11a1', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('f1e1f411-5935-4a06-906a-cc1c511b11a1', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('f1e1f411-5935-4a06-906a-cc1c511b11a1', kami_buddha_ontological_identity, deontological).
narrative_ontology:cs_axiom('f1e1f411-5935-4a06-906a-cc1c511b11a1', foundational, honji_suijaku_reveals_metaphysical_reality).
narrative_ontology:cs_axiom_status(honji_suijaku_reveals_metaphysical_reality, holdable).
narrative_ontology:cs_axiom_grounding('f1e1f411-5935-4a06-906a-cc1c511b11a1', honji_suijaku_reveals_metaphysical_reality, deontological).
narrative_ontology:cs_reference_frame('f1e1f411-5935-4a06-906a-cc1c511b11a1', unified_buddho_shinto_ontology).
narrative_ontology:cs_drift_state('f1e1f411-5935-4a06-906a-cc1c511b11a1', meiji_separation_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('f1e1f411-5935-4a06-906a-cc1c511b11a1', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, lay_practitioner_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE KAMI DEVOTEE (SNARE) — Structurally mobile (could reject the fusion doctrine and worship kami autonomously) but identity-locked through generations of family practice, local shrine affiliation, and internalized framing that 'kami and buddhas are really the same.' The binding is cognitive: the identity of 'proper devotee' is constituted through acceptance of the fusion doctrine. The doctrine itself becomes non-negotiable because rejecting it means abandoning the devotional identity. Maximum experienced extraction — the constraint extracts kami autonomy while the devotee internalizes the extraction as metaphysical truth.
constraint_indexing:constraint_classification(simultaneous_veneration__ontological_fusion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: BUDDHIST INSTITUTIONAL HIERARCHY (ROPE) — Benefits from interpretive monopoly granted by the fusion doctrine. The doctrine coordinates multiple devotional traditions (Tendai, Shingon, Pure Land) under a single metaphysical framework, enabling institutional consolidation and resource extraction from kami shrine networks. The hierarchy experiences the constraint as pure coordination: it solves the integration problem of syncretism. Beneficiary with arbitrage exit — can reframe or abandon the doctrine if institutional interests shift, but currently benefits from the monopoly it grants.
constraint_indexing:constraint_classification(simultaneous_veneration__ontological_fusion_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL SHRINE NETWORK (TANGLED ROPE) — Experiences genuine coordination (the fusion doctrine enables resource-sharing between Buddhist temples and Shinto shrines, solving structural problems of rural religious infrastructure) alongside asymmetric extraction (the doctrine subordinates kami shrines to Buddhist interpretive authority). Constrained exit — shrines depend on the Buddhist institutional framework for legitimacy and resource flows in the medieval Japanese context, but some regional shrines resist the fusion and maintain kami autonomy. Mixed experience of the constraint: coordination benefit + extraction cost.
constraint_indexing:constraint_classification(simultaneous_veneration__ontological_fusion_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MEIJI STATE SEPARATIONIST MOVEMENT (SCAFFOLD) — Organized actors (the Meiji government, nationalist intellectuals, Shinto revivalists) see simultaneous veneration as a temporary coordination failure to be dismantled. The sundown mechanism is explicitly political: the 1868 separation edict (shinbutsu bunri) forcibly dissolves the fusion doctrine and returns kami to autonomous status. This perspective sees the constraint as a time-limited hybrid with a definite exit point. Mobile exit capacity — the state has power to unwind the institutional arrangement. Theater ratio reflects performative aspects of the separation itself (manufactured historicity of 'pure Shinto'), but the constraint itself has a declared sunset.
constraint_indexing:constraint_classification(simultaneous_veneration__ontological_fusion_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONTEMPORARY SYNCRETISM OBSERVER (PITON) — Sees honji-suijaku theory as a degraded, inertial mechanism. The doctrine persists in folk practice and some institutional contexts despite losing its enforcing authority after 1868. The metaphysical machinery still runs in cultural memory and ritual practice, but its function (legitimizing institutional consolidation) has atrophied. The theater is high (70%+) because the doctrine is maintained through cultural inertia, textual transmission, and ritual habit rather than active institutional enforcement. Constrained by cultural weight and path dependency, but the constraint no longer has the power it once exercised.
constraint_indexing:constraint_classification(simultaneous_veneration__ontological_fusion_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / METAPHYSICAL TRUTH CLAIM (MOUNTAIN) — From the ontological reading's own framework, honji-suijaku is presented as capturing genuine metaphysical truth: kami and buddhas ARE ontologically identical, and the doctrine reveals this immutable fact. This perspective claims the constraint as natural law — the fusion is not an institutional imposition but a discovery of how reality is structured. However, the structural data reveals this as a false summit: the constraint has identifiable beneficiaries (the Buddhist hierarchy), explicit victims (kami autonomy), and requires active institutional enforcement. The 'metaphysical truth' framing is the mechanism by which extraction is naturalized.
constraint_indexing:constraint_classification(simultaneous_veneration__ontological_fusion_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simultaneous_veneration__ontological_fusion_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simultaneous_veneration__ontological_fusion_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, TR),
    TR >= 0.70.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The fusion doctrine extracts kami autonomy by subordinating it to Buddhist ontology and extracts interpretive authority by placing Buddhist institutions as the sole legitimate adjudicators of kami identity and worship. However, the extraction is not total because the doctrine also genuinely coordinates multiple devotional practices and does address real religious-marketplace problems (how to integrate pre-existing kami worship with new Buddhist institutions). The extractiveness value reflects that this is hybrid extraction-coordination, not pure extraction. Suppression (0.62): Moderate-high. The fusion doctrine suppresses alternative kami theologies (claims of kami autonomy, kami-buddha distinction) through institutional enforcement, doctrinal prohibition, and interpretive monopoly. Lay practitioners cannot advocate for kami autonomy without challenging the fusion framework, which would require exiting their identity and community position. However, suppression is not total — some shrines and practitioners resist the fusion, and after 1868 the institutional enforcement collapses. Theater ratio (0.68): Moderate-high. The honji-suijaku doctrine requires increasingly elaborate philosophical apparatus and ritual theater to maintain coherence. The core mechanism (claiming identity while maintaining functional distinction in practice) is inherently contradictory, and the doctrine's theater rises over time as it must suppress the contradiction more aggressively. Claimed type (Tangled Rope): The doctrine is tangled rope precisely because it combines genuine coordination (solving the problem of integrating heterogeneous devotional practices) with asymmetric extraction (subordinating kami to Buddhist authority). The buddhist institutional hierarchy requires active enforcement to maintain the fusion interpretation against alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits wide perspectival divergence because the fusion doctrine operates as both coordination and extraction depending on the observer's structural position. The Buddhist hierarchy (beneficiary/arbitrage) perceives the constraint as pure coordination — solving the religious marketplace problem of integration, enabling resource-sharing between temples and shrines, creating a unified metaphysical framework. This beneficiary perspective produces ROPE: effective extraction is low because the hierarchy experiences benefit and has exit options. The village kami devotee (victim/identity-locked) perceives the constraint as snare — the fusion doctrine forces acceptance of kami subordination while the devotee's identity becomes constituted through that acceptance, making exit impossible despite structural mobility. The shrine network (moderate/constrained) perceives the constraint as tangled rope — genuine coordination benefit (resource-sharing, institutional stability) alongside asymmetric extraction (interpretive subordination). The Meiji separationist movement (organized/mobile) perceives the constraint as scaffold with explicit sunset — the doctrine is a temporary institutional arrangement to be dissolved, and the state has power to enforce dissolution. The contemporary observer (institutional/constrained) perceives the constraint as piton — the doctrine persists through cultural inertia and ritual habit, but its enforcing power has atrophied after 1868. The analytical observer (analytical/analytical) attempting to take the fusion doctrine's own truth claim seriously perceives it as mountain — honji-suijaku is metaphysical truth, an immutable fact about kami-buddha ontology. However, this mountain classification fails the false summit gate: the doctrine has identifiable beneficiaries, victims, and enforcement mechanisms, revealing the 'metaphysical truth' framing as the institutional naturalization of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The honji-suijaku fusion doctrine operates at multiple directionalities simultaneously. From the beneficiary's position (Buddhist hierarchy), d ≈ 0.1–0.2 (beneficiary with arbitrage options): the doctrine benefits the hierarchy by consolidating authority, and the hierarchy can reframe or abandon it if institutional interests shift. The hierarchy experiences the constraint as coordination (solving religious integration) and low effective extraction. From the victim's position (indigenous kami tradition + lay devotees), d ≈ 0.85–0.95 (trapped or identity-locked victims): kami are ontologically subordinated with no structural exit, and lay devotees are identity-locked (can exit the practice but cannot exit the identity without social/family cost). Victims experience high effective extraction. From the moderate agent's position (regional shrine networks), d ≈ 0.55–0.65 (mixed victim/beneficiary): shrines benefit from resource-sharing and institutional legitimacy but pay cost of interpretive subordination. These directional differences produce the perspectival gap: the beneficiary sees rope, the victim sees snare, the moderate agent sees tangled rope. No override is needed because the directionality derivation from beneficiary/victim declarations and exit options captures the structure correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how a single institutional arrangement (the fusion doctrine) can simultaneously be TRUE as metaphysical claim AND extractive as institutional mechanism. The mandatrophy asks: Is honji-suijaku a real discovery (mountain/rope) or an institutional imposition (snare/tangled rope)? The resolution is that the READING itself — the ontological-fusion reading — is one coherent commitment that makes honji-suijaku appear as metaphysical truth. The empirical question is not whether the doctrine is 'really' true (metaphysical claims are underdetermined by evidence), but whether the fusion doctrine was adopted because it was true, or because it solved institutional problems (consolidating authority, integrating heterogeneous practices, creating resource flows). The historical record shows that the doctrine emerged alongside Buddhist institutional expansion (9th-11th centuries) and intensified as institutional consolidation requirements increased. The mandate-trophy is broken by recognizing that the doctrine CAN be both true (in the sense that honji-suijaku represents a coherent metaphysical framework) AND extractive (in the sense that it benefits the Buddhist hierarchy and subordinates kami autonomy). The constraint is mandatrophy-resolved when we classify it as tangled rope: the coordination function is real (integration, resource-sharing), and the extraction is real (institutional authority consolidation, kami subordination). Both are true simultaneously because they operate at different levels: metaphysically, the doctrine claims identity; institutionally, it enforces hierarchy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honji_suijaku_empirical_referent,
    'Does honji-suijaku theory refer to genuine ontological identity, or is it a institutional interpretive framework designed to subordinate kami to Buddhist authority?',
    'Historical analysis of doctrine development; examination of pre-fusion kami theology vs Buddhist theology; identification of whether the ''identity'' claim emerges from either tradition independently or only appears when institutional consolidation is underway',
    'If genuine identity: the constraint is a rope (coordination of metaphysical truth), and the mountain perspective is correct. If interpretive framework: the constraint is a snare (extraction enforced by naturalizing institutional subordination), confirming the false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_empirical_referent, conceptual, 'Whether honji-suijaku refers to ontological fact or institutional framework').

omega_variable(
    identity_lock_mechanism_strength,
    'Is the village devotee''s acceptance of fusion doctrine primarily identity-locked (cognitive capture) or primarily trapped (structural barriers to alternative worship)?',
    'Post-1868 empirical record: Did devotees who were exposed to separationist doctrine and had structural opportunity to abandon fusion worship do so? Did family/community identity persist as a barrier even when institutional enforcement ceased? Generational analysis of belief persistence after institutional support collapsed.',
    'If primarily identity-locked: the cognitive frame persists after institutional enforcement ends (confirming the identity_locked exit classification). If primarily trapped: the constraint''s power was structural (enforcement), not cognitive, and the devotee population should have shifted rapidly after 1868.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, empirical, 'Cognitive vs structural binding in practitioner compliance').

omega_variable(
    kami_autonomy_subordination_mechanism,
    'Through what mechanism does the fusion doctrine subordinate kami autonomy — through explicit doctrinal claims, through resource-flow control, through interpretive monopoly, or through all three?',
    'Detailed analysis of Buddhist institutional texts declaring kami status; examination of shrine revenue and resource flows; study of who adjudicates kami authenticity and correct worship; comparison of kami-shrine independence before and after fusion doctrine adoption',
    'If doctrinal only: the extraction is primarily theater (a legitimacy claim). If structural (resource/revenue): the extraction is material. If interpretive monopoly: the extraction is authority-control. Each mechanism implies different χ values and different vulnerability to institutional disruption (1868 separation weakens doctrinal claims but may not alter material flows).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_autonomy_subordination_mechanism, empirical, 'Mechanisms through which fusion doctrine subordinates kami autonomy').

omega_variable(
    sibling_reading_empirical_residue,
    'After the Meiji separation, which reading (domain_partition, ontological_fusion, or pragmatic_incoherence) best explains the historical record of devotional practice and belief?',
    'Post-1868 empirical examination: Did practitioners adopt domain-partition logic (kami for this-worldly, buddhas for afterlife, held separately)? Did they maintain fusion despite separation edict (suggesting the reading persists in practice)? Did they reveal hidden incoherence (suggesting pragmatic_incoherence was correct all along)?',
    'The empirical residue after institutional enforcement ends reveals which reading was ''closer to'' practitioner experience. A sibling reading may have superior explanatory power even if the institutional doctrine endorsed this (fusion) reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_residue, empirical, 'Which reading best explains post-separation devotional practice').

omega_variable(
    false_summit_naturalization_scope,
    'Is the ''honji-suijaku is metaphysical truth'' framing a localized institutional strategy, or does it instantiate a general pattern of naturalizing extractive institutional arrangements as universal laws?',
    'Comparative analysis with other religious/institutional false summits (e.g., divine right, caste as natural order, gender roles as biological law). Does the fusion doctrine employ the same naturalization mechanisms? Is it part of a broader epistemic pattern?',
    'If localized: the false summit is a domain-specific institutional move. If general pattern: it reveals a deeper structural vulnerability of knowledge systems to naturalizing institutional extraction. Either way confirms that the mountain classification at analytical scope is false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_scope, conceptual, 'Naturalization mechanism as localized vs general pattern').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sv_ont_theater_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sv_ont_theater_t400, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 400, 0.68).
narrative_ontology:measurement(sv_ont_theater_t800, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 800, 0.78).

% Extraction over time
narrative_ontology:measurement(sv_ont_extractiveness_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sv_ont_extractiveness_t400, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(sv_ont_extractiveness_t800, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 800, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sv_ont_suppression_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(sv_ont_suppression_t400, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 400, 0.62).
narrative_ontology:measurement(sv_ont_suppression_t800, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 800, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.12).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, shinbutsu_bunri_institutional_separation).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, meiji_religious_nationalism).

% DUAL FORMULATION NOTE:
% The simultaneous-veneration kernel decomposes into three structurally distinct constraint stories based on how the relationship between kami and buddhas is interpreted. The ontological-fusion reading (this constraint) claims unity and produces high extraction. The domain-partition reading claims complementary specialization and produces moderate extraction. The pragmatic-incoherence reading claims practitioners held contradictory beliefs and produces low extraction. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and classification. They are linked via network.affects_constraints to show they are sibling readings of the same contested kernel. Empirical evidence (historical texts, devotional practices, institutional records) may resolve which reading better explains the data, but all three remain coherent commitment positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
