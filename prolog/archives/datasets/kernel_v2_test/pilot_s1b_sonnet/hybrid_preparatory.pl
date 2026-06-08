% ============================================================================
% CONSTRAINT STORY: hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_preparatory, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hybrid_preparatory
 *   human_readable: Temple Sacrifice Study as Messianic Preparation
 *   domain: religious_law/halakhic_tradition/commitment_system
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple in 70 CE, rabbinic Judaism
 *   faced a structural commitment-system crisis: the Torah's detailed
 *   sacrificial law (Leviticus, large portions of Numbers and Deuteronomy)
 *   could no longer be performed, yet the halakhic tradition maintains that
 *   this law remains binding. The hybrid preparatory reading resolves this
 *   crisis by treating intensive study of Temple law as neither mere archival
 *   preservation nor as a substitute for performance, but as active
 *   preparation for messianic restoration. This reading emerged clearly in
 *   medieval Ashkenazi and Sephardi yeshivot and was codified by figures like
 *   Maimonides (who wrote detailed Temple service protocols in the Mishneh
 *   Torah's Avodah section despite no contemporary Temple). The constraint
 *   coordinates genuine transmission of complex legal knowledge across a
 *   multi-generational performance gap while extracting contemporary
 *   resources (scholar time, institutional funding, communal prestige) for
 *   uncertain future benefit. The preparatory logic differs structurally from
 *   pure archival (which would accept indefinite suspension) and from
 *   symbolic substitution (which would treat study as fulfilling the
 *   obligation rather than preparing for its restoration). The constraint
 *   exhibits rising extractiveness over the interval as the performance gap
 *   lengthens and messianic timeline recedes, while theater ratio rises
 *   modestly as some institutions maintain curriculum through inertia rather
 *   than conviction.
 *
 * KEY AGENTS:
 *   - Contemporary Resource Contributors: Primary victims (powerless/identity_locked) — fund and staff study of non-performable law; cannot exit without abandoning religious identity; extraction is biographical-scale (resources allocated to preparation with no lifetime benefit)
 *   - Non-Messianic Halakhic Practitioners: Secondary victims (moderate/constrained) — benefit from broader halakhic ecosystem while bearing costs of resource diversion to Temple law; constrained by communal norms but not identity-locked
 *   - Rabbinic Study Institutions: Primary beneficiaries (institutional/arbitrage) — capture funding, prestige, and enrollment justified by preparatory logic; experience constraint as coordination (training scholars, maintaining texts)
 *   - Messianic Restorationist Coalition: Organized beneficiaries (organized/constrained) — see preparatory logic as genuine investment with civilizational-scale sunset; lower extraction because exit path (messianic fulfillment) is part of their framework
 *   - Secularizing Rabbinical Schools: Institutional actors experiencing degraded function (institutional/arbitrage) — maintain Temple curriculum through inertia and donor expectations despite faculty skepticism; piton dynamics
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function (continuity across performance gap) and asymmetric extraction (contemporary resources for uncertain future benefit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_preparatory, 0.45).
domain_priors:suppression_score(hybrid_preparatory, 0.6).
domain_priors:theater_ratio(hybrid_preparatory, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_preparatory, extractiveness, 0.45).
narrative_ontology:constraint_metric(hybrid_preparatory, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(hybrid_preparatory, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hybrid_preparatory, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hybrid_preparatory, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(hybrid_preparatory, "Temple Sacrifice Study as Messianic Preparation").
narrative_ontology:topic_domain(hybrid_preparatory, "religious_law/halakhic_tradition/commitment_system").

domain_priors:requires_active_enforcement(hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_preparatory, 'a50252f2-0de1-40d5-994a-9453b8b8794f').
narrative_ontology:cs_kernel_codification('a50252f2-0de1-40d5-994a-9453b8b8794f', fixed_text).
narrative_ontology:cs_authority_grounding('a50252f2-0de1-40d5-994a-9453b8b8794f', lineage).
narrative_ontology:cs_interpretation_layer_present('a50252f2-0de1-40d5-994a-9453b8b8794f').
narrative_ontology:cs_reading_relation('a50252f2-0de1-40d5-994a-9453b8b8794f', hybrid_preparatory__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('a50252f2-0de1-40d5-994a-9453b8b8794f', hybrid_preparatory__performance_only, influences).
narrative_ontology:cs_axiom('a50252f2-0de1-40d5-994a-9453b8b8794f', foundational, study_maintains_preparatory_capacity).
narrative_ontology:cs_axiom_status(study_maintains_preparatory_capacity, holdable).
narrative_ontology:cs_axiom_grounding('a50252f2-0de1-40d5-994a-9453b8b8794f', study_maintains_preparatory_capacity, instrumental).
narrative_ontology:cs_axiom('a50252f2-0de1-40d5-994a-9453b8b8794f', foundational, messianic_restoration_requires_practical_knowledge).
narrative_ontology:cs_axiom_status(messianic_restoration_requires_practical_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('a50252f2-0de1-40d5-994a-9453b8b8794f', messianic_restoration_requires_practical_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('a50252f2-0de1-40d5-994a-9453b8b8794f', secondary, suspended_performance_preserves_obligation).
narrative_ontology:cs_axiom_status(suspended_performance_preserves_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a50252f2-0de1-40d5-994a-9453b8b8794f', suspended_performance_preserves_obligation, deontological).
narrative_ontology:cs_reference_frame('a50252f2-0de1-40d5-994a-9453b8b8794f', sinaitic_revelation_with_temple_performance).
narrative_ontology:cs_drift_state('a50252f2-0de1-40d5-994a-9453b8b8794f', post_temple_destruction_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a50252f2-0de1-40d5-994a-9453b8b8794f', '').
narrative_ontology:cs_kernel_id(hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_preparatory, rabbinic_study_institutions).
narrative_ontology:constraint_beneficiary(hybrid_preparatory, messianic_restorationist_coalition).
narrative_ontology:constraint_victim(hybrid_preparatory, contemporary_resource_contributors).
narrative_ontology:constraint_victim(hybrid_preparatory, non_messianic_halakhic_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hybrid_preparatory, non_messianic_halakhic_practitioners).
narrative_ontology:constraint_vindicates(hybrid_preparatory, messianic_restoration_inevitability).
narrative_ontology:constraint_vindicates(hybrid_preparatory, halakhic_continuity_through_study).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund and staff intensive study of Temple sacrificial law that cannot be performed in their lifetime. Identity as observant Jews is constituted through participation in this study system. Biographical time horizon makes the extraction visible: resources flow to preparation for messianic restoration that they will not see. Cannot exit without abandoning religious identity entirely — the identity lock is cognitive (self-concept depends on participation) rather than material.
narrative_ontology:constraint_stakeholder(hybrid_preparatory, contemporary_resource_contributors, payer,
    powerless, biographical, identity_locked, local).

% Participate in halakhic study tradition and benefit from shared textual infrastructure and rabbinic training methods, but bear costs of resource allocation to Temple law that diverts funding and attention from other halakhic priorities (social justice, community care, contemporary ethical questions). Constrained by communal norms and institutional pressure but not identity-locked — exits are costly but possible. Generational time horizon allows seeing both the coordination function (preserving tradition) and the extraction (resource diversion).
narrative_ontology:constraint_stakeholder(hybrid_preparatory, non_messianic_halakhic_practitioners, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hybrid_preparatory, non_messianic_halakhic_practitioners, beneficiary).

% Set curriculum and administer funding for Temple law study. Capture resource flows (donations, tuition, communal prestige) justified by preparatory logic. Experience the constraint as coordination: training scholars, maintaining textual mastery, preserving tradition. Net beneficiaries — institutions grow and gain legitimacy through the preparatory framework. Arbitrage exit: can shift focus if resource flows change, but currently benefit from maintaining preparatory emphasis.
narrative_ontology:constraint_stakeholder(hybrid_preparatory, rabbinic_study_institutions, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hybrid_preparatory, rabbinic_study_institutions, beneficiary).

% Organized groups committed to Temple restoration (certain Haredi communities, Temple Institute activists, religious Zionist factions). See preparatory study as genuine investment for imminent restoration rather than indefinite extraction. Civilizational time horizon encompasses the messianic transition, so current resource allocation is not experienced as extraction but as necessary preparation. Constrained exit because the constraint is embedded in communal structures, but organized capacity gives them agency and collective voice.
narrative_ontology:constraint_stakeholder(hybrid_preparatory, messianic_restorationist_coalition, beneficiary,
    organized, civilizational, constrained, global).

% Maintain Temple law curriculum through institutional inertia and donor expectations despite faculty skepticism about messianic restoration. The preparatory function has atrophied — study persists as credentialing ritual and cultural preservation rather than genuine preparation. Arbitrage exit: could drop Temple law from curriculum, but donor base and accreditation norms create incentives to maintain it. Generational time horizon shows the degradation: the preparatory logic that motivated medieval scholars no longer drives the institution, but the curriculum persists.
narrative_ontology:constraint_stakeholder(hybrid_preparatory, secularizing_rabbinical_schools, agenda_setter,
    institutional, generational, arbitrage, national).

% Organized Jewish movements that rejected Temple restoration theology in 19th-20th centuries. Would object to resource allocation for preparatory study if included in the conversation, arguing for reallocation to contemporary ethical and social priorities. Excluded from the constraint's authority structure (Orthodox halakhic discourse defines the terms) but present as alternative option for identity-locked individuals considering exit. Mobile exit: successfully created alternative Jewish institutional structures outside the preparatory framework.
narrative_ontology:constraint_stakeholder(hybrid_preparatory, reform_reconstructionist_movements, excluded,
    organized, generational, mobile, regional).

% Views the constraint from outside any particular commitment framework. Sees both the genuine coordination function (maintaining complex legal knowledge across 1900+ year performance gap is a real collective action problem with no obvious alternative solution) and the asymmetric extraction (contemporary biographical-scale contributors fund civilizational-scale benefits they will not see, with identity-lock preventing recognition of the extraction). Neither collecting from nor paying into the constraint — occupies analytical position to assess structural dynamics.
narrative_ontology:constraint_stakeholder(hybrid_preparatory, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining detailed technical knowledge of Temple sacrificial law across a multi-generational performance gap. Without this coordination, the complex legal tradition (animal selection criteria, ritual procedures, purity requirements, priestly duties) would be lost if Temple restoration became possible. The preparatory logic solves the real problem of knowledge transmission when practice is suspended.
% TRANSFER_FUNCTION: Scholar time, institutional funding, communal cognitive attention, and prestige flow from contemporary resource contributors to rabbinic study institutions and the messianic restorationist coalition. Resources move from biographical-scale individuals (who bear costs without seeing benefits) to institutions that capture legitimacy and funding through preparatory framework, and to the future generation that would perform restored Temple service.
% ABSENT_VOICES: Reform and Reconstructionist movements, secular Jews, non-messianic halakhic practitioners who prioritize contemporary social justice and ethical questions over Temple restoration. These voices would contest resource allocation to Temple law study, arguing for reallocation to present-day priorities. They are excluded because Orthodox halakhic discourse sets the terms of the conversation and defines what counts as authentic Jewish practice. Their absence is structural: the preparatory logic is embedded in Orthodox institutional authority, and dissenting voices must either exit (Reform/Reconstructionist movements) or remain constrained (non-messianic practitioners within Orthodox communities).
% DISAPPEARANCE_RATIONALE: If the preparatory constraint disappeared, resource flows would rearrange substantially: yeshiva funding would shift to other study areas, rabbinic training would emphasize different texts, communal prestige would flow to different scholarly priorities. The constraint organizes real institutional arrangements (curriculum structure, funding allocation, career paths). This is not a natural fact that would persist regardless — it is a contingent coordination mechanism that channels resources and shapes institutions. The rearrangement would be contested (messianic coalition would resist) but structural (institutions depend on preparatory logic for current resource flows).
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE created a halakhic crisis: the Torah's detailed sacrificial law could no longer be performed, yet the tradition maintains these laws are permanently binding divine commandments. The founding problem was: how to maintain commitment to laws that cannot be practiced? Three options emerged: abandon the laws (rejected by rabbinic consensus), treat study as substitute performance (study_as_exercise reading), or treat study as preparation for future restoration (hybrid_preparatory reading, THIS constraint). The founding problem was real and urgent: without some resolution, a major portion of the Torah would become dead letter, threatening the tradition's coherence.
% FOUNDING_PROBLEM_CORROBORATION: Status is contested between two camps: (1) Messianic restorationist coalition (certain Haredi communities, Temple Institute, religious Zionist factions) attests the problem remains live — Temple restoration is imminent or inevitable, so preparatory study maintains urgent functional value. (2) Non-messianic practitioners (some Modern Orthodox, Conservative movement, academic scholars of rabbinics) attest the problem is dead — after 1900+ years, messianic restoration is indefinitely deferred, so preparatory function is cover story and the constraint persists through institutional extraction and identity-lock rather than genuine preparation. The founding problem's status IS the structural ambiguity the constraint depends on: if messianic restoration is real and near, preparatory logic is genuine coordination; if indefinitely deferred, preparatory logic is extraction mechanism. Corroboration comes from both camps from within their frameworks; no neutral corroboration exists because the status question is the core theological dispute.
narrative_ontology:disappearance_verdict(hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(hybrid_preparatory, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTEMPORARY RESOURCE CONTRIBUTOR (SNARE) — Identity-locked within the community; contributes time, money, and cognitive resources to study of non-performable Temple law. Biographical time horizon reveals the extraction: resources flow to preparatory study with no performable output in their lifetime. Cannot exit without abandoning religious identity entirely. High experienced extraction — the preparatory logic defers benefit indefinitely while extracting resources now.
constraint_indexing:constraint_classification(hybrid_preparatory, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: NON-MESSIANIC HALAKHIC PRACTITIONER (TANGLED ROPE) — Constrained by communal norms and institutional pressure but not identity-locked. Benefits from the broader halakhic study ecosystem (shared methods, textual infrastructure, rabbinic training) while bearing costs of resource allocation to non-practical Temple law. Generational horizon allows seeing both coordination (preserving tradition) and extraction (resource diversion). Mixed experience — some genuine coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(hybrid_preparatory, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINIC STUDY INSTITUTION (ROPE) — Benefits from resource flows (funding, prestige, enrollment) justified by preparatory logic. Experiences the constraint as coordination: maintaining textual mastery and training scholars. Net beneficiary — extraction runs toward this agent. Immediate time horizon focuses on current institutional function rather than messianic fulfillment.
constraint_indexing:constraint_classification(hybrid_preparatory, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MESSIANIC RESTORATIONIST COALITION (SCAFFOLD) — Organized agents committed to Temple restoration see the constraint as temporary by definition: study prepares for eventual performance. Civilizational time horizon encompasses the messianic transition. The preparatory logic carries an implicit sunset — when the Temple is rebuilt, study transitions to performance. Lower experienced extraction because the coalition sees an exit path (messianic fulfillment) and treats current resource allocation as investment.
constraint_indexing:constraint_classification(hybrid_preparatory, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SECULARIZING RABBINICAL SCHOOL (PITON) — Maintains Temple law curriculum through institutional inertia and donor expectations despite faculty skepticism about messianic restoration. The preparatory function has atrophied into performance — study persists as credentialing ritual and cultural preservation rather than genuine preparation. Theater ratio is lower than full piton (0.35) because some faculty maintain sincere commitment, but the institutional center sees degraded function.
constraint_indexing:constraint_classification(hybrid_preparatory, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint coordinates continuity of legal tradition (genuine coordination function) while extracting contemporary resources for uncertain future benefit (asymmetric extraction). The hybrid preparatory logic solves a real commitment-system problem (how to maintain detailed technical knowledge across a performance gap) but creates resource allocation asymmetries. Both functions are structural — not pure coordination (too much extraction) and not pure extraction (genuine coordination function exists).
constraint_indexing:constraint_classification(hybrid_preparatory, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_preparatory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_preparatory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_preparatory, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_preparatory, TR),
    TR >= 0.70.

:- end_tests(hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate-high. The preparatory logic extracts contemporary resources (scholar time, institutional funding, communal cognitive attention) for uncertain future benefit. This is higher than pure coordination (0.10-0.25) but lower than pure extraction (0.65+) because genuine coordination function exists: the tradition successfully maintained detailed technical knowledge across 1900+ years without performance, and this knowledge would be necessary if restoration occurred. The value reflects that much extraction is real (biographical-scale contributors bear costs for civilizational-scale benefits they will not see) but some is legitimate coordination (maintaining complex legal tradition across performance gap is a genuine collective action problem). Suppression (0.60): Moderate-high. Strong communal and institutional pressure maintains commitment to preparatory study; identity-lock mechanisms are powerful; alternative halakhic priorities (social justice, community care) face legitimacy barriers when competing for resources. But suppression is not total — exits are possible (Reform/Reconstructionist movements abandoned Temple restoration theology) and some internal debate exists. Theater ratio (0.35): Low-moderate. Most Temple law study is functionally preparatory or educationally valuable (training in textual analysis, legal reasoning, conceptual precision), not pure performance. Theater has risen over the interval as some institutions maintain curriculum through inertia, but the preparatory function remains substantially real across most of the tradition. This is notably lower than regulatory theater (0.60-0.80) because the study maintains genuine scholarly rigor and institutional function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a clear six-way perspectival split. Contemporary contributors at biographical scale see snare — resources extracted for deferred benefit they will never see, with identity-lock preventing exit. Non-messianic practitioners at generational scale see tangled rope — both coordination (shared halakhic tradition) and extraction (resource diversion from their priorities). Rabbinic institutions see rope — they are net beneficiaries experiencing the constraint as coordination (scholar training, textual transmission). The messianic coalition sees scaffold — a temporary constraint with implicit sunset (Temple restoration), so current costs are investment rather than extraction. Secularizing institutions see piton — a degraded preparatory function maintained through inertia, with rising theater ratio. The analytical observer sees tangled rope at civilizational scope — genuine coordination function (knowledge transmission across performance gap) coexisting with genuine asymmetric extraction (contemporary resources for uncertain future benefit). The gap is not 'which type is correct' but 'which time horizon and exit capacity are you measuring from.' No single type captures the structure — the presheaf over the six perspectives IS the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Contemporary resource contributors are victims with identity-locked exit at biographical time horizon — they experience high d (near full target) and thus high effective extraction. The identity lock is cognitive: their self-concept as observant Jews is constituted through participation in the halakhic study system, making exit literally unthinkable from within their framework even though they are structurally mobile (could join non-Orthodox movements, secular life). Non-messianic practitioners are victims with constrained exit at generational horizon — they experience moderate d because they bear costs (resource diversion) but also benefit (shared halakhic infrastructure), and they have more exit capacity than identity-locked contributors. Rabbinic institutions are beneficiaries with arbitrage exit — they experience low or negative d (they are net subsidized by the constraint) and thus negative effective extraction. The messianic coalition are beneficiaries with constrained exit at civilizational horizon — they experience low d because the preparatory logic aligns with their framework (investment, not extraction) and their organized capacity gives them agency, but they cannot unilaterally exit because the constraint is embedded in communal structures. The secularizing schools are institutional actors with arbitrage exit experiencing piton dynamics — their effective extraction is low (they maintain the constraint for external reasons: donor expectations, credentialing ritual) and the classification derives from theater gate rather than high chi. The analytical observer at civilizational scope sees both coordination and extraction structurally, producing tangled rope classification with moderate effective extraction reflecting the genuine mixed function.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that preparatory logic can be BOTH genuine coordination (solving the real problem of maintaining complex legal knowledge across a performance gap) AND extractive (diverting contemporary resources for uncertain future benefit). The analytical perspective's tangled rope classification is not a failure to choose between rope and snare — it is the correct structural reading. The coordination function is real: no other mechanism would preserve the detailed technical knowledge required for Temple service across 1900+ years. The extraction is also real: biographical-scale contributors fund preparation for civilizational-scale restoration they will not see, with identity-lock preventing exit. The mandate (maintain Temple law knowledge for messianic restoration) has not outlived its function from the messianic coalition's perspective (function is future-oriented by definition), but has become extractive from contributors' biographical perspective (function is deferred beyond their lifetime). This is not mandatrophy in the classical sense (function atrophied, extraction persists) but rather extraction inherent to the coordination logic (preparatory investment always extracts from early contributors for late beneficiaries). The constraint is working as designed — the extraction IS the design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_ambiguity,
    'Is the messianic restoration timeline sufficiently concrete to distinguish preparatory investment from indefinite extraction?',
    'Historical analysis: comparison with other ''temporary'' religious suspensions that became permanent; tracking of messianic timeline predictions and their revision patterns within the tradition.',
    'If timeline is concrete and near: scaffold perspective strengthened — preparatory logic is genuine investment. If timeline recedes indefinitely: snare perspective strengthened — preparatory logic becomes extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_timeline_ambiguity, empirical, 'Whether messianic restoration timeline distinguishes preparation from extraction').

omega_variable(
    study_retention_effectiveness,
    'Does intensive study of non-performable Temple law actually maintain institutional capacity for future performance, or does the performance gap degrade practical knowledge regardless of textual study?',
    'Comparative analysis: other traditions with performance gaps (e.g., post-revolutionary liturgical restoration attempts); assessment of whether textual study without performance maintains tacit knowledge.',
    'If study maintains capacity: coordination function is real, tangled rope classification confirmed. If performance gap degrades practical knowledge: coordination claim is cover story, extraction is primary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_retention_effectiveness, empirical, 'Whether study without performance maintains practical capacity').

omega_variable(
    alternative_commitment_suppression,
    'Does the preparatory logic suppress alternative halakhic commitments (non-Temple-focused study, community service, social justice work) by monopolizing resource flows and institutional legitimacy?',
    'Resource allocation analysis: tracking of yeshiva funding, rabbinic training curriculum requirements, communal prestige allocation across different halakhic domains.',
    'If Temple study monopolizes resources: suppression metric should be higher, snare classification strengthened. If resources are genuinely available for alternatives: coordination function is real, rope/scaffold perspectives strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_commitment_suppression, empirical, 'Whether preparatory logic suppresses alternative halakhic commitments').

omega_variable(
    kernel_reading_structural_ambiguity,
    'Is this constraint (hybrid preparatory reading) structurally distinct from the study_as_exercise reading, or are they the same constraint with different theological framings?',
    'Cross-reading comparison: if resource flows, beneficiary sets, and institutional structures are identical across the two readings, they are the same constraint with different cover stories. If resource patterns differ (preparatory logic channels resources differently than exercise logic), they are distinct constraints.',
    'If structurally identical: the kernel decomposes into fewer constraints than sibling count suggests; the reading distinction is theological theater rather than structural difference. If structurally distinct: the kernel genuinely bifurcates — different readings create different extraction patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_ambiguity, conceptual, 'Whether preparatory and exercise readings are structurally distinct constraints').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the Temple sacrifice system itself (the destroyed practice), or is it the halakhic commitment to maintain knowledge of that system? These framings produce different cs_pattern classifications.',
    'Structural disambiguation: if the kernel is the sacrifice system, authority_grounding is lineage (transmission from Sinai) and the performance gap creates codification_collapse. If the kernel is the commitment to maintain knowledge, authority_grounding is practice (study community defines continuity) and interpretation_layer_present is true (study absorbs the performance gap without surfacing revision).',
    'First framing (sacrifice system as kernel): cs_pattern likely shows authority_erosion or codification_collapse — the kernel cannot govern practice because practice is impossible. Second framing (maintenance commitment as kernel): cs_pattern likely shows stable authority with interpretation_layer absorbing drift — the kernel IS the study practice, so no gap exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether kernel is the sacrifice system or the study commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_preparatory, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, hybrid_preparatory, theater_ratio, 0, 0.15).
narrative_ontology:measurement(theater_medieval, hybrid_preparatory, theater_ratio, 500, 0.2).
narrative_ontology:measurement(theater_early_modern, hybrid_preparatory, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(theater_modern, hybrid_preparatory, theater_ratio, 1500, 0.3).
narrative_ontology:measurement(theater_contemporary, hybrid_preparatory, theater_ratio, 1900, 0.35).

% Extraction over time
narrative_ontology:measurement(extraction_initial, hybrid_preparatory, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(extraction_medieval, hybrid_preparatory, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(extraction_early_modern, hybrid_preparatory, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement(extraction_modern, hybrid_preparatory, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement(extraction_contemporary, hybrid_preparatory, base_extractiveness, 1900, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(suppression_initial, hybrid_preparatory, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(suppression_medieval, hybrid_preparatory, suppression_requirement, 500, 0.5).
narrative_ontology:measurement(suppression_early_modern, hybrid_preparatory, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(suppression_modern, hybrid_preparatory, suppression_requirement, 1500, 0.58).
narrative_ontology:measurement(suppression_contemporary, hybrid_preparatory, suppression_requirement, 1900, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_preparatory, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the temple_sacrifice_commitment kernel. The readings are structurally distinct (different ε values due to different resource flows and beneficiary structures) and should be modeled as separate constraint stories linked by reading_relations in cs_structure. The hybrid_preparatory reading has higher extractiveness than study_as_exercise (which treats study as fulfilling the obligation rather than preparing for future performance) and lower theater ratio than performance_only (which would show higher theater because study has no claimed preparatory function, only archival).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
