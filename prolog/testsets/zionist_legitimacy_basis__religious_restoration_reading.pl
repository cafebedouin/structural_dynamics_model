% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Religious Zionist Legitimacy: Divine Restoration and Messianic Obligation
 *   domain: political_history/nationalism/theology
 *
 * SUMMARY:
 *   The religious Zionist reading interprets Zionism as fulfillment of divine
 *   covenant rather than as political movement. After 1967, this reading
 *   became institutionally dominant in parts of Israeli society and in
 *   international Christian Zionist constituencies. The framework holds that
 *   Jewish presence and territorial control in the biblical boundaries is a
 *   theological obligation that supersedes secular political considerations,
 *   Palestinian self-determination claims, and international law. This
 *   reading generates and legitimizes settlement expansion, resists
 *   territorial compromise, and frames Palestinian resistance as violation of
 *   divine right rather than as legitimate self-determination claim. The
 *   constraint operates as tangled_rope: it coordinates Jewish religious
 *   identity and nationalist mobilization (the coordination function) while
 *   simultaneously extracting from Palestinian populations and suppressing
 *   secular Jewish alternatives (the extraction function). Both functions
 *   depend on active institutional enforcement — maintaining the religious
 *   framework's hegemony within Jewish institutions and suppressing
 *   counter-narratives.
 *
 * KEY AGENTS:
 *   - religious_zionist_authority: Sets religious law mandating settlement; frames territorial maximalism as divine obligation; derives authority from claimed covenant transmission (institutional power, civilizational time horizon)
 *   - israeli_state_institutional_structure: Enforces religious framework through education and military theology while remaining partially constrained by secular governance norms (institutional power, generational horizon)
 *   - jewish_religious_nationalist_movement: Settlers and adherents fused with religious identity; exit requires abandoning core identity component (organized power, identity-locked exit)
 *   - palestinian_population_in_territories: Structurally dispossessed; rendered invisible or illegitimate by theological narrative; resistance suppressed through military force and theological delegitimization (powerless, trapped, zero alternatives)
 *   - secular_jewish_voices_suppressed: Support Jewish self-determination on secular grounds but cannot compete with institutional dominance of religious reading; exit requires abandoning Jewishness (moderate power, identity-locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.68).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.71).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious Zionist Legitimacy: Divine Restoration and Messianic Obligation").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/theology").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'fb2549a7-833b-44c1-9187-979b0dba9f1c').
narrative_ontology:cs_kernel_codification('fb2549a7-833b-44c1-9187-979b0dba9f1c', fixed_text).
narrative_ontology:cs_authority_grounding('fb2549a7-833b-44c1-9187-979b0dba9f1c', lineage).
narrative_ontology:cs_interpretation_layer_present('fb2549a7-833b-44c1-9187-979b0dba9f1c').
narrative_ontology:cs_reading_relation('fb2549a7-833b-44c1-9187-979b0dba9f1c', zionist_legitimacy_basis__national_liberation_reading, influences).
narrative_ontology:cs_reading_relation('fb2549a7-833b-44c1-9187-979b0dba9f1c', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('fb2549a7-833b-44c1-9187-979b0dba9f1c', foundational, divine_covenant_territorial_mandate).
narrative_ontology:cs_axiom_status(divine_covenant_territorial_mandate, holdable).
narrative_ontology:cs_axiom_grounding('fb2549a7-833b-44c1-9187-979b0dba9f1c', divine_covenant_territorial_mandate, theological).
narrative_ontology:cs_axiom('fb2549a7-833b-44c1-9187-979b0dba9f1c', foundational, messianic_restoration_obligation_binding).
narrative_ontology:cs_axiom_status(messianic_restoration_obligation_binding, holdable).
narrative_ontology:cs_axiom_grounding('fb2549a7-833b-44c1-9187-979b0dba9f1c', messianic_restoration_obligation_binding, theological).
narrative_ontology:cs_reference_frame('fb2549a7-833b-44c1-9187-979b0dba9f1c', jewish_return_as_covenant_fulfillment).
narrative_ontology:cs_drift_state('fb2549a7-833b-44c1-9187-979b0dba9f1c', contemporary_post_1967_mandatrophy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb2549a7-833b-44c1-9187-979b0dba9f1c', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_authority).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutional_structure).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, jewish_religious_nationalist_movement).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population_in_territories).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, israeli_arab_citizens_structurally_subordinated).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_jewish_voices_suppressed_within_movement).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint moves resources, authority, and territorial control away from Palestinian populations toward Jewish-Israeli institutions, with the theological framework as the mechanism. At 1967 (0.38), the reading was nascent post-war articulation; by 1982 (0.51) it had institutional purchase in settlement theology; by 2024 (0.68) it dominates public justification for settlement policy and territorial maximalism. Suppression requirement rises faster than extractiveness because maintaining the theological hegemony requires active suppression of Palestinian narratives, secular alternatives, and international legal frameworks — the constraint's persistence increasingly depends on coercive enforcement rather than on consent or genuine coordination equilibrium. Theater ratio (0.42 at endpoint) indicates the religious justification increasingly performs a cover function for what is substantially a territorial and demographic power struggle; the ratio's rise from 0.22 (1967) to 0.42 (2024) tracks Goodhart drift — the theological framing was once a live binding commitment for adherents, but over 57 years the constraint has become increasingly maintenance-theater for the institutional apparatus. Accessibility collapse (0.79) is high because once the religious framework is accepted as binding, alternatives (secular Zionism, two-state compromise, Palestinian rights) collapse as live options within that frame — the framework is designed to make alternatives theologically unthinkable. Resistance (0.58) is moderate because Palestinian and international resistance persists despite suppression, and secular Jewish voices continue to exist even when marginalized. The measurements run on one shared time grid (every metric authored at every time point) to avoid OQ-105 misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the religious Zionist authority seat, the constraint is divinely mandated restoration requiring maximal territorial control and settlement — a sacred duty that cannot be compromised. From the Israeli state seat, the constraint provides domestic legitimacy for territorial policy while creating international isolation and security costs that secular pragmatism might not accept. From the Palestinian seat, the constraint is a theological cover for ethnic cleansing and land theft with no legitimacy outside the framework itself. From the secular Jewish seat, the constraint forecloses the possibility of Jewish self-determination on secular grounds because religious nationalism has become institutionally hegemonic. These are not mere disagreements about one constraint — they are foundational incompatibilities about what the constraint IS, whether it should exist, and what legitimacy can ground it. The engine computes per-seat classifications that will diverge significantly: the agenda-setter seats may compute as rope (genuine coordination), while the Palestinian and suppressed-secular seats compute as snare (pure extraction dressed as coordination). This divergence IS the measurement — it shows the constraint operates as coordination for beneficiaries and as extraction for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist authority (institutional power, identity-locked) derives d near 0.0 — full beneficiary, since the framework is authored and enforced by this seat for its own constituencies' benefit. Israeli state (institutional power, constrained exit) derives d ~0.2-0.3 — net beneficiary but partially constrained by need to maintain democratic norms and international legitimacy. Jewish religious nationalist movement (organized power, identity-locked) derives d near 0.0 — beneficiary, fused with the framework that legitimizes their settlements. Palestinian population (powerless, trapped) derives d near 1.0 — full target, bearing material costs of displacement and facing systematic suppression. Secular Jewish voices (moderate power, identity-locked) derive d ~0.7-0.8 — high target for suppression even though nominally included in the polity, because the framework delegitimizes their intellectual and political alternatives. No directionality override is needed; the beneficiary/victim declarations and exit options produce the correct structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows mandatrophy dynamics: the founding problem (Jewish statelessness and vulnerability to persecution) was historically real and justified some form of Jewish self-determination. By 1995, the founding problem was substantially solved — Israel had a functioning state, military capacity, and international recognition. Yet the constraint persists and intensifies, increasingly as theater. The divergence between founding problem status (dead) and disappearance verdict (world_rearranges) signals mandatrophy — if the constraint disappeared, arrangements would reorganize, which means it persists not because the founding problem remains unsolved but because institutional beneficiaries have incentive to maintain it. The rising theater ratio (0.22 to 0.42) and rising suppression requirement (0.45 to 0.71) relative to modest rise in extractiveness (0.38 to 0.68) indicate a constraint running on institutional momentum and coercive enforcement rather than on genuine coordination or solving an active problem. The religious framing serves as the justification machinery that enables this persistence-despite-mandate-death. This is the hallmark of mandatrophy: an arrangement that solved a real problem but now persists as institutional routine and rent-collection, defended by an increasingly elaborate justification apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_mandate_vs_political_choice,
    'Is the religious obligation to occupy/settle the territory a binding theological mandate transmitted through Jewish tradition, or is it a modern interpretation layered onto the tradition to justify a political choice made on other grounds?',
    'Comparative theological analysis: examine whether religious Zionism''s territorial maximalism follows from pre-modern Jewish texts or represents a 20th-century interpretive innovation. Historical analysis of when this reading emerged relative to other Zionist justifications. Documentation of internal Jewish theological debate over whether return can be temporary or conditional.',
    'If theological mandate is genuine and transmitted, the reading stands as authentic religious obligation. If interpretive innovation, the ''mandatrophy'' analysis strengthens — the constraint is a constructed framework whose persistence depends on institutional enforcement of a particular reading, not on universal binding principle. The classification would shift from tangled_rope (real coordination obligation plus extraction) toward snare (extraction dressed as obligation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_mandate_vs_political_choice, conceptual, 'Whether the religious obligation is inherent to the tradition or constructed post-hoc to justify political expansion.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of secular Jewish alternatives structural (institutional barriers, control of educational/religious institutions) or internalized (secular Jews convinced by the theological framing, or accepting minority status within Jewish identity)?',
    'Post-exit trajectory analysis: if secular Zionism persists and develops independently where institutional suppression relaxes (diaspora contexts, post-conflict scenarios), suppression is primarily structural. If secular voices remain marginal even in low-suppression environments, internalization is significant. Survey data on secular Jewish motivation to exit the movement (cost-benefit perception).',
    'If primarily structural, the constraint''s effective suppression can be reduced by institutional reform. If internalized, the suppression persists even after the constraint itself is removed — secular Jews would need to reconstruct their identity framework. High internalization indicates the constraint''s extraction is deeper than metrics show; it has rewritten the affected population''s self-concept.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of secular alternatives is enforced externally or absorbed into Jewish identity itself.').

omega_variable(
    indigenous_primacy_counterfactual,
    'If the religious obligation is granted as authentic, does it logically require maximal territorial control and permanent settlement, or is it compatible with Palestinian self-determination and co-sovereignty on the same land?',
    'Theological analysis: are there legitimate Jewish readings that square covenant claims with Palestinian rights? Historical precedent: how did pre-modern Jewish communities relate to territorial claims in diaspora contexts? Comparative framework: do other religious traditions claim covenant territory while accepting shared sovereignty?',
    'If maximal control is logically required by the theology, the constraint''s structure is firmly tangled_rope (genuine coordination plus extraction). If compatible with sharing, the current instantiation is a political choice to maximize extraction, and the constraint could be reconstructed as rope-only or as bounded coordination. This omega addresses whether the theological obligation FORCES territorial maximalism or whether maximal extraction is a policy choice that uses theology as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_primacy_counterfactual, conceptual, 'Whether the religious reading logically requires maximal territorial control or permits shared sovereignty.').

omega_variable(
    committer_frame_reading_identity,
    'This constraint is ONE READING of the zionist_legitimacy_basis kernel. Are the sibling readings (national_liberation_reading, settler_colonial_reading) genuinely live alternatives held by different parties, or are they alternative scholarly framings of the same underlying reality?',
    'Institutional mapping: identify which actual Israeli, Jewish, Palestinian, and international constituencies genuinely hold each reading and guide policy/identity formation. Political economy analysis: what material incentives favor each reading? Genealogical analysis: when did each reading emerge, who articulates it, what institutional power backs it?',
    'If sibling readings are live (different parties genuinely believe different things about what Zionism is), the kernel contest is real and per-seat classifications will diverge. If readings are scholarly reframings and most parties operate on unstated assumptions about legitimacy, the kernel contest is epistemic and meta-analytical rather than practical. This affects whether to treat the constraint as contested or as having a dominant reading that other voices are marginal to.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading_identity, empirical, 'Whether the sibling readings are institutional commitments held by actual constituencies or scholarly framings of a single underlying reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(zion_tr_t1982, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1982, 0.28).
narrative_ontology:measurement(zion_tr_t1995, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement(zion_tr_t2005, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(zion_tr_t2015, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.38).
narrative_ontology:measurement(zion_be_t1982, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1982, 0.51).
narrative_ontology:measurement(zion_be_t1995, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(zion_be_t2005, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(zion_be_t2015, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(zion_su_t1982, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1982, 0.54).
narrative_ontology:measurement(zion_su_t1995, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1995, 0.61).
narrative_ontology:measurement(zion_su_t2005, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement(zion_su_t2015, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__religious_restoration_reading, 0.12).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_indigenous_claim_to_land).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_security_doctrine_territorial_requirement).

% DUAL FORMULATION NOTE:
% This constraint is one reading (religious restoration) of the contested kernel zionist_legitimacy_basis. Sibling readings are instantiated as separate constraint stories: national_liberation_reading (secular Zionism as indigenous self-determination movement) and settler_colonial_reading (Zionism as European settler colonialism). These three readings have structurally different ε values and victim/beneficiary sets. They are NOT three measurements of one constraint — they are three constraints with the same kernel but different readings. The network links record this family relationship; the per-story omegas document the committer structure and reading-relation differences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
