% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony Reading — Territorial Legitimacy from Continuous Habitation and Right of Return
 *   domain: political/theoretical/territorial_sovereignty
 *
 * SUMMARY:
 *   The Palestinian autochthony reading grounds territorial legitimacy in
 *   continuous habitation from the pre-1948 population, the trauma of the
 *   Nakba (displacement of 700,000+ Palestinians), and the non-negotiable
 *   right of return. From this reading's seat, the 1948 displacement is an
 *   ongoing injustice requiring remedy, not a historical event resolved by
 *   time. The Israeli state's legitimacy is contested because it was
 *   established through displacement and continues to enforce territorial
 *   reduction (78% of historic Palestine in 1948, 100% after 1967) and denial
 *   of return. The constraint operates as a snare from the Palestinian seat:
 *   high extraction (land, sovereignty, return denied), high suppression
 *   (military occupation, legal barriers, geographic fragmentation), active
 *   resistance (intifadas, diplomatic campaigns, BDS, sumud), and the
 *   coordination story ('two-state solution') functions as cover for
 *   continued extraction. The claim/metric gap is deliberate: Palestinian
 *   leadership and international actors often frame this as a rope
 *   (negotiated two-state coordination) while the metrics describe a snare —
 *   the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - palestinian_refugee_population: Primary target (powerless/identity_locked) — bears extraction of land, sovereignty, return; structural beneficiary of the claim but victim of its non-realization
 *   - palestinians_under_occupation: Primary target (powerless/trapped) — bears daily extraction via settlements, checkpoints, permit regime; identity_locked to land
 *   - internally_displaced_palestinians: Primary target (powerless/identity_locked) — citizens of Israel but denied return to original lands; trapped within Israeli legal framework
 *   - palestinian_civil_society: Agenda setter (organized/identity_locked) — maintains the claim, organizes resistance, bears costs of advocacy; derives identity from the struggle
 *   - israeli_state: Agenda setter (institutional/mobile) — administers the extraction, controls territory, suppresses alternatives; benefits from territorial control
 *   - arab_state_actors_supporting_return: Beneficiary (organized/mobile) — uses the claim for regional legitimacy; limited material cost
 *   - international_legal_order: Observer (institutional/analytical) — holds the legal framework (UNRWA, ICC, ICJ) but fails enforcement; analytical seat
 *   - zionist_constituency: Excluded from this reading's framework (powerful/mobile) — would contest the autochthony premise; their exclusion is structural to this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.82).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.88).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony Reading — Territorial Legitimacy from Continuous Habitation and Right of Return").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/theoretical/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '99a429d8-55ee-41ea-b055-0a8272e51ce6').
narrative_ontology:cs_kernel_codification('99a429d8-55ee-41ea-b055-0a8272e51ce6', formalized).
narrative_ontology:cs_authority_grounding('99a429d8-55ee-41ea-b055-0a8272e51ce6', lineage).
narrative_ontology:cs_interpretation_layer_present('99a429d8-55ee-41ea-b055-0a8272e51ce6').
narrative_ontology:cs_reading_relation('99a429d8-55ee-41ea-b055-0a8272e51ce6', territorial_legitimacy_dual__zionist_refuge_reading, forecloses).
narrative_ontology:cs_reading_relation('99a429d8-55ee-41ea-b055-0a8272e51ce6', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('99a429d8-55ee-41ea-b055-0a8272e51ce6', foundational, right_of_return_nonnegotiable).
narrative_ontology:cs_axiom_status(right_of_return_nonnegotiable, holdable).
narrative_ontology:cs_axiom_grounding('99a429d8-55ee-41ea-b055-0a8272e51ce6', right_of_return_nonnegotiable, deontological).
narrative_ontology:cs_axiom('99a429d8-55ee-41ea-b055-0a8272e51ce6', foundational, id_1948_displacement_as_ongoing_injustice).
narrative_ontology:cs_axiom_status(id_1948_displacement_as_ongoing_injustice, holdable).
narrative_ontology:cs_axiom_grounding('99a429d8-55ee-41ea-b055-0a8272e51ce6', id_1948_displacement_as_ongoing_injustice, empirically_contingent).
narrative_ontology:cs_reference_frame('99a429d8-55ee-41ea-b055-0a8272e51ce6', pre_1948_palestinian_majority_territory).
narrative_ontology:cs_drift_state('99a429d8-55ee-41ea-b055-0a8272e51ce6', post_oslo_accords_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('99a429d8-55ee-41ea-b055-0a8272e51ce6', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugee_population).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_civil_society).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_state_actors_supporting_return).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugee_population).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, internally_displaced_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, internally_displaced_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% 5-7 million refugees and descendants denied return to lands from which they were displaced in 1948 and 1967. They hold the right of return as constitutive of their identity — exit from the claim means self-erasure. They bear the extraction (statelessness, lost property, camp conditions) while the claim's diplomatic recognition provides minimal material benefit. UNRWA services are a palliative, not a remedy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugee_population, payer,
    powerless, generational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugee_population, beneficiary).

% 3+ million Palestinians in West Bank and Gaza living under military occupation: permit regime for movement, settlement expansion confiscating land, resource extraction (water, agriculture), home demolitions, administrative detention. Exit is geographically trapped (Gaza blockade, West Bank checkpoints) — leaving means abandoning the land claim. The constraint extracts daily; resistance is survival (sumud).
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinians_under_occupation, payer,
    powerless, biographical, trapped, national).

% ~400,000 Palestinian citizens of Israel displaced from their original villages (1948) but remaining inside the 1948 lines. They hold Israeli citizenship (formal beneficiary of rights) but are denied return to their lands (Absentee Property Law, Jewish National Fund). Their identity is locked to the displaced villages; exit means assimilating into Israeli national identity and surrendering the return claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, internally_displaced_palestinians, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, internally_displaced_palestinians, beneficiary).

% PLO, PA, factions, NGOs, BDS movement, cultural institutions — they maintain the autochthony claim, organize resistance (diplomatic, legal, popular, cultural), and extract diplomatic recognition and aid. But they also bear costs: repression, co-optation (PA security coordination), fragmentation. Their organizational existence is fused to the struggle; exit means dissolution.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_civil_society, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_civil_society, payer).

% Administers the occupation, controls 100% of historic Palestine, extracts land/water/labor/security, suppresses Palestinian alternatives (settlements, walls, legal framework, military force). Benefits from territorial control and international impunity. Could withdraw (mobile exit) but the Zionist project's logic and domestic politics prevent it. The constraint's suppression machinery is the Israeli state's enforcement apparatus.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, agenda_setter,
    institutional, generational, mobile, universal).

% Arab League, individual states (varies) — rhetorically support right of return for regional legitimacy and domestic stability (refugee populations). Material support is limited and declining (normalization agreements). Low cost, rhetorical gain. Mobile exit: can shift policy (Abraham Accords) without existential cost.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_state_actors_supporting_return, beneficiary,
    organized, biographical, mobile, regional).

% UN (UNRWA, Security Council resolutions 194, 242, 338), ICC, ICJ, human rights treaty bodies — hold the legal framework affirming Palestinian rights (return, self-determination, end of occupation) but fail enforcement. Analytical seat: they see the full structure but lack coercive power to alter it. Their 'peace process' scaffolding (Oslo, Quartet) functions as a piton — theatrical maintenance of a failed coordination.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_legal_order, observer,
    institutional, generational, analytical, universal).

% Israeli Jewish public, world Zionist organizations, Christian Zionist networks — they contest the autochthony premise, ground Israeli legitimacy in persecution history, divine promise, and UN partition acceptance. Structurally excluded from this reading's framework: the autochthony reading treats Zionist legitimacy as the extractive force, not a coordinating partner. Their exclusion is not accidental — it is the structural condition of this reading's coherence.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, zionist_constituency, excluded,
    powerful, generational, mobile, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The autochthony claim coordinates Palestinian collective identity, resistance strategy, and international legal advocacy around a single non-negotiable core: return. It solves the fragmentation problem — without return as the anchor, Palestinian politics fractures into local accommodations (PA security coordination, Gaza isolation, citizenship assimilation).
% TRANSFER_FUNCTION: Moves land, water, sovereignty, and demographic control from the Palestinian people (refugees, occupied, internally displaced) to the Israeli state and its settler population. The transfer is enforced by military occupation, legal frameworks (Absentee Property Law, Planning Law), and international diplomatic cover. The 'peace process' transfers Palestinian acquiescence (recognition, security coordination) for continued extraction.
% ABSENT_VOICES: Palestinian refugees in Lebanon, Syria, Jordan — denied civil rights, barred from professions, stateless — would object to any framework that trades return for statehood. They are structurally excluded from Palestinian decision-making (no vote in PLO/PA) and from Israeli politics. Their absence is not accidental: their inclusion would make the right of return non-negotiable in practice, not just rhetoric.
% DISAPPEARANCE_RATIONALE: If the autochthony claim and its enforcement machinery (UNRWA, right of return legal framework, resistance infrastructure) vanished overnight: the Israeli state would consolidate full sovereignty without demographic challenge; Palestinian refugees would lose their legal anchor and face permanent statelessness or forced assimilation; the international legal order would lose its most persistent test case; Arab states would lose a legitimating rhetoric. The territorial order would rearrange toward Israeli annexation as settled fact.
% FOUNDING_PROBLEM: The arrangement (international recognition of Palestinian rights via UNRWA, UNGA 194, PLO recognition) was built to remedy the 1948 Nakba — the displacement of 700,000+ Palestinians, confiscation of their property, and denial of return. The founding problem was: how to secure remedy for a displaced people whose land was taken by a state recognized by the international community.
% FOUNDING_PROBLEM_CORROBORATION: Israeli historians (Benny Morris, Ilan Pappé, Tom Segev) document the 1948 displacement and property confiscation from Israeli archives — corroboration from outside the Palestinian beneficiary set. UNRWA's own mandate renewal reports (annual, 1950-present) document the unresolved refugee condition. ICJ 2004 Wall Advisory Opinion and 2024 Occupation Advisory Opinion confirm the ongoing illegality from the international legal seat. The founding problem (remedy for 1948 displacement) is dead: no return, no restitution, no compensation — only management.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.82 because the constraint denies return to 5-7 million refugees, confiscated 78-100% of historic Palestine, and extracts sovereignty via permit regimes, settlements, and resource control. The 'coordination function' (two-state negotiations) has delivered negative returns for Palestinians — each round of talks coincided with accelerated settlement expansion. Theater ratio is low (0.15) because the constraint's operation is overtly extractive and suppressive; the Palestinian Authority's performance of state-building is a minor theater layer atop the occupation's reality. Accessibility collapse is 0.92 because alternatives (return, full sovereignty, binational state) are structurally foreclosed by Israeli power and international complicity. Resistance is 0.85 (intifadas, diplomatic warfare, cultural sumud, BDS) — the constraint meets sustained, multi-modal resistance. These metrics are authored from the Palestinian autochthony reading's seat; the engine will compute different χ for other seats.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian refugee seat (identity_locked, powerless, universal scope): the constraint is a snare — total extraction, no exit, active suppression. From the Palestinian civil society seat (organized, identity_locked): tangled_rope — they coordinate resistance and extract diplomatic capital, but the coordination is inseparable from the extraction they suffer. From the Israeli state seat (institutional, mobile): mountain — they treat their control as natural fact, the Palestinian claim as nonexistent. From the international legal order seat (institutional, analytical): scaffold — they maintain a 'peace process' structure with a sunset that never arrives. The engine computes these per-seat types from the structural data; this commentary explains the structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian refugee population is the primary victim (d→1.0): they bear the full extraction (denied return, lost property, statelessness) with identity_locked exit — their self-concept is constituted by the right of return; exit means self-erasure. Palestinians under occupation are also victims (d→0.95): trapped by permit regime, walls, checkpoints; constrained exit (can leave but lose land claim). Internally displaced Palestinians: d→0.9 — citizens but denied return, trapped in Israeli legal order. Palestinian civil society: secondary_role beneficiary (collects diplomatic recognition, NGO funding, political identity) but primary_role payer (organizes resistance, bears repression) — d≈0.4 (mixed). Israeli state: agenda_setter and primary beneficiary of extraction (d→0.05) — collects land, water, security control; mobile exit (could withdraw but chooses not to). Arab state actors: beneficiaries (d→0.1) — low cost, rhetorical gain. International legal order: observer (d=0.5 analytical). The directionality derivation from beneficiary/victim declarations + exit options produces this structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'peace process' scaffold (Oslo, 1993) declared a sunset clause (5-year interim period ending 1999) but the mandate persisted without the founding problem (interim self-government leading to state) being resolved. The mandatrophy is unresolved: the scaffold became a piton (PA security coordination serves Israeli occupation) and the autochthony reading exposes this. The right of return remains the non-negotiable core — no Palestinian leadership can surrender it without losing legitimacy. The snare classification prevents mislabeling the extraction as coordination: the two-state framework extracts Palestinian acquiescence while delivering continued dispossession.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the contested kernel ''territorial_legitimacy_dual''. What does the committer structure imply for classification?',
    'Treat as separate constraint per DP-001 (ε-invariance). The other readings (zionist_refuge_reading, two_state_coexistence_reading) instantiate different constraints with different ε, different victim/beneficiary structures, different classifications. Do not average or hedge across readings.',
    'Ensures this reading''s ε=0.82, suppression=0.88, claimed_type=snare are authored for this reading''s structural position only. Sibling readings will have their own scores.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commiter-frame routing: this reading instantiates one specific constraint; other readings are other files.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.88) primarily structural (military occupation, legal barriers, geographic enclosure) or internalized (Palestinian political fragmentation, normalized dependency, internalized inferiority), or both?',
    'Post-exit trajectory analysis: if suppression persists for Palestinians who exit the immediate occupation zone (diaspora, citizenship elsewhere), reclassify as partially internalized. Compare suppression scores across Gaza, West Bank, 1948 territories, diaspora.',
    'If internalized component is significant, the constraint''s effective suppression is higher than the structural measure suggests — the target carries suppression with them after geographic exit. Would increase χ for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the Palestinian autochthony reading').

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Does the autochthony claim function as a genuine natural-law constraint (mountain-like: ''continuous habitation grounds title regardless of power'') or as a constructed political claim that extracts recognition from the international system?',
    'Track whether the claim operates without active enforcement (mountain) or requires continuous diplomatic, legal, and resistance effort to maintain (snare/tangled_rope). The high suppression and resistance scores suggest the latter.',
    'If mountain-like, ε would be near 0 and the claim would be self-sustaining. The authored metrics (high ε, high suppression, active resistance) indicate it functions as an enforced claim — a snare from the Palestinian seat, contested by Israeli power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, conceptual, 'Natural-law framing vs. enforced political claim in autochthony discourse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pal_autochthony_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(pal_autochthony_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(pal_autochthony_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(pal_autochthony_tr_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(pal_autochthony_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(pal_autochthony_tr_t2023, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(pal_autochthony_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.65).
narrative_ontology:measurement(pal_autochthony_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.72).
narrative_ontology:measurement(pal_autochthony_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.78).
narrative_ontology:measurement(pal_autochthony_be_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1993, 0.8).
narrative_ontology:measurement(pal_autochthony_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(pal_autochthony_be_t2023, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2023, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(pal_autochthony_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(pal_autochthony_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.82).
narrative_ontology:measurement(pal_autochthony_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.85).
narrative_ontology:measurement(pal_autochthony_su_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1993, 0.88).
narrative_ontology:measurement(pal_autochthony_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(pal_autochthony_su_t2023, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2023, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.08).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, oslo_accords_scaffold).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, unrwa_mandate).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, right_of_return_legal_framework).

% DUAL FORMULATION NOTE:
% This constraint is the palestinian_autochthony_reading of the territorial_legitimacy_dual kernel. The zionist_refuge_reading and two_state_coexistence_reading are sibling constraints with different ε, different beneficiary/victim structures, different claimed types. The ε-invariance principle (DP-001) requires separate stories: this reading's ε=0.82 reflects the standing arrangement (Israeli control + denial of return) assessed by the autochthony reading's lights. The zionist reading would author ε≈0.1 (Israeli sovereignty as mountain). The two-state reading would author ε≈0.4 (negotiated compromise as tangled_rope). Different referents, different ε — different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__palestinian_autochthony_reading, organized, 0.4).
constraint_indexing:directionality_override(territorial_legitimacy_dual__palestinian_autochthony_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
