% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__ishmael_covenant_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Ishmaelic Covenant Reading — Inclusive Abrahamic Lineage through Muhammad
 *   domain: religious/theological/institutional
 *
 * SUMMARY:
 *   The Ishmaelic covenant reading re-interprets the Abrahamic promise
 *   (Genesis 12, 15, 17) as inclusive of both Isaac and Ishmael, with
 *   Muhammad as the culminating prophet of the Ishmaelic line. This reading
 *   is instantiated in the Quran (e.g., 2:124-141, 3:67-68, 19:54-55) and
 *   stabilized through tafsir, sira, and fiqh. It functions as a tangled
 *   rope: it coordinates a global umma across vast difference (genuine
 *   coordination function) while extracting interpretive authority and
 *   communal legitimacy from the Isaac-exclusive traditions (asymmetric
 *   extraction). Active enforcement is required — the reading is maintained
 *   through institutional authority (quranic_exegetical_authorities), social
 *   identity pressure (identity_locked exit for muslims), and political
 *   theology that penalizes apostasy and rival readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.38).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.42).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmaelic Covenant Reading — Inclusive Abrahamic Lineage through Muhammad").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious/theological/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, '9cdcd33d-e54f-4f46-bd7d-ad8948813702').
narrative_ontology:cs_kernel_codification('9cdcd33d-e54f-4f46-bd7d-ad8948813702', fixed_text).
narrative_ontology:cs_authority_grounding('9cdcd33d-e54f-4f46-bd7d-ad8948813702', lineage).
narrative_ontology:cs_interpretation_layer_present('9cdcd33d-e54f-4f46-bd7d-ad8948813702').
narrative_ontology:cs_reading_relation('9cdcd33d-e54f-4f46-bd7d-ad8948813702', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('9cdcd33d-e54f-4f46-bd7d-ad8948813702', abrahamic_covenant__christian_supersessionist_reading, influences).
narrative_ontology:cs_reading_relation('9cdcd33d-e54f-4f46-bd7d-ad8948813702', abrahamic_covenant__land_promise_constraint, coexists_with).
narrative_ontology:cs_axiom('9cdcd33d-e54f-4f46-bd7d-ad8948813702', foundational, ishmael_included_in_covenant).
narrative_ontology:cs_axiom_status(ishmael_included_in_covenant, holdable).
narrative_ontology:cs_axiom_grounding('9cdcd33d-e54f-4f46-bd7d-ad8948813702', ishmael_included_in_covenant, deontological).
narrative_ontology:cs_axiom('9cdcd33d-e54f-4f46-bd7d-ad8948813702', foundational, muhammad_seal_of_prophets).
narrative_ontology:cs_axiom_status(muhammad_seal_of_prophets, holdable).
narrative_ontology:cs_axiom_grounding('9cdcd33d-e54f-4f46-bd7d-ad8948813702', muhammad_seal_of_prophets, deontological).
narrative_ontology:cs_axiom('9cdcd33d-e54f-4f46-bd7d-ad8948813702', secondary, quranic_hermeneutic_priority_over_tawrat).
narrative_ontology:cs_axiom_status(quranic_hermeneutic_priority_over_tawrat, holdable).
narrative_ontology:cs_axiom_grounding('9cdcd33d-e54f-4f46-bd7d-ad8948813702', quranic_hermeneutic_priority_over_tawrat, conventional).
narrative_ontology:cs_reference_frame('9cdcd33d-e54f-4f46-bd7d-ad8948813702', primordial_abrahamic_monotheism).
narrative_ontology:cs_drift_state('9cdcd33d-e54f-4f46-bd7d-ad8948813702', contemporary_modernity_encounter, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9cdcd33d-e54f-4f46-bd7d-ad8948813702', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_umma).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, ishmaelite_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_adherents).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, exclusive_election_proponents).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, universal_abrahamic_election).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, prophetic_succession_validity).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, quranic_hermeneutic_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims descent from the covenant through Ishmael and Muhammad. The reading structures communal identity, legal inheritance (sharia), and eschatological orientation. Exit requires abandoning the core self-understanding of the community as the restored Abrahamic nation.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_umma, beneficiary,
    institutional, civilizational, identity_locked, global).

% Groups (e.g., certain Arab tribal confederations, minority Islamic sects) who ground specific political or spiritual claims in direct Ishmaelic descent. Their claims are recognized within the broader umma but contested at margins.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, ishmaelite_lineage_claimants, beneficiary,
    organized, generational, constrained, regional).

% Jewish and Christian traditions that read the covenant as exclusive to Isaac's line. The Ishmaelic reading structurally displaces their exclusivity claim — they bear the cost of a rival legitimacy that draws from the same textual reservoir. They cannot exit the contest because the same scriptures are foundational to their identity.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_adherents, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_adherents, excluded).

% Theological movements (e.g., certain Zionist, dispensationalist, or traditionalist strands) that treat covenantal particularism as non-negotiable. The inclusive reading undermines their political-theological architecture; they resist but operate in a discursive field where the rival reading has institutional weight.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, exclusive_election_proponents, payer,
    organized, biographical, constrained, global).

% Classical and contemporary tafsir tradition, fiqh councils, and major madhabs that authorize the Ishmaelic reading as normative. They administer the interpretive framework, define the boundaries of legitimate dissent, and extract epistemic rent from the reading's stabilization.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, quranic_exegetical_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars and clergy engaged in comparative theology who analyze the reading's structural effects without being bound by its claims. They track how the inclusive hermeneutic reshapes communal boundaries but do not bear its costs or collect its benefits.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, interfaith_dialogue_practitioners, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified Abrahamic identity framework that incorporates the Arab-Islamic world into the covenantal narrative, resolving the theological marginalization of Ishmael and his descendants. Coordinates ritual, legal, and eschatological orientation for a global community across linguistic and cultural difference.
% TRANSFER_FUNCTION: Moves interpretive authority and communal legitimacy from the Isaac-exclusive reading to the inclusive Ishmaelic reading. The transfer runs from exclusive_election_proponents (who lose monopoly on covenantal discourse) to islamic_umma and quranic_exegetical_authorities (who gain a validated, universalized claim). Status, land-theology, and scriptural priority are the transferred goods.
% ABSENT_VOICES: Pre-Islamic Ishmaelite traditions (oral, poetic, or otherwise unrecorded) that might have articulated a covenantal self-understanding independent of both Isaac-exclusive and Quranic framings. Also, medieval Jewish and Christian Ishmaelite communities (e.g., certain Karaites, Nazarene groups) whose readings were suppressed or assimilated. They are absent because the textual record was filtered through the victorious interpretive communities.
% DISAPPEARANCE_RATIONALE: If the Ishmaelic reading vanished, the Islamic umma would lose its primary Abrahamic legitimation; sharia's claim to restore the primordial covenant would collapse; Islamic political theology (caliphate, imamate, umma unity) would require a new foundation. The global religious landscape would reorganize — the Isaac-exclusive reading would regain de facto monopoly, but the structural vacuum would produce new contestations.
% FOUNDING_PROBLEM: The theological-existential problem of Arab exclusion from the Abrahamic covenant: Genesis presents Ishmael as blessed but not the covenant-bearer (Gen 17:19-21). The Quranic reading resolves this by re-reading the promise as inclusive of both lines, with Muhammad as the seal of prophetic succession through Ishmael.
% FOUNDING_PROBLEM_CORROBORATION: Islamic tradition (Quran, hadith, sira) attests the problem is live — the umma's Abrahamic belonging is continually re-actualized. Jewish and Christian exegetes (Rashi, Ibn Ezra, Augustine, Calvin) attest the problem is resolved in favor of Isaac-exclusivity. Modern historical-critical scholarship (e.g., Wansbrough, Crone, Donner) attests the Ishmaelic reading is a late-antique construction, not a recovery of an ancient tradition — corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__ishmael_covenant_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).
:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.38) is moderate: the reading transfers legitimacy from one lineage to another without material expropriation, but the epistemic rent is real — quranic_exegetical_authorities control the interpretive gate. Suppression (0.42) reflects the cost of maintaining the reading against textual counter-evidence (Gen 17:19-21) and rival communities. Theater (0.28) captures the gap between the reading's universalist claim ('religion of Abraham') and its particularist enforcement (apostasy laws, dhimmi structures, sectarian boundaries). Accessibility collapse (0.55) is mid-range: alternatives (Isaac-exclusive, secular, syncretic) persist but are structurally marginalized within the reading's domain. Resistance (0.68) is high — the reading has faced 1400 years of theological, military, and intellectual contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the quranic_exegetical_authorities' seat, the constraint is a rope — genuine coordination of a universal community. From the isaac_covenant_adherents' seat, it is a snare — extraction of their covenantal particularity enforced through supersessionist theology. From the islamic_umma's seat, it is a scaffold — the reading coordinates transition from jahiliyya to umma, but the sunset (eschatological fulfillment) is perpetually deferred. The engine computes this divergence from structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally irreducible.
 *
 * DIRECTIONALITY LOGIC:
 *   islamic_umma and ishmailite_lineage_claimants are beneficiaries (d near 0.0) — the reading subsidizes their identity and institutional claims. isaac_covenant_adherents and exclusive_election_proponents are payers (d near 1.0) — they bear the cost of a rival claim that draws from their textual patrimony. quranic_exegetical_authorities are agenda_setters with identity_locked exit (d ~0.1) — they administer the constraint and benefit from its stabilization. interfaith_dialogue_practitioners are observers (d ~0.5) — analytical seat with mobile exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Arab exclusion from covenant) was live in 610 CE. By 750 CE, the umma's Abrahamic belonging was institutionally secured — the problem shifted from 'are we included?' to 'how do we administer inclusion?'. The mandate has atrophied into maintenance of the interpretive structure that solved it. However, mandatrophy is not resolved: the reading still coordinates a living community (coordination function live) while extracting from rivals (extraction function live). The tangled_rope classification captures this dual persistence — neither pure coordination nor pure extraction, both requiring active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenantal_inclusivity_vs_textual_priority,
    'Does the Genesis text (17:19-21) structurally support an inclusive reading, or does the Ishmaelic reading require a hermeneutic override that constitutes extraction from the Isaac-exclusive tradition?',
    'Comparative philology of the Hebrew ''et-Yitzchaq'' (with Isaac) vs. Quranic ''Isma''il'' insertion; historical-critical analysis of whether the Quranic reading recovers a pre-exilic tradition or innovates a late-antique reinterpretation.',
    'If the inclusive reading is philologically forced, extraction drops toward rope; if it is a hermeneutic override, extraction is confirmed as asymmetric transfer from Isaac-tradition to Ishmael-tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenantal_inclusivity_vs_textual_priority, empirical, 'Whether the textual basis for inclusivity is recovery or construction.').

omega_variable(
    identity_locked_exit_mechanism,
    'Is the identity_locked exit for muslims primarily theological (creedal impossibility of exit), social (communal enforcement), or political (apostasy laws)? How does the mechanism vary across jurisdictions and sects?',
    'Sociological survey of exit trajectories in majority-Muslim vs. minority-Muslim contexts; legal analysis of apostasy statutes; ethnographic study of ex-Muslim communities.',
    'If exit is primarily political, suppression is state-enforced and potentially reversible; if theological, suppression is internalized and the constraint''s effective extraction is higher than structural measures suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, conceptual, 'Mechanism and variability of identity-locked exit for the primary beneficiary.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the abrahamic_covenant kernel have a single stable referent (the Genesis promise), or do the sibling readings instantiate different kernels (Abrahamic-election vs. Abrahamic-land vs. Abrahamic-universalism)?',
    'Genealogical analysis of how each reading cites ''the covenant'' — do they share enough textual and conceptual overlap to be readings of one kernel, or do they talk past each other?',
    'If multiple kernels, the ε-invariance principle requires separate constraint stories per kernel; if one kernel, the sibling relations (coexists_with) are correctly authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel_id refers to one contested commitment or a family of related commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 610, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t610, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 610, 0.08).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t661, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 661, 0.12).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t750, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 750, 0.18).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t900, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 900, 0.22).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t1258, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1258, 0.25).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t1517, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1517, 0.27).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t1924, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1924, 0.28).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t2024, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t610, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 610, 0.15).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t661, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 661, 0.22).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t750, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 750, 0.31).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t900, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 900, 0.34).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t1258, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1258, 0.36).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t1517, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1517, 0.37).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t1924, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1924, 0.38).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t2024, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t610, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 610, 0.25).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t661, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 661, 0.35).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t750, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 750, 0.41).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t900, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 900, 0.42).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t1258, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1258, 0.42).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t1517, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1517, 0.42).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t1924, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1924, 0.42).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t2024, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__ishmael_covenant_reading, 0.08).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__land_promise_constraint).

% DUAL FORMULATION NOTE:
% Part of the abrahamic_covenant constraint family. This reading (ishmael_covenant_reading) expands the beneficiary set to include the Islamic umma and validates Muhammad as prophetic seal. The isaac_covenant_reading restricts covenant to Isaac's line. The christian_supersessionist_reading claims the Church supersedes both lineages. The land_promise_constraint isolates the territorial grant as a separable covenantal component. All four decompose the 'Abrahamic covenant' label into structurally distinct claims with different ε, beneficiaries, and enforcement structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__ishmael_covenant_reading, institutional, 0.1).
constraint_indexing:directionality_override(abrahamic_covenant__ishmael_covenant_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
