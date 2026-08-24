% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Ishmael Covenant Reading — Inclusive Abrahamic Lineage through Muhammad
 *   domain: religious/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   This constraint story models the Ishmael Covenant Reading — the Islamic
 *   interpretive claim that the Abrahamic covenant continues through Ishmael
 *   to Muhammad, interpreting the Genesis promise as inclusive rather than
 *   exclusive. The reading functions as a legitimacy constraint: it
 *   coordinates Muslim communal identity around prophetic succession while
 *   extracting Abrahamic authority from competing Jewish (Isaac-exclusive)
 *   and Christian (supersessionist) readings. The claimed type is
 *   tangled_rope — genuine coordination of Muslim identity combined with
 *   asymmetric extraction of legitimating authority from the Abrahamic
 *   textual tradition. The engine will compute per-seat classifications from
 *   the structural data; the divergence between the agenda-setter seat
 *   (Islamic tradition) and payer seats (exclusivist claimants) is the
 *   measurement target.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.45).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.55).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmael Covenant Reading — Inclusive Abrahamic Lineage through Muhammad").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, 'e529ddce-f7f4-48f0-97a3-c0132588032b').
narrative_ontology:cs_kernel_codification('e529ddce-f7f4-48f0-97a3-c0132588032b', fixed_text).
narrative_ontology:cs_authority_grounding('e529ddce-f7f4-48f0-97a3-c0132588032b', lineage).
narrative_ontology:cs_interpretation_layer_present('e529ddce-f7f4-48f0-97a3-c0132588032b').
narrative_ontology:cs_reading_relation('e529ddce-f7f4-48f0-97a3-c0132588032b', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('e529ddce-f7f4-48f0-97a3-c0132588032b', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e529ddce-f7f4-48f0-97a3-c0132588032b', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_axiom('e529ddce-f7f4-48f0-97a3-c0132588032b', foundational, covenant_includes_ishmael_line).
narrative_ontology:cs_axiom_status(covenant_includes_ishmael_line, holdable).
narrative_ontology:cs_axiom_grounding('e529ddce-f7f4-48f0-97a3-c0132588032b', covenant_includes_ishmael_line, deontological).
narrative_ontology:cs_axiom('e529ddce-f7f4-48f0-97a3-c0132588032b', foundational, muhammad_as_final_prophet).
narrative_ontology:cs_axiom_status(muhammad_as_final_prophet, holdable).
narrative_ontology:cs_axiom_grounding('e529ddce-f7f4-48f0-97a3-c0132588032b', muhammad_as_final_prophet, deontological).
narrative_ontology:cs_axiom('e529ddce-f7f4-48f0-97a3-c0132588032b', secondary, abrahamic_lineage_inclusivity).
narrative_ontology:cs_axiom_status(abrahamic_lineage_inclusivity, holdable).
narrative_ontology:cs_axiom_grounding('e529ddce-f7f4-48f0-97a3-c0132588032b', abrahamic_lineage_inclusivity, conventional).
narrative_ontology:cs_reference_frame('e529ddce-f7f4-48f0-97a3-c0132588032b', abrahamic_covenant_inclusive_lineage).
narrative_ontology:cs_drift_state('e529ddce-f7f4-48f0-97a3-c0132588032b', contemporary_interfaith_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('e529ddce-f7f4-48f0-97a3-c0132588032b', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, muslim_umma).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_interpretive_tradition).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, prophetic_lineage_through_ishmael).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, isaac_line_exclusivists).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_claimants).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, ishmael_covenant_continuity).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, muhammad_as_seal_of_prophets).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, abrahamic_lineage_inclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The global Muslim community derives its collective identity and legitimating genealogy from this reading. The covenant through Ishmael to Muhammad anchors Islamic self-understanding as the restored Abrahamic community. Exit from this identity framework is experienced as apostasy — religiously, socially, and in many jurisdictions legally consequential.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, muslim_umma, beneficiary,
    organized, generational, identity_locked, global).

% The scholarly and juridical tradition (tafsir, sira, fiqh, kalam) that administers, transmits, and defends this reading. It sets the interpretive boundaries, declares orthodoxy, and benefits from the institutional authority the reading confers. Individual scholars may dissent at the margins but the tradition as a whole enforces the reading's coherence.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_interpretive_tradition, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, islamic_interpretive_tradition, beneficiary).

% The genealogical claim itself — that the covenant promise runs through Ishmael to Muhammad — functions as a non-agent beneficiary in the sense that the reading's operation vindicates this proposition. It collects no rents but its vindication is the reading's core coordination function.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, prophetic_lineage_through_ishmael, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(abrahamic_covenant__ishmael_covenant_reading, prophetic_lineage_through_ishmael).

% Jewish tradition bearers who hold the Isaac-exclusive reading (Genesis 17:19-21). This reading structurally challenges their exclusive covenantal claim, extracting the uniqueness of their lineage-based legitimacy. They cannot exit the contest — the Ishmael reading's global presence makes the exclusivity claim a live dispute.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, isaac_line_exclusivists, payer,
    organized, generational, constrained, global).

% Christian theological traditions that claim the Church supersedes both Jewish and Islamic covenantal claims. The Ishmael reading contests their supersessionist narrative by asserting a post-Christian prophetic line that also claims Abrahamic continuity. They bear the cost of a three-way legitimacy contest rather than a binary one.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_claimants, payer,
    institutional, generational, constrained, global).

% Beyond the exclusivist claimants, the broader Jewish tradition — including non-exclusivist and secular Jewish voices — is structurally excluded from the Islamic interpretive conversation that produces this reading. They would object to the genealogical reassignment of Ishmael but have no seat in the isnad/tafsir process.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_tradition_bearers, excluded,
    organized, generational, constrained, global).

% Scholars of comparative religion, early Islamic history, and Abrahamic studies who analyze the reading's formation, function, and contestation from outside the commitments. They neither collect nor pay but their work shapes the inter-traditional discourse in which the reading's legitimacy is negotiated.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, academic_comparative_theologians, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the Muslim community around a validated prophetic succession from Abraham through Ishmael to Muhammad, resolving the problem of legitimate authority after the biblical prophets by anchoring Islamic identity in the Abrahamic covenant itself.
% TRANSFER_FUNCTION: Moves legitimating authority from the Genesis covenant promise to the Islamic prophetic line, granting the Muslim community sole valid inheritance of the Abrahamic covenant and authorizing the Islamic interpretive tradition as its exclusive custodian.
% ABSENT_VOICES: Jewish tradition bearers (exclusivist and non-exclusivist), Christian supersessionist theologians, secular historians of early Islam, and pre-Islamic Arabian oral tradition bearers — they are structurally excluded from the Islamic interpretive conversation (isnad/tafsir/sira) that produces and maintains this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, Islamic prophetic legitimacy would lose its Abrahamic anchoring, the Muslim community's self-understanding as the restored Abrahamic community would collapse, the Islamic interpretive tradition would lose its foundational genealogical claim, and the tripartite Abrahamic legitimacy contest would revert to a binary Jewish-Christian dispute.
% FOUNDING_PROBLEM: The problem of legitimate prophetic succession after the biblical period — who carries forward the Abrahamic covenant when the Israelite prophetic line ends, and how is post-biblical prophecy validated without breaking the covenantal chain?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by early Islamic historical sources (Ibn Ishaq's Sira, al-Tabari's Tarikh) which frame Muhammad's mission explicitly as the answer to the Abrahamic succession problem, and recognized by non-Muslim scholars of comparative religion (Wensinck, Rubin, Crone, Hoyland) as the structuring problem of early Islamic identity formation. The Islamic tradition itself attests the problem is live; external scholarship corroborates the framing without endorsing the reading's truth claim.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__ishmael_covenant_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) reflects moderate but real extraction of legitimating authority from the shared Abrahamic textual pool — the reading claims the entire covenant for one lineage. Suppression (0.55) reflects active enforcement within Islamic orthodoxy (isnad criticism, takfir mechanisms, institutional fatwa bodies) and the reading's structural exclusion of competing genealogies. Theater ratio (0.25) is low: the prophetic succession is functionally real for believers, not performative. Accessibility collapse (0.65) is moderate: alternative readings exist but are suppressed within the Islamic epistemic sphere. Resistance (0.55) is moderate: competing readings persist globally and the tripartite contest is live.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this is genuine coordination (rope-like) solving the succession problem. From the payer seats, the same structure operates as extraction of their covenantal uniqueness. The engine computes this divergence from the declared structural data — the claimed type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Islamic interpretive tradition (agenda_setter) sits near the beneficiary end (d ~ 0.15) — it administers the reading and collects institutional authority. The Muslim umma (beneficiary, identity_locked) sits at strong beneficiary (d ~ 0.10) — the reading constitutes its core identity. Isaac-line exclusivists and Christian supersessionists (payers) sit at strong target (d ~ 0.85-0.90) — their exclusive/supersessionist claims are structurally extracted. Jewish tradition bearers (excluded) sit at target (d ~ 0.80) — they bear the genealogical reassignment without a seat. Academic observers sit at analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate post-biblical prophecy) remains live within Islamic theology and the inter-Abrahamic contest. The reading has not atrophied into piton — it actively coordinates Muslim identity and its enforcement machinery (orthodoxy maintenance) is functional, not theatrical. The moderate theater ratio rise over time reflects institutionalization of interpretive authority, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_committer_structure,
    'How does this reading''s structural relationship to the abrahamic_covenant kernel differ from its siblings, and where is the disagreement located?',
    'Comparative analysis of the three readings'' genealogical claims, validation mechanisms, and beneficiary structures using the constraint story format for each sibling reading.',
    'If the sibling readings produce different ε values and classification types, this confirms the kernel decomposes into distinct constraints per the ε-invariance principle. If they converge, the kernel may be a single constraint with observer-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committers frame: this reading''s structural delta from siblings — genealogical line, validation mechanism, beneficiary set').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readings within the Islamic tradition structural (institutional enforcement, legal penalties) or internalized (identity-fused belief that the reading is self-evidently true)?',
    'Post-exit trajectory study: track suppression levels for individuals who leave the Islamic tradition — if suppression persists as internalized epistemic closure, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them after exit, affecting the identity_locked exit classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in Islamic orthodoxy maintenance').

omega_variable(
    coordination_extraction_boundary,
    'Is the reading''s coordination function (unifying Muslim identity) structurally separable from its extraction function (claiming sole Abrahamic legitimacy), or are they co-constitutive?',
    'Counterfactual analysis: if the reading retained its internal coordination function but renounced exclusive Abrahamic legitimacy (accepting parallel valid lineages), would the coordination survive? Historical test: Sufi perennialist and modern pluralist Islamic movements that attempt this separation.',
    'If separable, the extraction is contingent and the reading could evolve toward rope; if co-constitutive, the extraction is essential to the coordination and tangled_rope is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether coordination and extraction are structurally separable in this reading').

omega_variable(
    cs_framing_underdetermination,
    'Does the Islamic interpretive tradition constitute a single authority_grounding (lineage) with interpretation_layer_present=true, or are there competing authority groundings (caliphal-political vs scholarly-epistemic vs mystical-experiential) within the tradition?',
    'Historical-sociological analysis of authority contests within Islamic history (caliphate vs ulama vs sufis, Sunni vs Shia authority structures) to determine if a single authority_grounding atom suffices.',
    'If multiple groundings exist, the cs_structure declaration oversimplifies; the constraint may need decomposition into sub-readings per authority structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether authority_grounding=lineage adequately captures intra-Islamic authority pluralism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t0, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t100, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t200, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t400, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 400, 0.18).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t600, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t800, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 800, 0.22).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t1000, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1000, 0.23).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t1200, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1200, 0.24).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_tr_t1400, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1400, 0.25).

% Extraction over time
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t0, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t100, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 100, 0.3).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t200, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t400, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t600, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 600, 0.4).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t800, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 800, 0.42).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t1000, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1000, 0.43).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t1200, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1200, 0.44).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_be_t1400, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1400, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t0, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t100, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t200, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t400, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 400, 0.5).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t600, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 600, 0.52).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t800, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 800, 0.53).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t1000, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1000, 0.54).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t1200, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1200, 0.55).
narrative_ontology:measurement(abrahamic_covenant__ishmael_covenant_reading_su_t1400, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1400, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__ishmael_covenant_reading, 0.08).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, islamic_prophetic_authority).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_land_promise).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__christian_supersessionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the abrahamic_covenant constraint family. The kernel decomposes into at least three readings with distinct ε values: isaac_covenant_reading (ε ~ 0.2, claimed mountain/rope), ishmael_covenant_reading (ε ~ 0.45, claimed tangled_rope), christian_supersessionist_reading (ε ~ 0.5, claimed tangled_rope/snare). The land_promise_constraint interacts with all three via territorial legitimacy claims. Network edges reflect structural influence: this reading's legitimacy claim influences the land promise constraint's Islamic interpretation (waqf, dar al-islam), and is influenced by the Isaac reading's textual priority in the shared Genesis source.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__ishmael_covenant_reading, institutional, 0.15).
constraint_indexing:directionality_override(abrahamic_covenant__ishmael_covenant_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
