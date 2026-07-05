% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Isaac-Exclusive Covenant Reading (Genesis 17:19-21)
 *   domain: religious/theological/institutional
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Abrahamic covenant
 *   kernel: the interpretation of Genesis 17:19-21 as transmitting the
 *   covenant exclusively through Isaac, with Ishmael explicitly and
 *   permanently excluded despite receiving his own divine blessing in the
 *   same passage. This reading has functioned within rabbinic Jewish
 *   tradition (and, refracted through supersessionist logic, within much of
 *   Christian tradition) as the textual anchor for a claim of singular
 *   covenantal chosenness. It is authored here as its own ε-invariant
 *   constraint, structurally distinct from the sibling reading in which the
 *   covenant passes through Ishmael to Muhammad, and from the separate
 *   land-promise constraint concerning territorial grant. Extraction is
 *   substantial and rising over the interval because the exclusivity claim
 *   does not remain a private theological position — it has been mobilized,
 *   particularly in later centuries of interfaith polemic and political
 *   rhetoric, to delegitimize rival Abrahamic claimants entirely rather than
 *   merely to organize internal communal identity.
 *
 * KEY AGENTS:
 *   - rabbinic_jewish_institutional_authority: primary agenda-setter and beneficiary (institutional/identity_locked) — transmits and defends the exclusive reading
 *   - isaac_lineage_claimants: beneficiary (organized/identity_locked) — communal identity constituted through the reading
 *   - ishmaelite_claimants: primary textual victim (powerless/trapped) — named and blessed in the same passage, then excluded from covenantal status by interpretation
 *   - islamic_prophetic_tradition: institutional victim (organized/constrained) — later tradition whose prophetic succession claim is delegitimized by this reading circulating outside its own canon
 *   - comparative_theologians: analytical observer (analytical/analytical) — documents the structural function of the exclusivity claim across traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.68).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.58).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Isaac-Exclusive Covenant Reading (Genesis 17:19-21)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/theological/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, 'f2183b79-43fc-4c01-af6f-f00a5e0360ea').
narrative_ontology:cs_kernel_codification('f2183b79-43fc-4c01-af6f-f00a5e0360ea', fixed_text).
narrative_ontology:cs_authority_grounding('f2183b79-43fc-4c01-af6f-f00a5e0360ea', lineage).
narrative_ontology:cs_interpretation_layer_present('f2183b79-43fc-4c01-af6f-f00a5e0360ea').
narrative_ontology:cs_reading_relation('f2183b79-43fc-4c01-af6f-f00a5e0360ea', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2183b79-43fc-4c01-af6f-f00a5e0360ea', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('f2183b79-43fc-4c01-af6f-f00a5e0360ea', foundational, isaac_line_covenantal_singularity).
narrative_ontology:cs_axiom_status(isaac_line_covenantal_singularity, holdable).
narrative_ontology:cs_axiom_grounding('f2183b79-43fc-4c01-af6f-f00a5e0360ea', isaac_line_covenantal_singularity, conventional).
narrative_ontology:cs_axiom('f2183b79-43fc-4c01-af6f-f00a5e0360ea', secondary, ishmael_blessing_subordinate_to_covenant).
narrative_ontology:cs_axiom_status(ishmael_blessing_subordinate_to_covenant, holdable).
narrative_ontology:cs_axiom_grounding('f2183b79-43fc-4c01-af6f-f00a5e0360ea', ishmael_blessing_subordinate_to_covenant, conventional).
narrative_ontology:cs_reference_frame('f2183b79-43fc-4c01-af6f-f00a5e0360ea', second_temple_covenant_boundary_consensus).
narrative_ontology:cs_drift_state('f2183b79-43fc-4c01-af6f-f00a5e0360ea', contemporary_interfaith_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f2183b79-43fc-4c01-af6f-f00a5e0360ea', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_authority).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, isaac_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, christian_typological_readers).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, christian_typological_readers).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, isaac_line_covenantal_singularity).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, genesis_17_19_exclusivity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits, canonizes, and teaches the reading of Genesis 17:19-21 as textually exclusive to Isaac's line. This reading is load-bearing for the continuity of Jewish peoplehood as covenantally singular; the institution's own legitimacy and self-understanding are constituted through defending this exclusivity claim against rival readings.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_authority, beneficiary).

% Understand themselves as the sole covenantal heirs of the Abrahamic promise. This reading grounds their claim to unique chosenness, historical land promise continuity, and religious distinctiveness; abandoning the exclusive reading would require reconstituting core elements of communal identity.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, isaac_lineage_claimants, beneficiary,
    organized, civilizational, identity_locked, global).

% Named in the same founding text as Abraham's firstborn son and recipient of a divine blessing (Genesis 17:20), but read out of the covenantal line by this interpretation. They cannot exit the constraint because it is embedded in the authoritative canon of a tradition they do not control; the exclusion is asserted about them, not negotiated with them.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants, payer,
    powerless, civilizational, trapped, global).

% Holds that the Abrahamic covenant and prophetic succession pass through Ishmael to Muhammad. The Isaac-exclusive reading, where it circulates in interfaith, legal, or political contexts, delegitimizes this succession claim at its root; the tradition can and does contest the reading in its own texts and scholarship, but cannot alter what the Genesis text says or how it is read within Jewish and Christian institutions.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition, payer,
    organized, civilizational, constrained, global).

% Draw on the Isaac-exclusive reading as a premise for typological readings of covenant narrowing toward a chosen line, while also relativizing it through supersessionist claims that the line passes through faith rather than bloodline. They benefit from the exclusivity logic structurally while contesting its terminus.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, christian_typological_readers, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, christian_typological_readers, payer).

% Study how the same verses (Genesis 17:19-21) generate mutually exclusive covenantal claims across traditions. They document the structural function of the exclusivity reading without being bound by any tradition's authority to adjudicate which reading is correct.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, comparative_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable genealogical answer to 'who carries the covenant forward,' allowing a religious community to coordinate identity, ritual practice, and communal boundary-setting around one unambiguous line of descent rather than a contested or plural one.
% TRANSFER_FUNCTION: Moves symbolic and institutional capital — covenantal legitimacy, chosenness status, and interpretive authority over the shared Abrahamic narrative — from Ishmael's line and its later religious heirs to Isaac's line and its later religious heirs.
% ABSENT_VOICES: Ishmaelite claimants within the text itself have no voice in how later communities interpret their exclusion; the Islamic tradition that identifies Ishmael as ancestor of Muhammad is not a party to the canon-formation or councils that fixed the Isaac-exclusive reading, and encounters the reading already settled within a scriptural authority it does not control.
% DISAPPEARANCE_RATIONALE: If the exclusive reading were dropped in favor of an inclusive or ambiguous reading, the theological ground for singular Jewish covenantal chosenness would need re-articulation, interfaith polemics over 'true' Abrahamic succession would lose a key proof-text, and centuries of doctrinal, liturgical, and political material built on the exclusivity claim (including modern political rhetoric about land and legitimacy) would require reinterpretation.
% FOUNDING_PROBLEM: Ancient Israelite communities needed a textual warrant for understanding themselves as the specific, singular recipients of the Abrahamic promise, distinguishing their covenantal status from neighboring peoples who also traced descent from Abraham.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic and traditional Jewish sources attest the reading as settled scriptural fact rather than a constructed boundary-solution; historical-critical biblical scholars (working outside any confessional beneficiary community) attest that the exclusivity framing reflects later redactional and communal-identity concerns rather than a self-evidently singular textual meaning, and Islamic scholarship independently attests a rival transmission claim from the same textual base — both outside-the-benefiting-parties readings treat the 'exclusivity' as an interpretive choice, not a discovered fact.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is substantial: the reading does more than answer an internal identity question — it produces a durable status hierarchy over people who are not party to the canon that defines them. Suppression (0.58) is moderate-high: the constraint's persistence depends on scriptural canonization and institutional transmission across millennia rather than on Ishmaelite or Islamic assent, though it is not enforced by direct coercive apparatus in the way a legal statute is. Theater ratio is low (0.22, rising modestly) — the reading performs real communal-boundary work rather than being predominantly performative, though its use in modern polemical and political contexts introduces a growing performative component. Accessibility collapse (0.6) reflects that once canonized within a tradition, the alternative reading becomes very difficult to hold from inside that tradition's own authoritative texts, though it remains fully visible and contestable from outside (hence not near-mountain levels). Resistance (0.62) is substantial because both the excluded Ishmaelite line's later tradition (Islam) and academic historical-critical scholarship actively contest the exclusivity claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic Jewish institutional authority and Isaac-lineage claimants sit at the beneficiary end: the reading is the textual ground of their communal self-understanding as singularly covenanted, and they administer and transmit it. Ishmaelite claimants sit at the full-target end: they are named favorably in the same verses, then read out by the same interpretive act, with no voice in the canon-formation process and no exit from a text they do not control. The Islamic prophetic tradition is a target at one remove: it does not appear in Genesis at all, but its own foundational succession claim depends on Ishmael's line being covenantally live, so the Isaac-exclusive reading directly negates that tradition's central genealogical warrant wherever the two traditions' texts are read comparatively.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a Mandatrophy case in the classic sense (a mandate that outlived its function) — the founding problem (distinguishing a specific community's covenantal self-understanding from neighboring Abrahamic-descended peoples) remains structurally live for the community that maintains the reading. What the classification prevents is treating the reading as a neutral, cost-free coordination mechanism: it does coordinate identity for one community, but it does so by asserting a status verdict about another community's core lineage, which is where the tangled_rope (not pure rope) classification comes from — genuine coordination function plus asymmetric extraction through the same textual structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_exclusivity_vs_constructed_boundary,
    'Does Genesis 17:19-21 itself assert covenantal exclusivity for Isaac, or does later communal and institutional interpretation construct exclusivity from a text that grants Ishmael a real, if different, blessing without explicitly foreclosing covenantal status?',
    'Historical-critical and redaction-critical analysis of the Genesis text''s compositional layers, compared against the earliest attested interpretive traditions (Second Temple period sources, early rabbinic midrash) to determine whether exclusivity is present in the earliest strata or emerges later as communities needed a boundary-marking device.',
    'If exclusivity is a later interpretive imposition rather than an original textual feature, the reading functions more clearly as a constructed identity-boundary mechanism (supporting the tangled_rope/extraction reading); if the earliest strata already assert exclusivity, the coordination function is more textually grounded and the extraction component is better understood as a later accretion rather than intrinsic to the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_exclusivity_vs_constructed_boundary, empirical, 'Whether covenantal exclusivity is original to the text or a later interpretive construction.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the isaac_covenant_reading and ishmael_covenant_reading genuinely incommensurable within their respective traditions'' own interpretive frameworks, or is there a coherent meta-framework (e.g., comparative religious studies, or a pluralist theological reading) in which both readings could be held as partial truths about a genuinely ambiguous or dual-blessing text?',
    'Survey of contemporary interfaith theological scholarship and pluralist Abrahamic-faith initiatives to determine whether any coherent framework has been proposed and accepted (even partially) by authorities within either tradition that holds both readings as compatible rather than mutually exclusive.',
    'If no coherent meta-framework exists and is accepted by authorities in either tradition, the two readings genuinely foreclose one another wherever compared, which would argue for a ''forecloses'' rather than ''coexists_with'' relation between the readings; existing evidence of practical (if not doctrinal) coexistence across the two traditions'' communities without formal theological reconciliation supports the ''coexists_with'' choice made here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the sibling covenant readings are logically incommensurable or merely competing within an unreconciled but non-foreclosing space.').

omega_variable(
    framing_choice_institution_vs_legitimacy_claim,
    'Should this constraint be framed as the institutional transmission practice (how rabbinic authority teaches and canonizes the reading) or as the underlying legitimacy claim itself (the propositional content that Isaac alone inherits the covenant, independent of any particular institution''s transmission of it)?',
    'Compare classification outcomes under each framing: institutional-transmission framing centers agenda_setter power and enforcement mechanisms (councils, canon-formation, communal discipline); legitimacy-claim framing centers the propositional content''s persistence independent of any single institution, closer to a distributed/diffuse authority pattern.',
    'The institutional framing (adopted here) yields authority_grounding=''lineage'' and supports the tangled_rope classification via active transmission and enforcement; a legitimacy-claim framing might yield ''distributed'' authority grounding with lower measured suppression, since no single institution fully controls the proposition''s circulation once it exists in shared scripture. This story adopts the institutional-transmission framing because the beneficiary/victim structure (who administers vs. who is excluded) is clearest under that framing; the alternative framing is noted here as an unresolved conceptual choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_institution_vs_legitimacy_claim, conceptual, 'Alternative framings of this constraint (institutional practice vs. propositional legitimacy claim) that would shift the cs_structure authority_grounding value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(abra_tr_t0, projected).
narrative_ontology:measurement(abra_tr_t500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement_basis(abra_tr_t500, projected).
narrative_ontology:measurement(abra_tr_t1000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1000, 0.14).
narrative_ontology:measurement_basis(abra_tr_t1000, projected).
narrative_ontology:measurement(abra_tr_t1500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1500, 0.16).
narrative_ontology:measurement_basis(abra_tr_t1500, projected).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement_basis(abra_tr_t2000, projected).
narrative_ontology:measurement(abra_tr_t2500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2500, 0.2).
narrative_ontology:measurement_basis(abra_tr_t2500, projected).
narrative_ontology:measurement(abra_tr_t3000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 3000, 0.22).
narrative_ontology:measurement_basis(abra_tr_t3000, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(abra_be_t0, projected).
narrative_ontology:measurement(abra_be_t500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 500, 0.55).
narrative_ontology:measurement_basis(abra_be_t500, projected).
narrative_ontology:measurement(abra_be_t1000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement_basis(abra_be_t1000, projected).
narrative_ontology:measurement(abra_be_t1500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1500, 0.62).
narrative_ontology:measurement_basis(abra_be_t1500, projected).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement_basis(abra_be_t2000, projected).
narrative_ontology:measurement(abra_be_t2500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2500, 0.67).
narrative_ontology:measurement_basis(abra_be_t2500, projected).
narrative_ontology:measurement(abra_be_t3000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 3000, 0.68).
narrative_ontology:measurement_basis(abra_be_t3000, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(abra_su_t0, projected).
narrative_ontology:measurement(abra_su_t500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 500, 0.45).
narrative_ontology:measurement_basis(abra_su_t500, projected).
narrative_ontology:measurement(abra_su_t1000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1000, 0.48).
narrative_ontology:measurement_basis(abra_su_t1000, projected).
narrative_ontology:measurement(abra_su_t1500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement_basis(abra_su_t1500, projected).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2000, 0.53).
narrative_ontology:measurement_basis(abra_su_t2000, projected).
narrative_ontology:measurement(abra_su_t2500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2500, 0.56).
narrative_ontology:measurement_basis(abra_su_t2500, projected).
narrative_ontology:measurement(abra_su_t3000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 3000, 0.58).
narrative_ontology:measurement_basis(abra_su_t3000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__isaac_covenant_reading, 0.1).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of at least three linked readings of the abrahamic_covenant kernel. isaac_covenant_reading (this story) and ishmael_covenant_reading share the identical source text (Genesis 17:19-21) but produce structurally opposite beneficiary/victim assignments — each names the other tradition's founding claim as excluded. christian_supersessionist_reading is a further downstream reading that relativizes both bloodline claims through a faith-based covenant model, which influences (without foreclosing) both bloodline readings by shifting the terms of legitimacy from genealogy to faith. land_promise_constraint is a structurally adjacent but distinct kernel-territory concerning whether the same covenant includes a conditional or unconditional territorial grant; it is affected by (but not identical to) the transmission-line dispute this story concerns, since who counts as covenantal heir bears on who can claim the associated land promise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
