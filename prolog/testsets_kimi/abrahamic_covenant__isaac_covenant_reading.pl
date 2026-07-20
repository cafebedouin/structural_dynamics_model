% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Isaac-Exclusive Abrahamic Covenant Reading
 *   domain: religious/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   This constraint story models the interpretive tradition that reads
 *   Genesis 17:19-21 as limiting the Abrahamic covenant exclusively to
 *   Isaac's line, thereby excluding Ishmael and his claimed descendants. The
 *   reading functions as an identity-coordination mechanism for Jewish
 *   continuity while structurally extracting theological legitimacy from
 *   Ishmaelite claimants and the Islamic tradition. Key agents include the
 *   rabbinic authority that administers the interpretation, the Jewish
 *   communal identity that receives boundary definition, and the Ishmaelite
 *   and Islamic seats that bear the cost of exclusion.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: agenda_setter (institutional/constrained) â interprets and enforces the exclusive lineage
 *   - jewish_communal_identity: beneficiary (organized/identity_locked) â receives covenantal continuity and boundary
 *   - ishmaelite_claimants: payer (powerless/trapped) â excluded from covenantal legitimacy
 *   - islamic_tradition: payer (institutional/identity_locked) â bears delegitimation of prophetic genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.78).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.72).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Isaac-Exclusive Abrahamic Covenant Reading").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '0885d18b-fbc4-4a35-ad99-b3b8f9152463').
narrative_ontology:cs_kernel_codification('0885d18b-fbc4-4a35-ad99-b3b8f9152463', fixed_text).
narrative_ontology:cs_authority_grounding('0885d18b-fbc4-4a35-ad99-b3b8f9152463', lineage).
narrative_ontology:cs_interpretation_layer_present('0885d18b-fbc4-4a35-ad99-b3b8f9152463').
narrative_ontology:cs_reading_relation('0885d18b-fbc4-4a35-ad99-b3b8f9152463', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('0885d18b-fbc4-4a35-ad99-b3b8f9152463', abrahamic_covenant__christian_supersessionist_reading, influences).
narrative_ontology:cs_reading_relation('0885d18b-fbc4-4a35-ad99-b3b8f9152463', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_axiom('0885d18b-fbc4-4a35-ad99-b3b8f9152463', foundational, isaac_exclusive_lineage).
narrative_ontology:cs_axiom_status(isaac_exclusive_lineage, holdable).
narrative_ontology:cs_axiom_grounding('0885d18b-fbc4-4a35-ad99-b3b8f9152463', isaac_exclusive_lineage, theological).
narrative_ontology:cs_axiom('0885d18b-fbc4-4a35-ad99-b3b8f9152463', foundational, ishmael_blessing_without_covenant).
narrative_ontology:cs_axiom_status(ishmael_blessing_without_covenant, holdable).
narrative_ontology:cs_axiom_grounding('0885d18b-fbc4-4a35-ad99-b3b8f9152463', ishmael_blessing_without_covenant, theological).
narrative_ontology:cs_reference_frame('0885d18b-fbc4-4a35-ad99-b3b8f9152463', isaac_exclusive_covenant_framework).
narrative_ontology:cs_drift_state('0885d18b-fbc4-4a35-ad99-b3b8f9152463', post_islamic_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0885d18b-fbc4-4a35-ad99-b3b8f9152463', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_communal_identity).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits Genesis 17 through rabbinic tradition, ruling that covenantal sign, obligation, and promise flow exclusively through Isaac. Maintains circumcision as a boundary marker and adjudicates lineage legitimacy. The authority of this seat depends on the exclusivity claim; exit means relinquishing the interpretive gatekeeper role and its associated institutional standing.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).

% Receives covenantal continuity, communal boundary definition, and theological distinctiveness through the Isaac-exclusive reading. The exclusivity underwrites a unique Abrahamic status and land-promise inheritance. Exit from this constraint means assimilating out of the covenantal identity framework, which dissolves the self-understanding the arrangement sustains.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_communal_identity, beneficiary,
    organized, generational, identity_locked, global).

% Claim Abrahamic lineage and covenantal blessing through Ishmael, but are structurally excluded from the covenant's promises by the Isaac-exclusive interpretation. They lack standing within the rabbinic interpretive framework to contest the reading, and their genealogical claims are ruled illegitimate by the authoritative tradition.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants, payer,
    powerless, generational, trapped, global).

% Grounds prophetic legitimacy in Ishmaelite lineage and Quranic reaffirmation of Abrahamic inheritance. The Isaac-exclusive reading delegitimizes this foundation by restricting the covenant to Isaac's line. Islamic tradition mounts robust theological counter-readings but cannot alter the Jewish interpretive authority that enforces the exclusion, and abandoning the Ishmaelite claim would dissolve Islamic prophetic theology.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_tradition, payer,
    institutional, civilizational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains communal identity, theological continuity, and intergenerational boundary clarity for the Jewish people by anchoring covenantal legitimacy in a specific patriarchal lineage and a single authorized interpretive tradition.
% TRANSFER_FUNCTION: Moves covenantal legitimacy, land-promise entitlement, and prophetic inheritance from Abraham's broader descendants to Isaac's exclusive line, withholding these goods from Ishmaelite claimants and the Islamic tradition that grounds itself in Ishmael.
% ABSENT_VOICES: Ishmaelite claimants and the Islamic interpretive tradition are structurally excluded from the rabbinic discourse that adjudicates covenantal legitimacy; their Quranic counter-readings and genealogical claims are not admitted as valid interpretive inputs within the classical Jewish framework.
% DISAPPEARANCE_RATIONALE: If the Isaac-exclusive reading vanished, Jewish communal boundary mechanisms would lose a primary genealogical anchor, necessitating a shift to other identity markers or inclusive covenantal frameworks; the Islamic tradition's Ishmaelite claims would gain theological standing within Abrahamic discourse, and the land-promise question would reopen along different lineage lines.
% FOUNDING_PROBLEM: Maintaining communal cohesion and divine-promise fidelity during and after the Babylonian exile, when multiple lineagesâIsraelite, Samaritan, and Ishmaiteâclaimed Abrahamic heritage and threatened the group's theological distinctiveness.
% FOUNDING_PROBLEM_CORROBORATION: Biblical critical scholars and ancient Near Eastern historians attest that exclusive lineage consolidation served post-exilic ethnic boundary formation. Islamic tradition and Ishmaelite genealogical narratives attest that the problem was solved by exclusion rather than divine necessity. External corroboration comes from comparative studies showing covenantal exclusivity as a standard ethnic boundary strategy in the ancient Near East.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the reading structurally withholds covenantal legitimacy, land promise, and prophetic inheritance from a major rival lineage. Suppression is high (0.72) because the constraint requires active interpretive exclusion of Quranic and Ishmaelite counter-readings to persist. Theater ratio is moderate (0.40): much rabbinic labor is genuine identity maintenance, but a significant share is performative boundary-policing against the Islamic counter-claim. Accessibility collapse is moderate (0.45) because the Islamic tradition constitutes a robust, globally present alternative that prevents full collapse. Resistance is high (0.70) because the Islamic tradition mounts organized, civilizational resistance to the exclusivity claim. Temporal measurements show extraction rising through the rabbinic and medieval periods, dipping slightly during the Enlightenment, and rising again under modern nationalism.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority seat experiences the constraint as necessary identity coordination preserving a people across exile; the Jewish communal identity seat experiences both the benefit of boundary and the cost of identity lock-in. The Ishmaelite and Islamic seats experience the identical structure as enforced theological extraction and delegitimation. The engine computes these divergent seat classifications from the same structural data: beneficiary/victim declarations, identity-locked exits, and global scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and Jewish communal identity sit near the beneficiary end because the constraint subsidizes their authority and identity continuity. Ishmaelite claimants and Islamic tradition sit near the full-target end because the constraint extracts legitimacy and standing from them. Islamic tradition is institutional and global, but its identity-locked exit and declared victim status override power and scope dampening, pushing its effective extraction upward. The global scope of the Abrahamic claim amplifies extraction because verification of lineage legitimacy is distributed and difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a mountain (divine natural law) by documenting active enforcement, identifiable beneficiaries, and identifiable victims. It also prevents mislabeling as a pure snare by acknowledging the genuine coordination function the reading serves for Jewish communal identity across diaspora. Tangled Rope captures the hybridity: the same lineage mechanism that coordinates the in-group also extracts from the out-group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_exclusivity_ontology,
    'Is the exclusivity of the Isaac covenant an ontological feature of divine revelation, or a historically contingent interpretive construction of the Second Temple and Rabbinic periods?',
    'Textual criticism, redaction criticism, and historical analysis of Genesis 17''s compositional layers; comparison with ancient Near Eastern covenantal forms.',
    'If the exclusivity is shown to be a contingent construction, the constraint shifts toward snare or piton (extractive cover story); if ontological, it remains a high-extraction commitment system with theological grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_exclusivity_ontology, conceptual, 'Whether Isaac exclusivity is divine fact or constructed interpretation').

omega_variable(
    ishmaelite_exclusion_mechanism,
    'Is the exclusion of Ishmael maintained primarily by institutional interpretive authority, or by the logical structure of the Genesis text itself?',
    'Analysis of the relationship between plain-sense reading and interpretive tradition; comparison of pre-rabbinic, rabbinic, and modern readings.',
    'If institutional authority is the primary mechanism, the constraint''s suppression score reflects active enforcement; if textual determinacy, the constraint moves toward mountain-like immunity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ishmaelite_exclusion_mechanism, conceptual, 'Institutional enforcement vs textual determinacy of Ishmael exclusion').

omega_variable(
    sibling_reading_foreclosure,
    'Does the Isaac-exclusive reading genuinely foreclose the Ishmaelite-inclusive reading, or can the two be reconciled within a single pluralist theological framework?',
    'Interfaith hermeneutical analysis and examination of theological frameworks that attempt to hold both lineages as covenantally valid.',
    'If reconcilable, the forecloses relation in reading_relations is incorrect and the constraint is less structurally extractive than modeled; if mutually exclusive, the current reading_relations hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between Isaac-exclusive and Ishmaelite-inclusive readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isaac_covenant_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(isaac_covenant_tr_t5, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(isaac_covenant_tr_t10, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(isaac_covenant_tr_t15, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(isaac_covenant_tr_t20, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(isaac_covenant_tr_t25, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement(isaac_covenant_tr_t30, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(isaac_covenant_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(isaac_covenant_be_t5, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(isaac_covenant_be_t10, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(isaac_covenant_be_t15, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(isaac_covenant_be_t20, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(isaac_covenant_be_t25, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(isaac_covenant_be_t30, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(isaac_covenant_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(isaac_covenant_su_t5, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(isaac_covenant_su_t10, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(isaac_covenant_su_t15, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(isaac_covenant_su_t20, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(isaac_covenant_su_t25, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(isaac_covenant_su_t30, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% The label 'Abrahamic covenant' conflates structurally distinct claims: exclusive Isaac-lineage transmission (this story), inclusive Ishmaelite continuity (ishmael_covenant_reading), and territorial land promise (land_promise_constraint). Each carries distinct epsilon values, beneficiary/victim structures, and stakeholder configurations. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
