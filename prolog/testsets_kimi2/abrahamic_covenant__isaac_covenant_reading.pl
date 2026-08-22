% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Isaac-Exclusive Covenant Reading (Genesis 17:19-21)
 *   domain: religious/theological/institutional
 *
 * SUMMARY:
 *   The Genesis 17:19-21 reading limits the Abrahamic covenant to Isaac's
 *   line, explicitly excluding Ishmael ('through Isaac shall your offspring
 *   be named'). This constraint functions as a theological identity boundary:
 *   rabbinic institutions derive legitimacy from administering the exclusive
 *   lineage claim, while the Jewish covenanted community receives the goods
 *   of continuity and distinct identity. The cost is borne by Ishmaelite
 *   claimants and later Islamic tradition, whose Abrahamic covenant claims
 *   are delegitimized within the rabbinic framework. The constraint is
 *   actively enforced through halakhic interpretation, liturgical memory, and
 *   communal boundary maintenance. It is claimed as coordination (preserving
 *   monotheistic identity across exile and diaspora) but operates with high
 *   asymmetric extraction toward the excluded lineage.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_authority: Primary agenda-setter and beneficiary (institutional/identity_locked) â administers the covenant boundary and captures institutional legitimacy
 *   - jewish_covenanted_community: Primary beneficiary (organized/identity_locked) â receives covenant identity and theological distinctiveness
 *   - ishmaelite_claimants: Primary payer (moderate/constrained) â bears genealogical exclusion and loss of covenant standing
 *   - later_islamic_tradition: Secondary payer (institutional/constrained) â bears theological delegitimation within the rabbinic framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.82).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.75).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Isaac-Exclusive Covenant Reading (Genesis 17:19-21)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/theological/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '2e160e55-65d4-4340-a3ac-d0e804421064').
narrative_ontology:cs_kernel_codification('2e160e55-65d4-4340-a3ac-d0e804421064', fixed_text).
narrative_ontology:cs_authority_grounding('2e160e55-65d4-4340-a3ac-d0e804421064', lineage).
narrative_ontology:cs_interpretation_layer_present('2e160e55-65d4-4340-a3ac-d0e804421064').
narrative_ontology:cs_reading_relation('2e160e55-65d4-4340-a3ac-d0e804421064', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('2e160e55-65d4-4340-a3ac-d0e804421064', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('2e160e55-65d4-4340-a3ac-d0e804421064', foundational, isaac_exclusive_covenant_heir).
narrative_ontology:cs_axiom_status(isaac_exclusive_covenant_heir, holdable).
narrative_ontology:cs_axiom_grounding('2e160e55-65d4-4340-a3ac-d0e804421064', isaac_exclusive_covenant_heir, theological).
narrative_ontology:cs_axiom('2e160e55-65d4-4340-a3ac-d0e804421064', foundational, eternal_unbroken_lineage_promise).
narrative_ontology:cs_axiom_status(eternal_unbroken_lineage_promise, holdable).
narrative_ontology:cs_axiom_grounding('2e160e55-65d4-4340-a3ac-d0e804421064', eternal_unbroken_lineage_promise, theological).
narrative_ontology:cs_reference_frame('2e160e55-65d4-4340-a3ac-d0e804421064', isaac_exclusive_covenant).
narrative_ontology:cs_drift_state('2e160e55-65d4-4340-a3ac-d0e804421064', contemporary_interfaith_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2e160e55-65d4-4340-a3ac-d0e804421064', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_covenanted_community).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, later_islamic_tradition).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, abrahamic_exclusivity_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, isaac_lineage_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the covenant boundary through halakhic interpretation, liturgical practice, and communal gatekeeping; derives institutional legitimacy from the exclusive lineage claim and transmits it across generations via rabbinic succession.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_authority, beneficiary).

% Organizes kinship, marriage, liturgy, and collective memory around the exclusive Isaac covenant; receives theological distinctiveness and continuity while the boundary excludes rival Abrahamic claimants.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_covenanted_community, beneficiary,
    organized, generational, identity_locked, global).

% Descendants of Ishmael whose genealogical claim to Abrahamic blessing is delegitimized by the Genesis 17 exclusion; they assert covenant standing but are barred from recognition within the rabbinic interpretive framework.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants, payer,
    moderate, generational, constrained, regional).

% A world religious tradition that grounds prophetic legitimacy in Ishmaelite lineage through Muhammad; the Isaac-exclusive reading structurally relegates its covenant claim to secondary status and withholds the Abrahamic promise as interpreted by the rabbinic kernel.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, later_islamic_tradition, payer,
    institutional, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_authority).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves monotheistic covenant fidelity and communal identity across generations by anchoring legitimacy to an exclusive, uncontested lineage that prevents dissipation into rival claims or syncretism.
% TRANSFER_FUNCTION: Moves covenant status, legitimation, and theological standing from Ishmaelite genealogical claimants to Isaac's descendants and the rabbinic institutions that interpret and transmit that lineage.
% ABSENT_VOICES: Ishmaelite genealogical claimants and Islamic theological voices are structurally excluded from the rabbinic canonical conversation; they would assert inclusive or parallel Abrahamic covenanthood but are outside the interpretive framework.
% DISAPPEARANCE_RATIONALE: If the Isaac-exclusive covenant reading vanished, Jewish communal identity would lose a primary boundary mechanism, rabbinic authority would require re-grounding, and Islamic and Christian Abrahamic claims would gain theological space â the inter-religious landscape would reorganize around open or dual lineage claims.
% FOUNDING_PROBLEM: Preserving covenant fidelity and distinct communal identity after Abraham's death amid multiple sons and competing lineage claims, preventing theological dissolution or assimilation into surrounding kinship structures.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources attest the problem of continuity, but external corroboration from Islamic tradition and modern critical scholarship disputes that Ishmael's exclusion was necessary to solve it, arguing the exclusivity reflects later editorial polemic rather than an original coordination requirement.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the reading structurally withholds covenant standing from a rival Abrahamic lineage, concentrating legitimation in Isaac's descendants. Suppression (0.75) reflects the active interpretive work required to maintain exclusivity against Ishmaelite and Islamic counter-readings. Theater_ratio (0.45) captures the performative dimension of lineage maintenance â ritual, liturgy, and boundary policing â that exceeds the functional coordination minimum. Accessibility_collapse is high (0.70) because once the rabbinic framework is accepted, Ishmaelite alternatives collapse textually; resistance (0.60) reflects the persistent Islamic and modern interfaith challenge. The temporal series show gradual intensification as rival Abrahamic traditions grew and required stronger boundary maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic seat, the constraint is necessary coordination without which Jewish identity dissolves into generic Abrahamism or assimilation. From the Ishmaelite and Islamic seats, the same structure is arbitrary exclusion that denies an equally valid genealogical reading. The engine computes this divergence from the structural asymmetry in exit options (identity_locked beneficiaries versus constrained payers) and the beneficiary-victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and the Jewish covenanted community sit near the beneficiary pole: they receive legitimacy, continuity, and boundary definition from the constraint, and their exit is identity_locked because leaving would dissolve communal selfhood. Ishmaelite claimants and Islamic tradition sit near the target pole: they bear the cost of excluded status and have no recourse within the rabbinic framework to reclaim covenant standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because the coordination function is genuine and historically necessary: Jewish survival across diaspora required a coherent lineage mechanism. However, the coordination is inseparable from the extraction â the same boundary that preserves identity excludes rivals. If the coordination function were labeled as pure rope, the Ishmaelite victim set would be invisible; if labeled as pure snare, the genuine communal coordination would be denied. Tangled rope captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    isaac_exclusivity_constructed_or_revealed,
    'Is the Isaac-exclusive covenant a revealed theological limit or a constructed interpretive boundary serving institutional Jewish continuity?',
    'Historical-critical redaction analysis of Genesis 17, comparative Ancient Near Eastern covenant formulary, and interfaith hermeneutical arbitration.',
    'If constructed, the constraint reclassifies toward snare and the victim set gains moral standing; if revealed, the extraction is reframed as divine prerogative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isaac_exclusivity_constructed_or_revealed, conceptual, 'Whether Isaac exclusivity is revealed or constructed.').

omega_variable(
    ishmael_blessing_scope,
    'Does Genesis 17:19-21 logically negate Ishmael''s blessing in Genesis 21:13-18, or merely prioritize Isaac''s covenant without abolishing Ishmael''s standing?',
    'Exegetical synthesis of the Genesis cycle and rabbinic versus Islamic tafsir traditions.',
    'If Ishmael retains a non-exclusive blessing, the constraint''s extraction severity decreases and the boundary becomes porous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ishmael_blessing_scope, conceptual, 'Scope of Ishmael''s blessing relative to Isaac''s covenant.').

omega_variable(
    active_enforcement_mechanism,
    'Is the constraint''s persistence due to active rabbinic interpretive enforcement, or to passive communal identity inertia?',
    'Comparative analysis of communities where rabbinic authority weakened (Reform Judaism, secular Zionism) to see if the Isaac exclusivity attenuates or persists.',
    'If passive inertia dominates, the constraint drifts toward piton; if active enforcement remains primary, it stays tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_enforcement_mechanism, empirical, 'Enforcement mechanism ambiguity for covenant boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(abra_tr_t500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 500, 0.28).
narrative_ontology:measurement(abra_tr_t1000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(abra_tr_t1500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(abra_tr_t1900, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1900, 0.43).
narrative_ontology:measurement(abra_tr_t2024, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(abra_be_t500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 500, 0.65).
narrative_ontology:measurement(abra_be_t1000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1000, 0.72).
narrative_ontology:measurement(abra_be_t1500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1500, 0.76).
narrative_ontology:measurement(abra_be_t1900, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1900, 0.79).
narrative_ontology:measurement(abra_be_t2024, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(abra_su_t500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 500, 0.55).
narrative_ontology:measurement(abra_su_t1000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(abra_su_t1500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1500, 0.7).
narrative_ontology:measurement(abra_su_t1900, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1900, 0.73).
narrative_ontology:measurement(abra_su_t2024, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the abrahamic_covenant kernel. The Isaac-exclusive reading and its siblings (Ishmael-inclusive, Christian supersessionist, land-promise territorial) are structurally distinct claims with different epsilon values and victim/beneficiary structures, despite sharing the Genesis textual kernel. They form a constraint family linked by textual kinship but divergent in classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
