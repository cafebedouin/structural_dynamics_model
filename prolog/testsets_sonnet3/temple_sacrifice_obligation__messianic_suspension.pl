% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Suspended Sacrificial Obligation Pending Messianic Restoration
 *   domain: religious/legal-theological
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple, the commandments
 *   concerning animal sacrifice (korbanot) could no longer be performed,
 *   creating a legal-theological problem: Torah law is held to be eternal and
 *   complete, yet a substantial body of it became physically impossible to
 *   enact. This reading holds that the obligation is neither fulfilled nor
 *   violated but suspended, pending a future messianic restoration of the
 *   Temple that will reactivate it. Under this reading, study of sacrificial
 *   law is an act of maintaining knowledge-in-waiting — distinct from the
 *   sibling readings which treat study as either fulfilling the obligation
 *   through occupation (study_as_occupation) or as archival preservation
 *   without any relationship to obligation-fulfillment (study_as_archiving).
 *   This story authors ONLY the suspension reading; the sibling readings are
 *   separate constraints linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - halakhic_authorities: institutional/analytical — administer and defend the suspension category
 *   - observant_jewish_community: organized/identity_locked — beneficiaries of psychological and doctrinal stability
 *   - diaspora_torah_scholars: moderate/constrained — sustain the category through pedagogy and interpretation
 *   - messianic_restorationist_movements: powerless/trapped — excluded alternative framing favoring active preparation
 *   - comparative_religion_scholars: analytical/analytical — external observers of the doctrinal mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, scaffold).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Suspended Sacrificial Obligation Pending Messianic Restoration").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/legal-theological").

narrative_ontology:has_sunset_clause(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, 'b81c4847-2d91-4253-a180-2a7abfc740ad').
narrative_ontology:cs_kernel_codification('b81c4847-2d91-4253-a180-2a7abfc740ad', fixed_text).
narrative_ontology:cs_authority_grounding('b81c4847-2d91-4253-a180-2a7abfc740ad', lineage).
narrative_ontology:cs_interpretation_layer_present('b81c4847-2d91-4253-a180-2a7abfc740ad').
narrative_ontology:cs_reading_relation('b81c4847-2d91-4253-a180-2a7abfc740ad', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('b81c4847-2d91-4253-a180-2a7abfc740ad', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_axiom('b81c4847-2d91-4253-a180-2a7abfc740ad', foundational, obligation_persists_in_dormant_legal_state).
narrative_ontology:cs_axiom_status(obligation_persists_in_dormant_legal_state, holdable).
narrative_ontology:cs_axiom_grounding('b81c4847-2d91-4253-a180-2a7abfc740ad', obligation_persists_in_dormant_legal_state, theological).
narrative_ontology:cs_axiom('b81c4847-2d91-4253-a180-2a7abfc740ad', secondary, study_is_neither_fulfillment_nor_mere_archive).
narrative_ontology:cs_axiom_status(study_is_neither_fulfillment_nor_mere_archive, holdable).
narrative_ontology:cs_axiom_grounding('b81c4847-2d91-4253-a180-2a7abfc740ad', study_is_neither_fulfillment_nor_mere_archive, conventional).
narrative_ontology:cs_reference_frame('b81c4847-2d91-4253-a180-2a7abfc740ad', second_temple_sacrificial_praxis).
narrative_ontology:cs_drift_state('b81c4847-2d91-4253-a180-2a7abfc740ad', contemporary_diaspora_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b81c4847-2d91-4253-a180-2a7abfc740ad', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, observant_jewish_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, halakhic_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, diaspora_torah_scholars).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, divine_covenant_continuity).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, temple_restoration_eschatology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rule on the status of sacrificial law in the Temple's absence, holding that the obligation is suspended (anuss - circumstance beyond control) rather than annulled. They administer the category itself: deciding what counts as suspension versus fulfillment versus violation, and how study relates to the dormant obligation. They bear no cost from the ruling and their communal standing depends partly on their competence to hold this exact line.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_authorities, agenda_setter,
    institutional, civilizational, analytical, global).

% Live under a legal system where an entire category of commandments (korbanot) sits in abeyance rather than either binding them to impossible action or being struck from the law. This lets them retain full covenantal identity and textual inheritance without daily violation anxiety over commandments they structurally cannot perform. Exit from the framework would mean exit from the broader halakhic identity, which is not experienced as a live option by most who hold it.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, observant_jewish_community, beneficiary,
    organized, civilizational, identity_locked, global).

% Study the laws of sacrifice as part of ongoing Torah study, occupying professional and communal roles built around transmitting this suspended-but-preserved body of law. Their study is neither obligated performance nor claimed as fulfillment under this reading; it is framed as maintenance of knowledge that would be needed if restoration occurred. Their livelihoods and communal status are partly constituted by teaching material that has no operative referent.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, diaspora_torah_scholars, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__messianic_suspension, diaspora_torah_scholars, agenda_setter).

% Groups actively preparing physical or ritual infrastructure for Temple restoration (e.g. red heifer breeding programs, priestly lineage verification) sit awkwardly outside mainstream halakhic consensus, which treats restoration as divinely initiated and not to be hastened by human action. Their preparatory activity is not endorsed by this reading's authority structure and they have limited standing to challenge the suspension framing from within it.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_restorationist_movements, excluded,
    powerless, civilizational, trapped, regional).

% Study the suspension doctrine as a case of a legal system managing an unperformable obligation without either enforcing impossible compliance or declaring the law void. They compare it to other traditions' handling of superseded or impossible commandments, without a stake in which halakhic reading prevails.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal category for an entire body of commandments that cannot currently be performed, avoiding both the incoherence of binding people to impossible acts and the destabilization of declaring core Torah law permanently void.
% TRANSFER_FUNCTION: Moves almost nothing materially; what it transfers is psychological and identity-constituting — relief from violation-anxiety to the community, and continued interpretive authority and pedagogical standing to the scholars and authorities who administer the category.
% ABSENT_VOICES: Messianic-restorationist and Temple-preparation movements who would prefer active preparatory obligation over passive suspension are structurally outside the consensus that authors this reading; their alternative framing is not represented in the ruling.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine vanished, halakhic authorities disagree on what would replace it: some argue the community would simply continue as before since no one currently performs sacrifice regardless of doctrine, while others argue the entire logic of Torah's eternal validity depends on this exact category surviving intact, and its removal would force either an admission of partial abrogation or a crisis over unperformed core law.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), an entire legal corpus (korbanot) became physically unperformable while the tradition's premise held that Torah law is eternal and complete; a mechanism was needed to hold the law's status open without either forcing impossible compliance or admitting the law had lapsed.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion outside the halakhic tradition (studying Second Temple Judaism's transition to rabbinic Judaism) corroborate that the destruction created a genuine adjudicative crisis the suspension category was built to resolve, independent of whether they accept the theological premises involved; this is not solely attested by the beneficiary community.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.05, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.05) because under this specific reading nothing material is extracted from anyone: no one is compelled to perform an impossible act, no one is penalized for non-performance, and no resource transfer occurs. Suppression is low-moderate (0.12) reflecting the mild social pressure to accept the suspension framing rather than alternative readings, but this is interpretive pressure, not coercive enforcement. Theater ratio is low but slowly rising (0.05 to 0.15) reflecting a modest increase over centuries in ceremonial and pedagogical gestures toward the dormant obligation (liturgical references, memorial fasts) without any change in actual performance status. Accessibility collapse is moderate (0.4): the suspension framing does not foreclose alternative theological readings (the sibling readings coexist), so alternatives have not fully collapsed. Resistance is low (0.1) because the reading is broadly accepted within mainstream rabbinic authority structures; resistance is concentrated in a small excluded minority (restorationist movements).
 *
 * DIRECTIONALITY LOGIC:
 *   The observant community and halakhic authorities are coded as beneficiaries because the suspension doctrine removes them from violation-jeopardy for an entire body of unperformable law while preserving the doctrine of Torah's completeness — this is a pure stabilizing function with no identified victim group under this reading. There is no victim set: this is the structural delta that distinguishes this reading most sharply from any extractive reading of the kernel. Diaspora scholars occupy a dual position (beneficiary of professional standing, agenda_setter through interpretive authority) but bear no cost either.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an eternal-law doctrine colliding with physical impossibility of performance) remains live as long as the Temple remains undestroyed-but-unrebuilt and mainstream authorities hold to Torah's completeness doctrine; this is why founding_problem_status is 'live' rather than 'dead' — unlike a mandatrophy case where the original problem has resolved but the structure persists for capture, here the structural fact motivating the doctrine (no Temple exists) is unchanged since its founding. This is precisely what prevents mislabeling the suspension doctrine as either pure extraction (there is no active transfer to label as extraction) or as an obsolete husk (the underlying condition it addresses has not gone away).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_abrogation_ambiguity,
    'Is ''suspension pending restoration'' a stable legal category that genuinely differs from de facto abrogation, or is it a theological label preserving the doctrine of Torah''s eternal completeness over a law that has, in practical effect, lapsed for nearly two millennia?',
    'Comparative analysis of how the tradition treats other genuinely time-bound suspended obligations (e.g., laws contingent on the Sanhedrin''s existence) against how it treats korbanot; convergence in treatment would support genuine suspension, divergence (i.e. korbanot receiving unique doctrinal effort to avoid the abrogation label) would support that suspension functions primarily as a legitimacy-preserving fiction.',
    'If suspension functions as a preserving fiction, the doctrine''s near-zero extractiveness reading holds structurally but its claimed_type shifts toward piton (a doctrine maintained by inertia and interpretive labor rather than active coordination) rather than scaffold with a genuine future sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_abrogation_ambiguity, conceptual, 'Whether suspension is a substantively different legal state from abrogation or a label preserving doctrinal completeness.').

omega_variable(
    committer_reading_selection_ambiguity,
    'Among the three sibling readings of the temple_sacrifice_obligation kernel (messianic_suspension, study_as_occupation, study_as_archiving), which reading a given community or authority holds is itself underdetermined by shared texts — different halakhic authorities across history and geography have leaned toward different readings without a single adjudicating mechanism resolving the dispute.',
    'Survey of responsa literature across major halakhic authorities (Rambam, Shulchan Aruch commentators, contemporary poskim) to determine whether the suspension framing is dominant, contested, or merely one thread among genuinely co-held positions.',
    'If messianic_suspension is a minority reading rather than dominant consensus, the beneficiary set (observant_jewish_community) authored here is overstated — many communities may function under study_as_occupation instead, which would carry a different extractiveness and directionality profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_selection_ambiguity, conceptual, 'Uncertainty over how widely the suspension reading, versus its siblings, is actually held across halakhic authority structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(temp_tr_t0, projected).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 300, 0.08).
narrative_ontology:measurement_basis(temp_tr_t300, projected).
narrative_ontology:measurement(temp_tr_t700, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 700, 0.1).
narrative_ontology:measurement(temp_tr_t1100, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.14).
narrative_ontology:measurement(temp_tr_t1955, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1955, 0.15).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(temp_be_t0, projected).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 300, 0.04).
narrative_ontology:measurement_basis(temp_be_t300, projected).
narrative_ontology:measurement(temp_be_t700, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 700, 0.04).
narrative_ontology:measurement(temp_be_t1100, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1100, 0.05).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(temp_be_t1955, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1955, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__messianic_suspension, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__messianic_suspension, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the temple_sacrifice_obligation kernel. messianic_suspension (this story) holds the obligation in a distinct third legal state — neither fulfilled nor void — with study functioning as knowledge-maintenance rather than fulfillment or archiving. study_as_occupation treats scholarly study itself as substitute-fulfillment of the obligation. study_as_archiving treats study as preservation only, explicitly bracketing the fulfillment question. All three share near-zero extractiveness (no current material obligation is enforced under any reading) but differ in coordination function, beneficiary framing, and their relationship to the messianic timeline. Each carries its own ε and stakeholder set per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
