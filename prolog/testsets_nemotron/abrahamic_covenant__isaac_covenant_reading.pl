% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Isaac-Exclusive Abrahamic Covenant Reading
 *   domain: religious/theological/institutional
 *
 * SUMMARY:
 *   This constraint story models the Isaac-exclusive reading of the Abrahamic
 *   covenant (Genesis 17:19-21): God establishes the covenant with Isaac and
 *   his descendants, explicitly excluding Ishmael from the covenantal line.
 *   The reading functions as a religious identity boundary that coordinates
 *   Jewish communal continuity (genuine coordination) while extracting
 *   compliance and allegiance from adherents and excluding rival claimants
 *   (asymmetric extraction). The constraint is actively enforced through
 *   halakhic boundary-maintenance (conversion standards, matrilineal descent
 *   rules, exclusion from ritual participation). The claimed_type is
 *   tangled_rope because the arrangement solves a real coordination problem
 *   (transgenerational identity transmission) while simultaneously extracting
 *   from the excluded and from adherents through compliance demands. Over the
 *   2500-year interval, extractiveness rises as the boundary hardens from
 *   tribal affiliation into a legal-theological category with state-enforced
 *   consequences in some jurisdictions; theater_ratio rises as ritual
 *   performance increasingly substitutes for the covenant's original
 *   kinship-logic; suppression_requirement rises as the excluded traditions
 *   (Islam, Christianity, secular universalism) develop counter-claims
 *   requiring active boundary defense.
 *
 * KEY AGENTS:
 *   - rabbinic_judaism_institutions: Primary agenda_setter (institutional/biographical) — administers the boundary, defines conversion, controls ritual access
 *   - jewish_continuity_project: Beneficiary (organized/generational) — receives the identity-transmission infrastructure the covenant reading provides
 *   - halakhic_authority_structures: Beneficiary (institutional/generational) — derives interpretive authority from being the covenant's authorized readers
 *   - ishmaelite_lineage_claimants: Victim (organized/generational) — excluded from the covenantal promise their tradition traces to Abraham through Ishmael
 *   - islamic_tradition_bearers: Victim (institutional/generational) — their prophetic succession narrative (Abraham→Ishmael→Muhammad) is structurally foreclosed by this reading
 *   - interfaith_universalist_claimants: Victim (organized/biographical) — their claim that the covenant is universal or inclusive is excluded by the reading's particularist logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.72).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.68).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Isaac-Exclusive Abrahamic Covenant Reading").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/theological/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, 'a8fcc2df-d447-43f5-a834-2f2b59a9cacd').
narrative_ontology:cs_kernel_codification('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', fixed_text).
narrative_ontology:cs_authority_grounding('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', lineage).
narrative_ontology:cs_interpretation_layer_present('a8fcc2df-d447-43f5-a834-2f2b59a9cacd').
narrative_ontology:cs_reading_relation('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', foundational, covenant_exclusively_through_isaac).
narrative_ontology:cs_axiom_status(covenant_exclusively_through_isaac, holdable).
narrative_ontology:cs_axiom_grounding('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', covenant_exclusively_through_isaac, deontological).
narrative_ontology:cs_axiom('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', foundational, ishmael_explicitly_excluded_from_covenant).
narrative_ontology:cs_axiom_status(ishmael_explicitly_excluded_from_covenant, holdable).
narrative_ontology:cs_axiom_grounding('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', ishmael_explicitly_excluded_from_covenant, deontological).
narrative_ontology:cs_reference_frame('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', sinaitic_covenantal_particularism).
narrative_ontology:cs_drift_state('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', contemporary_state_sovereignty_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a8fcc2df-d447-43f5-a834-2f2b59a9cacd', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_judaism_institutions).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_continuity_project).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, halakhic_authority_structures).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_tradition_bearers).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, interfaith_universalist_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, secular_jewish_identifiers).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, observant_jewish_adherents).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, secular_jewish_identifiers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the covenantal boundary through conversion courts, matrilineal descent rulings, and ritual access controls. Define who counts as Jewish for religious purposes. Their authority derives from being the authorized interpreters of the covenant text. Exit from this role means abandoning the interpretive tradition that constitutes their institutional identity.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_judaism_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives the identity-transmission infrastructure the covenant reading provides: a ready-made boundary that distinguishes 'in' from 'out' across diaspora, enabling communal cohesion without territorial sovereignty. Benefits from the boundary's clarity but does not administer it. Exit means building alternative identity infrastructure from scratch.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_continuity_project, beneficiary,
    organized, generational, constrained, global).

% Derives interpretive authority from being the covenant's authorized readers. The covenant's textual authority grounds their legal rulings on personal status, marriage, divorce, and conversion. Collects authority-rents from the boundary's enforcement. Exit means surrendering the textual ground of their authority.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, halakhic_authority_structures, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, halakhic_authority_structures, agenda_setter).

% Trace their covenantal standing to Abraham through Ishmael. The Isaac-exclusive reading structurally forecloses this claim — their ancestral line is explicitly excluded from the promise. They bear the cost of foreclosure: their tradition's founding narrative is ruled invalid by the dominant reading. Exit requires abandoning their tradition's core genealogical claim.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants, payer,
    organized, generational, trapped, global).

% Their prophetic succession (Abraham → Ishmael → Muhammad) validates the broader Abrahamic lineage. The Isaac-exclusive reading forecloses this by making Ishmael a non-covenantal figure. The cost is structural: Islamic theology must either reject the Genesis text or reinterpret it against its plain sense. Exit from the foreclosure is impossible without surrendering the Quranic narrative.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_tradition_bearers, payer,
    institutional, generational, trapped, global).

% Claim the covenant is universal or at least inclusive of all Abrahamic descendants. Their voice is excluded by the reading's particularist logic — the boundary exists precisely to deny universalist claims. They can advocate from outside but cannot change the boundary from within. Exit means accepting the boundary as a given fact of the religious landscape.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, interfaith_universalist_claimants, excluded,
    organized, biographical, constrained, global).

% Bear the compliance costs of the covenant: halakhic observance, endogamy norms, communal obligation, educational burden. The covenant reading constitutes their religious identity — exit is not merely leaving a community but dissolving the self-concept the covenant provides. They pay the extraction (compliance) while receiving the coordination (identity, community, meaning).
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, observant_jewish_adherents, payer,
    moderate, biographical, identity_locked, global).

% Identify culturally/ethnically as Jewish without halakhic observance. Receive the identity-coordination benefit (communal belonging, cultural transmission) without paying the full compliance cost. But they are subject to the boundary's exclusionary effects (denied ritual access, marriage recognition in Israel). Exit is easier than for observant adherents but still carries identity-cost.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, secular_jewish_identifiers, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, secular_jewish_identifiers, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining a distinct peoplehood across diaspora, exile, and statelessness by providing a transgenerational identity boundary that requires no territory, no shared language, and no centralized polity — only the covenantal claim and its interpretive tradition.
% TRANSFER_FUNCTION: Moves interpretive authority and communal allegiance from adherents to halakhic institutions; moves the cost of boundary-maintenance (exclusion, foreclosure) onto rival traditions and internal dissenters; moves the psychological burden of chosenness/otherness onto the excluded.
% ABSENT_VOICES: Pre-rabbinic Jewish sects that read the covenant inclusively (e.g., certain Second Temple groups); early Muslim communities that saw themselves as the true heirs of the Abrahamic promise; modern post-ethnic Jews who reject the boundary entirely. These voices were historically suppressed or assimilated; their absence is structural, not accidental.
% DISAPPEARANCE_RATIONALE: If the Isaac-exclusive reading vanished overnight, Jewish identity would lose its primary theological boundary. Conversion standards would collapse, matrilineal descent would become optional, the halakhic definition of Jewishness would dissolve. The Jewish people would either fragment into multiple unconnected communities or reorganize around a new boundary (national, cultural, or universalist). The Islamic tradition would lose its primary theological foil for the Ishmael claim. The Abrahamic ecumenical project would lose its hardest boundary case.
% FOUNDING_PROBLEM: How to maintain a coherent collective identity across forced exile, without territory, temple, or sovereign institutions — a people that persists as a people through textual covenant alone.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record: the Babylonian exile (586 BCE) created the condition of stateless dispersion that the covenant reading solves. This is corroborated by non-Jewish historians (Josephus, Tacitus) and by the internal rabbinic record (Talmud, midrash) which explicitly frames the covenant as the survival mechanism. No serious scholar disputes that the covenant reading functioned as identity-infrastructure for a stateless people. The dispute is whether that problem still requires THIS boundary.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the reading demands costly compliance (halakhic observance, endogamy, communal allegiance) while the excluded traditions bear the cost of structural foreclosure. Suppression (0.68) is substantial because the boundary is maintained through active exclusion (conversion barriers, ritual exclusion, legal definitions of Jewishness) rather than passive neglect. Theater_ratio (0.25) is moderate: the covenant's original kinship-coordination function has partially atrophied into ritual performance, but the coordination function (identity transmission across diaspora) remains real. Accessibility_collapse (0.65) is elevated because the identity boundary, once internalized, makes exit psychologically and socially costly — but not total, as secularization and intermarriage demonstrate. Resistance (0.55) is moderate: the excluded traditions mount theological and political counter-claims, and internal reform movements challenge the boundary's stringency.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (rabbinic institutions), the constraint is genuine coordination: it solves the problem of maintaining a distinct people across exile without territory or sovereignty. From the beneficiary seats (continuity project, halakhic authorities), it is a subsidized infrastructure — they receive the benefits of a pre-built identity system. From the victim seats (Islamic tradition, Ishmaelite claimants, universalists), it is extraction via foreclosure: their Abrahamic claims are structurally ruled out. The engine computes this divergence from the structural data — the declared beneficiaries and victims, the exit_options (identity_locked for adherents, trapped for excluded claimants), and the power distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda_setters (rabbinic institutions) are structural beneficiaries: they control the authoritative interpretation and collect the authority rents (d ~0.15). Beneficiaries (continuity project, halakhic authorities) receive the coordination infrastructure without bearing its enforcement costs (d ~0.25). Adherent-payers (observant Jews) bear compliance costs (observance, endogamy, communal obligation) with constrained exit — identity_locked (d ~0.75). Victims (Islamic tradition, Ishmaelite claimants) are trapped: their counter-claims are structurally excluded, and exit from the foreclosure requires abandoning their own tradition's core narrative (d ~0.9). Interfaith universalists are constrained: they can advocate but cannot change the boundary from outside (d ~0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transgenerational identity transmission without territory/sovereignty) remains live — the coordination function is not dead. But the extraction component has accumulated: the boundary now extracts compliance from adherents and forecloses rival traditions that did not exist at founding. The constraint is not a piton (the coordination function is still load-bearing) but a tangled_rope where the extraction layer has thickened over time. The mandatrophy_resolved flag is false because the founding problem persists, even as the arrangement has grown extractive beyond its original scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'Is this constraint a reading of the contested abrahamic_covenant kernel, and if so, which reading does it instantiate?',
    'Committer frame declaration: this story instantiates the isaac_covenant_reading of kernel abrahamic_covenant. Sibling readings are ishmael_covenant_reading and christian_supersessionist_reading.',
    'Structures the constraint family via network.affects_constraints and cs_structure.reading_relations. If the kernel framing is rejected, the constraint stands alone as a flat institutional claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system framing: this constraint is the Isaac-exclusive reading of the Abrahamic covenant kernel.').

omega_variable(
    exclusion_mechanism_ambiguity,
    'Is the exclusion of Ishmaelite/Islamic claimants structural (textual interpretation, legal definition) or internalized (identity-fusion making exit unthinkable)?',
    'Comparative study of boundary-maintenance practices: if exclusion persists after formal barriers are removed (e.g., in interfaith dialogue), reclassify as partially internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests — the excluded carry the boundary with them. Affects omega-state computation for identity_locked exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious identity boundaries.').

omega_variable(
    covenantal_unconditionality_vs_conditionality,
    'Does the Isaac-exclusive reading treat the covenant as unconditional (election by grace alone) or conditional (dependent on halakhic observance)?',
    'Historical theology: compare pre-exilic, rabbinic, medieval, and modern Zionist readings. The conditional reading extracts compliance; the unconditional reading extracts identity allegiance.',
    'Conditional reading raises extractiveness (compliance extraction) and suppression (heresy policing). Unconditional reading raises accessibility_collapse (identity is inescapable). Both are tangled_rope but with different extraction profiles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenantal_unconditionality_vs_conditionality, conceptual, 'Whether the covenant''s terms are understood as conditional on observance or absolute election.').

omega_variable(
    land_promise_entanglement,
    'Is the territorial grant (Land of Canaan) a separable component of this reading, or is it structurally fused with the Isaac-lineage claim?',
    'Analyze whether land_promise_constraint operates as a distinct constraint with its own ε, or whether the two claims are co-extensive in this reading''s enforcement.',
    'If fused, the land_promise_constraint is not a separate constraint — it is a projection of this one. If separable, they are a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_promise_entanglement, conceptual, 'Whether the territorial promise is a distinct constraint or fused with the lineage claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 500, 0.15).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t1000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t1500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1500, 0.22).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t2000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t2500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2500, 0.25).

% Extraction over time
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t1000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1000, 0.62).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t1500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t2000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t2500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2500, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 500, 0.48).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t1000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1000, 0.58).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t1500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1500, 0.62).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t2000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t2500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2500, 0.68).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=2500
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_01, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(class), 0, 0.45).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_01, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_02, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(class), 2500, 0.75).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_02, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_03, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(individual), 0, 0.3).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_03, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_04, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(individual), 2500, 0.65).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_04, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_05, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(organizational), 0, 0.4).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_05, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_06, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(organizational), 2500, 0.7).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_06, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_07, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(structural), 0, 0.5).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_07, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_08, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(structural), 2500, 0.8).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_08, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_09, abrahamic_covenant__isaac_covenant_reading, resistance(class), 0, 0.5).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_09, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_10, abrahamic_covenant__isaac_covenant_reading, resistance(class), 2500, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_10, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_11, abrahamic_covenant__isaac_covenant_reading, resistance(individual), 0, 0.6).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_11, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_12, abrahamic_covenant__isaac_covenant_reading, resistance(individual), 2500, 0.45).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_12, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_13, abrahamic_covenant__isaac_covenant_reading, resistance(organizational), 0, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_13, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_14, abrahamic_covenant__isaac_covenant_reading, resistance(organizational), 2500, 0.5).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_14, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_15, abrahamic_covenant__isaac_covenant_reading, resistance(structural), 0, 0.45).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_15, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_16, abrahamic_covenant__isaac_covenant_reading, resistance(structural), 2500, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_16, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_17, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(class), 0, 0.35).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_17, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_18, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(class), 2500, 0.7).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_18, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_19, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(individual), 0, 0.25).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_19, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_20, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(individual), 2500, 0.6).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_20, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_21, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(organizational), 0, 0.3).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_21, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_22, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(organizational), 2500, 0.65).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_22, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_23, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(structural), 0, 0.4).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_23, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_24, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(structural), 2500, 0.72).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_24, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_25, abrahamic_covenant__isaac_covenant_reading, suppression(class), 0, 0.4).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_25, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_26, abrahamic_covenant__isaac_covenant_reading, suppression(class), 2500, 0.75).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_26, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_27, abrahamic_covenant__isaac_covenant_reading, suppression(individual), 0, 0.2).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_27, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_28, abrahamic_covenant__isaac_covenant_reading, suppression(individual), 2500, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_28, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_29, abrahamic_covenant__isaac_covenant_reading, suppression(organizational), 0, 0.35).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_29, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_30, abrahamic_covenant__isaac_covenant_reading, suppression(organizational), 2500, 0.7).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_30, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_31, abrahamic_covenant__isaac_covenant_reading, suppression(structural), 0, 0.45).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_31, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_32, abrahamic_covenant__isaac_covenant_reading, suppression(structural), 2500, 0.78).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__isaac_covenant_reading, 0.08).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% This constraint and ishmael_covenant_reading form a foreclosure pair within the abrahamic_covenant kernel: each reading's core premise (Isaac-exclusive vs. Ishmael-inclusive) logically rules out the other within a single interpretive framework. christian_supersessionist_reading coexists_with both — it re-frames the lineage dispute as obsolete rather than resolving it. land_promise_constraint is structurally influenced by this reading: the Isaac-lineage claim is the theological basis for territorial claims in religious Zionism, but the land constraint has its own ε and enforcement machinery (state sovereignty, military control) that partially separates it from the lineage boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__isaac_covenant_reading, institutional, 0.15).
constraint_indexing:directionality_override(abrahamic_covenant__isaac_covenant_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
