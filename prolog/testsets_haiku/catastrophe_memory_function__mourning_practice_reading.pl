% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Commemorative Mourning Ritual as Boundary-Maintenance Mechanism
 *   domain: religious/social/collective_memory
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'catastrophe_memory_function.' The mourning-practice reading frames
 *   commemorative ritual (exemplified by Tisha B'Av — the annual day of
 *   mourning for the destruction of the Temple) as serving primarily the
 *   D1/D4 functions: maintaining group boundary and identity through
 *   synchronized ritual obligation. This reading claims the ritual's core
 *   binding function is collective grief-expression and memory-preservation,
 *   not transmission of survival-competence or adaptive institutional
 *   knowledge (the functions the sibling readings emphasize). The
 *   mourning-practice reading sees the ritual as an END in itself — the
 *   community exists, in part, to enact this mourning together. Enforcement
 *   of the ritual's form is justified as fidelity to transmission; innovation
 *   is read as dilution. The reading is held by ritual custodians and
 *   observant traditionalists. Dissenting and innovation-seeking members
 *   contest it from within the community, while non-observant descendants
 *   experience it as external imposition.
 *
 * KEY AGENTS:
 *   - ritual_custodians: Authority structure defining and enforcing the canonical mourning form — identity-locked to transmission authority
 *   - observant_community: Participants who accept the framework and benefit from synchronized collective grief — constrained exit due to relational embeddedness
 *   - dissenting_members: Experience the ritual as constraint on authentic expression — identity-locked because exit threatens group membership
 *   - innovation_seekers: Wish to adapt the ritual to contemporary forms — excluded from shaping its form, enforcement as corruption of transmission
 *   - non_observant_descendants: Experience the constraint indirectly through family/social pressure — mobile exit but high relational cost
 *   - historical_specialists: Analytical observers who study how the ritual preserves memory across generations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Commemorative Mourning Ritual as Boundary-Maintenance Mechanism").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious/social/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '040babb3-43a2-4d71-b3c2-1a861d627dcc').
narrative_ontology:cs_kernel_codification('040babb3-43a2-4d71-b3c2-1a861d627dcc', fixed_text).
narrative_ontology:cs_authority_grounding('040babb3-43a2-4d71-b3c2-1a861d627dcc', lineage).
narrative_ontology:cs_interpretation_layer_present('040babb3-43a2-4d71-b3c2-1a861d627dcc').
narrative_ontology:cs_reading_relation('040babb3-43a2-4d71-b3c2-1a861d627dcc', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('040babb3-43a2-4d71-b3c2-1a861d627dcc', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('040babb3-43a2-4d71-b3c2-1a861d627dcc', foundational, ritual_mourning_primary_binding_function).
narrative_ontology:cs_axiom_status(ritual_mourning_primary_binding_function, holdable).
narrative_ontology:cs_axiom_grounding('040babb3-43a2-4d71-b3c2-1a861d627dcc', ritual_mourning_primary_binding_function, deontological).
narrative_ontology:cs_axiom('040babb3-43a2-4d71-b3c2-1a861d627dcc', foundational, canonical_form_fidelity_required_for_authenticity).
narrative_ontology:cs_axiom_status(canonical_form_fidelity_required_for_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('040babb3-43a2-4d71-b3c2-1a861d627dcc', canonical_form_fidelity_required_for_authenticity, conventional).
narrative_ontology:cs_reference_frame('040babb3-43a2-4d71-b3c2-1a861d627dcc', tisha_bav_canonical_remembrance).
narrative_ontology:cs_drift_state('040babb3-43a2-4d71-b3c2-1a861d627dcc', contemporary_post_witness_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('040babb3-43a2-4d71-b3c2-1a861d627dcc', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, ritual_custodians).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, group_identity_maintainers).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, dissenting_members).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, innovation_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, observant_community).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, observant_community).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, non_observant_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious authorities, community leaders, and designated ritual practitioners who maintain the prescribed form of commemorative practice. They define what counts as correct mourning, enforce fidelity to the canonical sequence (prayer, fasting, scriptural reading, lamentation), and exclude divergent expressions or innovations. Their authority derives from custodianship of a transmitted tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_custodians, agenda_setter,
    organized, generational, identity_locked, regional).

% Community members who accept the mourning framework as legitimate and participate according to its rules. They benefit from a shared, synchronized grief-practice that binds them to a collective history and to each other. They also bear the cost of performing the ritual within its prescribed boundaries, including emotional labor, time commitment, and the suppression of alternative expressions of loss.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, observant_community, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, observant_community, payer).

% Community members who experience the constraint between their own mourning impulses and the ritual's prescribed form. Some wish to grieve differently (silence vs. vocalization, individualized narrative vs. canonical text, modern media vs. traditional objects). They experience the ritual as a constraint on authentic expression, yet remain within the community because exit threatens their relational and identity membership. Enforcement is both social disapproval and internalized guilt.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, dissenting_members, payer,
    moderate, biographical, identity_locked, regional).

% Members who see the ritual as static and wish to adapt it to contemporary forms (incorporate modern poetry, create new ceremonies addressing recent losses, blend canonical text with personal testimony). They are structurally excluded from shaping the ritual's form because custodians' authority is grounded in faithfulness to transmission, not in innovation. Their attempted modifications face enforcement as corruption of the tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, innovation_seekers, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, innovation_seekers, excluded).

% Descendants of the catastrophe who have left the observant community or never entered it. They experience the constraint indirectly through social expectation, family pressure, and public commemoration events they may view as obligatory or performative. Their exit is real (they can and do leave), but carries relational cost. The constraint's enforcement machinery includes family dynamics and communal stigma.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, non_observant_descendants, payer,
    powerful, biographical, mobile, regional).

% Scholars, historians, and analysts who study how ritual preserves and transforms collective memory. They document the constraint's operation across generations, track innovation pressures within the tradition, and measure the fidelity/adaptation ratio. They take no position on whether mourning-practice is the binding function (as this reading claims) or whether other functions (survival-competence, adaptive transformation) are equally operative.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, historical_specialists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, ritual_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes grief and loss-recognition across a dispersed community: the ritual ensures that on a appointed date, community members enact shared emotional and spiritual work tied to a specific historical catastrophe. The coordination solves the problem of how a group maintains collective memory and identity across generations when individual mourning impulses diverge.
% TRANSFER_FUNCTION: Channels individual emotional experience and creative expression into a canonical, repeatable form. Members transfer their varied grief-responses into the framework the ritual provides: they perform prescribed actions (fasting, reading, lamentation) rather than pursuing their own mourning vocabularies. The constraint also transfers authority to custodians — the power to define what counts as legitimate commemoration.
% ABSENT_VOICES: Dissenting members and innovation-seekers are present but structurally subordinated — they cannot shape the ritual's form. Also absent from the formal decision-making: future generations who will inherit this constraint; non-observant descendants who experience it as imposed rather than chosen; alternative commemorative traditions that might perform the same memory function through different mechanisms.
% DISAPPEARANCE_RATIONALE: If the commemorative mourning ritual and its enforcement vanished, the community would lose the synchronized anchor for collective grief. Individual members would grieve privately and variously; the boundary-maintenance function would degrade — the shared identity constituted through shared mourning-practice would fracture. Subgroups might create alternative commemoration forms; generational transmission of loss-memory would become diffuse and less structured. The community would not disappear, but its internal cohesion through ritual observance would reorganize.
% FOUNDING_PROBLEM: After a catastrophic loss, how does a community maintain continuity of collective identity and memory across generations when the living witnesses die? The founding problem is: what mechanism preserves the specific shape of the loss — not just that it happened, but what it meant — so that descendants know themselves as participants in a shared history rather than as observers of ancient events?
% FOUNDING_PROBLEM_CORROBORATION: Historians of catastrophe memory (Assmann, Halbwachs, Yerushalmi) and survivors' testimonies attest that without structured ritual, catastrophe memory fades within 2–3 generations. Non-observant community members and secular scholars acknowledge the problem is real; they dispute whether the traditional ritual is still the solution (they advocate for updated forms, secular memorialization, or integration with modern historical practice). The founding problem is attested from outside the ritual-custodian beneficiary set.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint does coordinate genuine collective memory work (reducing it below coordination cost would be false), but it also extracts: members suppress their own mourning vocabularies and innovations in favor of canonical forms. Suppression is substantial (0.62) because enforcement is both structural (social disapproval, exclusion from authority) and internalized (ritual custodians are trusted; dissent feels like betrayal of the dead). Theater_ratio is HIGH (0.71), rising over the measurement interval from 0.55 to 0.71. This rise reflects the core dynamic this reading observes: as distance from the catastrophe grows (fewer living witnesses), the ritual's capacity to transmit lived loss-memory decays; more of the observance becomes performance and obligation ('we do this because it is how we have always done it') rather than authentic connection to the founding trauma. The theater rise is not dysfunction but the reading's signature diagnostic: in the mourning-practice framing, as theater rises, the constraint's binding power shifts from genuine grief-coordination to identity-boundary maintenance. Measurement at t=80 shows a slight decline (0.38 → 0.38 on extractiveness, 0.70 → 0.71 on theater) reflecting stabilization — the community has completed the transition from experience-based mourning to obligation-based identity practice, and the constraint settles into a steady-state theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Ritual custodians and observant members experience the constraint as coordination (we grieve together, we know ourselves as a people) and see enforcement as necessary to preserve the transmission. Dissenting members and innovation-seekers experience the same constraint as extraction (their grief-impulses and creative expressions are suppressed; they are subordinated in the authority structure). Non-observant descendants experience it as imposition (they did not choose this identity, yet are expected to honor it). The engine computes these divergences from the stakeholder power/exit structure: custodians (organized, identity-locked) and observant members compute low directionality (beneficiaries); dissenting members and innovation-seekers (moderate power, identity-locked exit, subordinated in the structure) compute high directionality (targets). This reading claims the beneficiary reading (custodians' perspective) is the functional truth — the ritual's primary binding purpose IS to maintain group boundary through mourning obligation — while acknowledging extraction exists as a side effect, not the primary function.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual custodians: Organized power, identity-locked to the role of authority-maintainer, low exit. They benefit from the constraint by holding interpretive authority over the community's most sacred practice. Directionality near beneficiary end (d ≈ 0.15). Observant community: Organized power, constrained exit (relational embeddedness), genuine coordination benefit (shared grief-practice). Directionality symmetric (d ≈ 0.50) — they benefit from the coordination, but also pay the cost of conformity. Dissenting members: Moderate power, identity-locked to the community (exit costs relational membership), their voices are suppressed in the authority structure. Directionality near target end (d ≈ 0.70). Innovation-seekers: Similar positioning to dissenters — moderate power, identity-locked, excluded from shaping the constraint's form. Directionality near target end (d ≈ 0.70). Non-observant descendants: Powerful if they completely defect (mobile exit is real), but experience ongoing family/social pressure. Directionality intermediate (d ≈ 0.45) — they can exit but choose constrained participation due to relational ties.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap by clearly specifying what the ritual was built for (collective memory of catastrophic loss across generations) and arguing that function is still live. The dissent comes from whether the traditional form is still the optimal mechanism, not from whether the founding problem exists. However, the rising theater_ratio signals a secondary mandatrophy pressure: the ritual's capacity to transmit genuine emotional resonance (D1/D4 authenticity) decays as witnesses die. The community transitions from 'we mourn together because we remember' to 'we observe this form because it is our identity.' The reading's honest answer: the founding problem (preserving catastrophe memory) remains live, and this ritual form solves it, BUT the mechanism is increasingly theatrical — the emotional content is replaced by obligation-structure. This is not mandatrophy (the founding problem persists) but rather a phase transition in how the problem is solved: the ritual shifts from experience-coordination to identity-boundary-maintenance. The measurement series documents this transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.62) primarily structural (custodians enforce; innovators are blocked) or internalized (community members accept the constraint''s legitimacy; dissenters police themselves)?',
    'Post-defection suppression trajectory: if dissenting members who leave the observant community report sustained guilt/obligation (carrying suppression with them), the mechanism is partially internalized. Oral history from innovation-seekers documenting whether they abandoned attempts due to external barriers vs. internal conviction that innovation is wrong.',
    'If internalized, the constraint''s effective suppression persists even if custodians lose institutional power — members remain constrained. If structural, removal of custodial authority would substantially reduce suppression. This affects predictions for how the constraint would evolve under competing authority structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Suppression mechanism: structural enforcement vs. internalized identity-guilt').

omega_variable(
    founding_problem_live_status_contested,
    'Is the founding problem ''preserving catastrophe memory across generations'' actually LIVE in the contemporary community, or has it been substantially solved by historians/museums/secular memorialization such that the traditional ritual persists as zombie-obligation?',
    'Generational cohort analysis: do younger non-observant descendants show decreased attachment to collective catastrophe memory compared to generations who lived through or received direct witness testimony? Does the ritual''s discontinuation in modernized/secular communities result in observable loss of historical identity-continuity, or is continuity maintained through alternative channels (education, museums, family narrative)?',
    'If the founding problem is dead (memory is now preserved through secular institutions and does not require ritual enactment), the constraint becomes a Piton — maintained through obligation-structure and family pressure, but without the coordination function that initially justified it. If the founding problem is live, the constraint remains Tangled Rope (coordination + extraction). This determines whether the community faces mandatrophy or stable function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_status_contested, empirical, 'Whether the founding memory-preservation problem persists or has migrated to alternative institutional forms').

omega_variable(
    mourning_vs_survival_reading_empirical_separability,
    'Can the mourning-practice function (D1/D4: maintaining group boundary through collective grief) be structurally separated from the survival-competence function (D5: transmitting adaptive institutional knowledge for organizational continuity)? Or are they empirically inseparable in the ritual''s actual operation?',
    'Textual and ethnographic analysis: does the ritual''s canonical content (prayers, readings, lamentations) explicitly teach survival-adaptive lessons (decentralized community-building, economic self-sufficiency, institutional resilience), or is survival-competence a sibling reading''s interpretation imposed on content whose manifest intention is mourning and identity-preservation? Do observant practitioners report learning survival-capacity from the ritual, or only learning identity-membership and loss-memory?',
    'If separable, the mourning-practice reading is correct and the hybrid-transformation reading is reading extra functions into the ritual that are not actually there. If inseparable, the constraint does serve both functions simultaneously and the survival-competence and hybrid readings are empirically more accurate. This is the central disagreement between this reading and its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mourning_vs_survival_reading_empirical_separability, conceptual, 'Whether mourning-practice (D1/D4) and survival-competence (D5) are separable functions or intrinsically entangled in the ritual').

omega_variable(
    identity_locked_exit_authenticity,
    'Dissenting members and innovation-seekers report identity-locked exit, claiming their identity is constituted through the community and leaving would dissolve it. Is this identity-fusion genuine, or is it an internalized reframing of a choice that is actually more mobile?',
    'Life-history interviews with defectors who left the observant community: did they report the defection as identity-death (I became a different person), or as liberation-through-exit (I chose a fuller version of myself)? Trajectory analysis: do defectors maintain identity-continuity with the community''s catastrophe memory (through secular commemoration, family narrative, political identity), or is memory-tie severed?',
    'If genuinely identity-locked, dissenters are trapped and suppression is high-impact. If exit is more mobile than reported (identity-fusion is a learned frame that could be unlearned), the constraint''s suppression score should be lower and exit options should be upgraded. This affects the directionality calculation for dissenters and the classification''s robustness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_authenticity, empirical, 'Whether identity-locked exit for dissenters is genuine fusion or learned frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 10, 0.59).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.63).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 60, 0.7).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 80, 0.71).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 80, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_function' kernel. The mourning-practice reading interprets the constraint's primary function as maintaining group identity and collective memory through synchronized ritual mourning (D1/D4), with no essential survival-competence transmission. The survival-competence reading interprets the same ritual's function as transmitting adaptive institutional knowledge (D5). The hybrid-transformation reading claims both functions operate simultaneously. The three readings share a referent (the same ritual, the same catastrophic loss) but disagree fundamentally on what function the constraint serves. The ε-invariance principle requires three separate constraint stories, each with its own beneficiary/victim structure and type classification. This file documents ONLY the mourning-practice reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
