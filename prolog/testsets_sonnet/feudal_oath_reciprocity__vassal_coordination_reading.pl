% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath as Fixed Reciprocal Charter Obligation (Vassal Coordination Reading)
 *   domain: medieval political economy / legal history / institutional analysis
 *
 * SUMMARY:
 *   The feudal oath — homage and fealty between a lord and a vassal, backed
 *   by charter or customary text specifying the bounded schedule of military
 *   service, counsel, and aids owed in exchange for land and protection — is
 *   a single institutional label covering structurally distinct claims
 *   depending on which enforcement mechanism and which party's bound the
 *   analyst foregrounds. This story instantiates the VASSAL COORDINATION
 *   reading: the oath as a fixed, textually bounded reciprocal schedule that
 *   both parties can invoke against the other's overreach, functioning as
 *   genuine coordination infrastructure in a territory lacking centralized
 *   administration. This is deliberately narrow — it is not a claim that
 *   feudal tenure was benign in general, only that THIS reading (fixed
 *   reciprocal bounds, mutual enforceability via charter and peer court) is a
 *   coordination mechanism with negligible structural extraction. The sibling
 *   readings (lord_extraction_reading: oath as cover for open-ended
 *   extraction bounded only by vassal capacity;
 *   ecclesiastical_mediation_reading: oath bound by Church-imposed charity
 *   obligations) are separate constraints with their own ε, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - vassal_landholders: primary co-beneficiary (moderate/constrained) — receives land and protection, owes bounded service
 *   - liege_lords: agenda-setter and co-beneficiary (powerful/constrained) — grants land and protection, bound by the same charter ceiling
 *   - peer_vassals_court: enforcement and interpretation body (organized/mobile) — adjudicates charter compliance disputes
 *   - unlanded_peasantry: excluded voice (powerless/trapped) — not party to the oath, bears downstream burdens with no charter standing
 *   - regional_peace_and_succession_order: the coordination good itself, not an actor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.22).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath as Fixed Reciprocal Charter Obligation (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval political economy / legal history / institutional analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '0319003f-bd47-4d62-ba8a-d5bd467a1f6c').
narrative_ontology:cs_kernel_codification('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', fixed_text).
narrative_ontology:cs_authority_grounding('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', practice).
narrative_ontology:cs_interpretation_layer_present('0319003f-bd47-4d62-ba8a-d5bd467a1f6c').
narrative_ontology:cs_reading_relation('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', foundational, charter_ceiling_mutually_invocable).
narrative_ontology:cs_axiom_status(charter_ceiling_mutually_invocable, holdable).
narrative_ontology:cs_axiom_grounding('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', charter_ceiling_mutually_invocable, conventional).
narrative_ontology:cs_axiom('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', foundational, peer_court_judgment_binds_lord).
narrative_ontology:cs_axiom_status(peer_court_judgment_binds_lord, holdable).
narrative_ontology:cs_axiom_grounding('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', peer_court_judgment_binds_lord, conventional).
narrative_ontology:cs_reference_frame('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', bounded_reciprocal_charter_schedule).
narrative_ontology:cs_drift_state('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', post_centralization_state_formation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0319003f-bd47-4d62-ba8a-d5bd467a1f6c', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassal_landholders).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, regional_peace_and_succession_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, vassal_landholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold land and jurisdiction in exchange for a fixed, enumerated set of military and counsel obligations set out in the charter or customary record of the oath. They know in advance the ceiling on what can be demanded of them — a specified number of days' service, specified aids on specified occasions (ransom, eldest son's knighting, eldest daughter's marriage) — and can invoke the charter text against demands exceeding it. Exit from a given lord is difficult (land and lineage are entangled with the relationship) but the terms themselves are not open-ended, which is the coordination good this reading identifies.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassal_landholders, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassal_landholders, payer).

% Grant land and protection in exchange for military service and counsel, and administer the oath's enforcement (calling up levies, adjudicating disputed obligations, hearing homage). Their own capacity to extract is bounded by the same charter text that binds the vassal — demanding beyond the enumerated obligations invites the vassal's recognized right of diffidatio (renunciation) and peer judgment in the lord's own court. This reading treats the lord as a co-bound party to a fixed schedule, not as an unconstrained extractor.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords, beneficiary).

% The assembled body of fellow vassals who sit in judgment when a lord is accused of exceeding the charter's bounds or a vassal is accused of default. Their role is to interpret and enforce the fixed schedule of obligations as a coordination mechanism among peers with parallel interests in a stable, predictable regime, rather than as agents of either party.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, peer_vassals_court, observer,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, peer_vassals_court, agenda_setter).

% Not an actor but the coordination good this reading names: a predictable succession, mustering, and dispute-resolution order across a fragmented territory lacking centralized administration. It is what the fixed, mutually enforceable obligation schedule produces when both parties keep to the charter.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, regional_peace_and_succession_order, beneficiary,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(feudal_oath_reciprocity__vassal_coordination_reading, regional_peace_and_succession_order).

% Work the land under the vassal's jurisdiction but are not party to the oath at all — the charter's reciprocal bounds run between lord and vassal only. They have no voice in setting or enforcing the schedule of obligations and bear whatever burdens the vassal in turn passes downward to meet the vassal's own service quota; this reading does not extend the coordination-good framing to them, and their absence from the charter relationship is exactly what the sibling extraction reading would foreground.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, unlanded_peasantry, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an otherwise unbounded, renegotiable relationship of protection-for-service into a fixed, mutually legible schedule of obligations — a specific number of knight's-fee service days, specified aids on specified occasions, specified counsel duties — recorded or attested in charter form so that neither party can unilaterally escalate demands without breaching a text both can invoke.
% TRANSFER_FUNCTION: Land, jurisdiction, and protection flow from lord to vassal; military service, counsel, and specified aids flow from vassal to lord — both flows are capped by the charter's enumerated terms rather than open to continuous renegotiation.
% ABSENT_VOICES: The unlanded peasantry working the vassal's land are not party to the oath and have no forum to contest obligations passed down to them; the sibling ecclesiastical_mediation_reading would also note the Church's independent claim to bound the relationship by sacramental obligation, a claim this reading treats as external to the charter mechanism itself.
% DISAPPEARANCE_RATIONALE: If the fixed, charter-enforced schedule vanished and reverted to open-ended personal fealty with no textual ceiling, both lords and vassals would lose the predictability that lets them plan military musters, successions, and inheritances years in advance; peer courts would lose their reference standard for adjudicating disputes, and the region would revert to case-by-case negotiation backed only by relative force — a materially different and less stable arrangement.
% FOUNDING_PROBLEM: In a territory without centralized administrative capacity to levy troops, collect revenue, or adjudicate disputes directly, some mechanism was needed to let a lord raise a reliable, predictable military and administrative capacity from landholders without renegotiating terms with each individually and continuously, while giving those landholders enough certainty about the ceiling on demands to invest in the land and pass it to heirs.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians examining surviving charters, feudal registers (e.g., cartularies, the Domesday-adjacent inquest records, and later custumals) attest that the coordination problem — raising predictable levies and settling succession without central bureaucracy — was real in the high medieval period and is now solved by other means (standing armies, centralized taxation, statutory inheritance law) in every jurisdiction where feudal tenure has since been abolished. This corroboration comes from historians and legal scholars outside the beneficiary class (neither lords nor vassals), though it should be noted that no contemporary party to the arrangement itself survives to attest either way.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) and rising only modestly across the interval because the defining feature of THIS reading is that the schedule is fixed and textually enumerated — demands beyond the charter's terms are the exception that peer courts exist to check, not the rule. Suppression is low (0.18): the mechanism that holds the arrangement together is mutual textual invocation and peer adjudication, not coercive enforcement against a trapped party, though the vassal's practical exit is genuinely constrained by land and lineage entanglement (hence exit_options: constrained rather than mobile). Theater ratio is low and stable (0.08 rising to ~0.15): the charter obligations are functionally invoked (musters actually occur on the schedule, aids are actually collected at the specified life events) rather than performed. Accessibility collapse is moderate (0.35) — once sworn, the specific lord-vassal bond is hard to exit, but the SCHEDULE OF TERMS itself remains a live, contestable text rather than a naturalized inevitability, which is why accessibility collapse is well below mountain-range values.
 *
 * DIRECTIONALITY LOGIC:
 *   Both vassal and lord are declared beneficiaries because the coordination reading holds that each gains a predictable ceiling neither could unilaterally extract past without breaching a text the other side can invoke in a forum (peer court) that is not simply an instrument of the stronger party. This is what distinguishes the coordination reading from the extraction reading: in the extraction reading the lord's power to set terms is effectively unbounded by vassal service capacity alone; here it is bounded by the charter text and by a peer-judgment mechanism that exists independent of either party's individual power. The excluded unlanded_peasantry are marked non-beneficiary, non-victim WITHIN THIS CONSTRAINT because the oath's reciprocal bound simply does not run to them — their situation is a separate, un-modeled cost this reading is honest about not capturing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (predictable levy-raising and succession order without centralized administration) is dead in the sense that centralized states now perform this function through standing bureaucracies, yet the reading resists retroactive mandatrophy framing: at the time and place this reading describes, the fixed charter schedule was solving a live problem for both co-bound parties, not merely providing cover for one-sided extraction. Reading this constraint as a rope (bounded, mutual, low suppression) prevents collapsing it into the lord_extraction_reading's snare/tangled-rope framing, which is the mislabeling this decomposition is built to avoid — the two readings differ on exactly the empirical question of whether the charter ceiling was genuinely binding on the lord in practice, which is why that question is routed to an omega rather than resolved by fiat here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_ceiling_actually_binding,
    'In documented practice, did the charter''s enumerated obligation schedule actually constrain lords from demanding more than specified, or was the schedule routinely exceeded with the peer court mechanism too weak or too captured by lordly power to enforce it?',
    'Comparative study of surviving charter texts against documented aid-levies, muster records, and recorded diffidatio (renunciation) cases: a high rate of charter-exceeding demands with rare or unsuccessful peer-court challenge would support the lord_extraction_reading over this one for the specific relationships studied.',
    'If the ceiling was routinely exceeded in practice, this reading''s low-ε rope classification is descriptively wrong for those relationships and the lord_extraction_reading''s tangled_rope/snare framing is the accurate structural account; if the ceiling held, this reading is vindicated as the operative structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_ceiling_actually_binding, empirical, 'Whether the charter''s bound was practically enforceable or merely nominal.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice among the three sibling readings (vassal_coordination, lord_extraction, ecclesiastical_mediation) determined by which specific lord-vassal relationship and period is examined, or is it an irreducible interpretive choice about how to weight the same surviving evidence?',
    'Case-level historical analysis of specific charters and their enforcement histories, cross-referenced with regional variation in peer-court strength and ecclesiastical intervention frequency, would show whether the three readings sort cleanly onto different empirical cases or genuinely compete over the same cases.',
    'If the readings sort onto different cases, the kernel decomposition is a taxonomy of historically distinct regimes; if they compete over the same evidence, the decomposition documents a genuine interpretive underdetermination in feudal legal history requiring separate historiographic treatment rather than resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether sibling kernel readings map to distinct cases or to competing interpretations of the same evidence.').

omega_variable(
    peasantry_exclusion_defensibility,
    'Is it structurally correct to treat the unlanded peasantry as fully external to this constraint (no beneficiary or victim status), or does the vassal''s bounded service obligation systematically transfer pressure onto the peasantry in a way that should count as this constraint''s own downstream extraction rather than a separate, unmodeled cost?',
    'Manorial account analysis tracing whether periods of heavier lordly demand on vassals correlate with heavier vassal demand on tenant labor and dues, which would indicate the charter''s bound displaces rather than absorbs extractive pressure.',
    'If displacement is demonstrated, the coordination reading''s claim of ''no structural victim'' would need revision to include the peasantry as an indirect victim, moving this story''s classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasantry_exclusion_defensibility, empirical, 'Whether obligation pressure absorbed at the lord-vassal level is passed downward onto excluded parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(feud_tr_t160, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 160, 0.12).
narrative_ontology:measurement(feud_tr_t240, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 240, 0.14).
narrative_ontology:measurement(feud_tr_t320, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 320, 0.16).
narrative_ontology:measurement(feud_tr_t400, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 400, 0.15).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(feud_be_t160, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 160, 0.2).
narrative_ontology:measurement(feud_be_t240, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 240, 0.22).
narrative_ontology:measurement(feud_be_t320, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 320, 0.24).
narrative_ontology:measurement(feud_be_t400, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 400, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feudal_oath_reciprocity__vassal_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the feudal_oath_reciprocity kernel. vassal_coordination_reading claims low-ε rope status (fixed, mutually enforceable bounds, no structural victim). lord_extraction_reading claims high-ε tangled_rope or snare status (same oath, read as extraction bounded only by vassal capacity, with vassal and downstream peasantry as victims). ecclesiastical_mediation_reading claims a distinct ε reflecting the Church's independent moral ceiling on secular extraction. All three are linked here per the ε-invariance principle: they are not three measurements of one constraint but three structurally distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
