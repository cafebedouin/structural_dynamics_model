% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Reading: Community Rebuilt Through Participatory Labor and Plural Communion
 *   domain: Catholic Social Doctrine / Technology Ethics / Political Theology
 *
 * SUMMARY:
 *   This story instantiates the Jerusalem reading of the
 *   human_transcendence_pathway kernel: authentic community rebuilt through
 *   patient, participatory labor under divine blessing, where plurality of
 *   trades, dialects, and degrees of exilic assimilation is integrated into
 *   communion rather than flattened into uniformity. The rebuilding is slow
 *   by design — it trades efficiency for solidarity and legitimacy. Unlike
 *   the Babel reading (unified technological/linguistic self-sufficiency
 *   without reference to transcendence) or the technocratic-vs-incarnational
 *   reading (optimization/limit-elimination versus grace received in
 *   vulnerability), this reading treats diversity itself as a resource for
 *   the community rather than an obstacle to be engineered away. It is
 *   generated here as a single, clean, ε-invariant constraint; the sibling
 *   readings are separate constraint stories, not variants folded into this
 *   one.
 *
 * KEY AGENTS:
 *   - returning_exiles: primary participants and primary beneficiaries — bear the labor and receive restored civic and religious standing
 *   - wider_community_of_jerusalem: secondary beneficiaries of restored infrastructure and order
 *   - artisan_and_laboring_classes: bear the material sacrifice the solidarity model requires, without central coercion
 *   - civic_and_religious_leadership: sets the agenda through persuasion and shared covenant appeal, not enforcement
 *   - surrounding_regional_powers: excluded from the internal deliberation, structurally positioned as objectors outside the frame
 *   - theological_and_political_observers: analytical seat reading this narrative as a model against Babel and technocratic alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.22).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.12).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Reading: Community Rebuilt Through Participatory Labor and Plural Communion").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "Catholic Social Doctrine / Technology Ethics / Political Theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '3a0550c1-2f8b-4e6f-976c-2719f380a408').
narrative_ontology:cs_kernel_codification('3a0550c1-2f8b-4e6f-976c-2719f380a408', distributed).
narrative_ontology:cs_authority_grounding('3a0550c1-2f8b-4e6f-976c-2719f380a408', practice).
narrative_ontology:cs_interpretation_layer_present('3a0550c1-2f8b-4e6f-976c-2719f380a408').
narrative_ontology:cs_reading_relation('3a0550c1-2f8b-4e6f-976c-2719f380a408', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a0550c1-2f8b-4e6f-976c-2719f380a408', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('3a0550c1-2f8b-4e6f-976c-2719f380a408', foundational, plurality_integrated_not_erased).
narrative_ontology:cs_axiom_status(plurality_integrated_not_erased, holdable).
narrative_ontology:cs_axiom_grounding('3a0550c1-2f8b-4e6f-976c-2719f380a408', plurality_integrated_not_erased, deontological).
narrative_ontology:cs_axiom('3a0550c1-2f8b-4e6f-976c-2719f380a408', foundational, transcendence_received_through_patient_shared_labor).
narrative_ontology:cs_axiom_status(transcendence_received_through_patient_shared_labor, holdable).
narrative_ontology:cs_axiom_grounding('3a0550c1-2f8b-4e6f-976c-2719f380a408', transcendence_received_through_patient_shared_labor, theological).
narrative_ontology:cs_reference_frame('3a0550c1-2f8b-4e6f-976c-2719f380a408', covenant_community_restored_through_plural_labor).
narrative_ontology:cs_drift_state('3a0550c1-2f8b-4e6f-976c-2719f380a408', contemporary_pluralist_reappropriation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3a0550c1-2f8b-4e6f-976c-2719f380a408', '2026-06-19T00:00:00Z').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, wider_community_of_jerusalem).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, artisan_and_laboring_classes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, artisan_and_laboring_classes).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, plurality_integrated_into_communion_doctrine).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, patient_participatory_rebuilding_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Return to a ruined city with few resources, differing dialects, trades, and degrees of assimilation to exile cultures. They participate directly in the rebuilding labor (walls, temple, civic order) rather than having it done for or to them. Their exit options are limited by attachment to ancestral land and covenant identity, but this is experienced as belonging rather than entrapment; they gain restored civic standing and a voice in the community's reconstitution.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, returning_exiles, agenda_setter).

% Includes those who remained in the land during exile, neighboring peoples drawn into cooperative or contested relations, and successive generations who inherit the rebuilt city. They benefit from restored infrastructure, worship, and social order achieved through negotiated, participatory effort rather than imposed uniformity.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, wider_community_of_jerusalem, beneficiary,
    moderate, generational, constrained, regional).

% Do the physical work of rebuilding — masonry, carrying materials, organizing watch shifts — often at cost to their own household economies and with no guarantee of proportional reward. They bear the material sacrifice (slower personal advancement, foregone efficiency) that solidarity requires, but retain voice in how the work is organized and are not coerced into it by a central authority; their participation is the coordination mechanism itself.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, artisan_and_laboring_classes, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, artisan_and_laboring_classes, payer).

% Figures who organize the labor, negotiate with surrounding powers, and interpret the covenant obligations that ground the rebuilding effort. They set the agenda for how work is distributed and how disputes over resources or lineage purity are adjudicated, but they operate through persuasion, communal deliberation, and appeal to shared religious commitment rather than coercive enforcement — their authority is accountable to the community's ongoing consent.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, civic_and_religious_leadership, agenda_setter,
    organized, generational, constrained, regional).

% Neighboring polities and interests who view the rebuilding with suspicion or hostility, sometimes obstructing it, and who are not party to the internal deliberations that shape the rebuilding's terms. Their objections (loss of regional leverage, disrupted trade or security arrangements) are not addressed within this constraint's own structure.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, surrounding_regional_powers, excluded,
    powerful, biographical, mobile, regional).

% Later interpreters — theologians, political theorists, social doctrine commentators — who read the Jerusalem narrative as a model of transcendence-under-grace achieved through plural, participatory community-building, and who contrast it against technocratic and Babel-style alternatives without being party to the original rebuilding effort.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, theological_and_political_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of rebuilding a devastated city and its institutions when no single actor has sufficient resources, legitimacy, or knowledge to do it alone — requiring dispersed exiles, remaining residents, and diverse trades to pool labor and negotiate shared standards without erasing their distinct origins.
% TRANSFER_FUNCTION: Moves labor, material sacrifice, and foregone individual efficiency from participating households and workers into shared civic and religious infrastructure (walls, temple, social order), returning restored communal identity, security, and worship capacity to the same population that bore the cost.
% ABSENT_VOICES: Surrounding regional powers who lose leverage or trade advantage from a re-consolidated Jerusalem are not part of the internal deliberation; some remained-in-the-land residents may also have grievances about returning exiles' claims to land or leadership that are only partially aired within the narrative's own frame.
% DISAPPEARANCE_RATIONALE: If the patient, participatory rebuilding pathway disappeared, the community would either fracture into disconnected factions pursuing individual survival strategies, or fall under externally imposed order (regional power domination or an internal strongman) — the walls, temple, and covenant-based civic identity depend on this specific mode of pooled, plural, consent-based labor for their existence.
% FOUNDING_PROBLEM: A displaced and fragmented people, returning from exile, needed to reconstitute a viable community and civic order from physical ruin without either dissolving into competing factions or imposing uniformity that would erase the plurality of trades, dialects, and degrees of assimilation exiles had accumulated.
% FOUNDING_PROBLEM_CORROBORATION: Later theological and political-theory commentators (outside the original beneficiary community) attest that the model of plural, participatory rebuilding under shared covenant remains a live template for community reconstruction; some contemporary political theorists studying post-conflict reconstruction independently corroborate that participatory, plurality-preserving models outperform imposed-uniformity models on durability, though they do not necessarily share the theological framing of divine blessing.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.22 at interval end, starting slightly higher at 0.30 and declining as the rebuilding stabilizes and participatory norms take root) because the primary transfer is self-imposed sacrifice of efficiency for solidarity, not coercive rent extraction — the same population that bears the cost receives the restored good. Suppression is low (0.12): the model depends on persuasion, shared identity, and voluntary participation rather than force; the accessibility_collapse and resistance values are both low because alternatives (individual survival strategies, factional secession, appeal to external powers) remain genuinely available and are not systematically closed off — people could choose otherwise and mostly choose to remain. Theater ratio is low and only mildly rising (0.10 to 0.15) reflecting that the rebuilding remains substantially functional throughout the measured interval, with only a small and stable margin of purely symbolic or performative activity (ceremonial elements of covenant renewal) alongside the real coordination work.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the artisan and laboring classes, the constraint could in principle be misread as extractive (real material sacrifice, foregone efficiency, no guaranteed proportional return) — but the structural data establishes they retain voice in how work is organized and are not coerced by a separate extracting party; the sacrifice flows into a shared good they also receive. From the seat of surrounding regional powers, the same rebuilding looks like a loss of external leverage, but that is a cost borne by an excluded outside party, not a payer inside the constraint's own structure — this is why no victims are declared for this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Returning exiles and the wider community are beneficiaries who also do the work — this reading structurally collapses the beneficiary/payer distinction into a shared position, which is the mark of genuine coordination rather than extraction: the same agents who sacrifice efficiency are the agents who receive the restored civic and religious good. No stakeholder group carries a purely extractive relationship to the constraint. Civic and religious leadership occupies an agenda-setting role but one accountable to ongoing communal consent rather than coercive enforcement, which keeps its directionality far from a captured or extractive institutional seat. Surrounding regional powers are excluded rather than exploited — their objection is to loss of leverage, not to being extracted from by this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconstituting a fragmented, displaced people into a viable, plural community) is authored as contested rather than flatly dead or live: within the tradition's own reading it remains perpetually live as a template (each generation must re-integrate plurality into communion anew), while external political-theory observers corroborate its practical durability without necessarily endorsing the theological grounding. This keeps the classification from mistaking a genuinely renewable coordination problem for either an obsolete relic (which would risk misreading ongoing formation labor as pure theater) or a permanently settled extraction (which the low ε and low suppression values structurally rule out).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jerusalem_reading_exclusivity,
    'Does the Jerusalem reading''s premise (transcendence through plural, participatory communion under divine blessing) structurally foreclose the Babel reading''s premise (self-sufficient stability through unified technological/linguistic power without transcendent reference), or can both be held as coexisting positions by different communities without contradiction?',
    'Examine whether any single political-theological framework has historically or coherently held both premises simultaneously (transcendent grounding as necessary AND unnecessary for communal stability) — if no framework can hold both, foreclosure is structurally supported; if different communities simply disagree while each remains internally coherent, coexistence is supported.',
    'If forecloses, the two readings represent a genuine either/or choice at the level of a single community''s self-understanding, sharpening the stakes of adopting one framework over the other. If coexists_with, the readings persist as parallel live options across different communities without resolution, which is the relation this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jerusalem_reading_exclusivity, conceptual, 'Whether Jerusalem and Babel readings can be simultaneously held or are mutually exclusive at the framework level.').

omega_variable(
    efficiency_sacrifice_measurement,
    'Is the sacrifice of efficiency for solidarity in the Jerusalem model a genuinely costless-to-victims coordination mechanism, or does it quietly produce diffuse victims (e.g., those whose individual advancement is permanently foregone) who are simply not visible within the tradition''s own self-narration?',
    'Comparative historical analysis of participation records, if available, to determine whether the material sacrifice borne by laboring classes was ever compensated, recognized, or remained a permanent uncompensated cost concentrated on specific sub-groups (e.g., particular clans or trades).',
    'If diffuse uncompensated costs concentrate on an identifiable sub-group, this reading would need a victims declaration and would shift toward tangled_rope; the current authoring (no victims, low ε) assumes the sacrifice is genuinely shared and voluntary rather than concentrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_sacrifice_measurement, empirical, 'Whether the efficiency-for-solidarity trade conceals a hidden, concentrated victim class.').

omega_variable(
    divine_blessing_naturality_ambiguity,
    'Is the framing of this rebuilding as occurring ''under divine blessing'' a genuine theological claim about transcendent grounding, or a constructed legitimating narrative that benefits the leadership seat by grounding their agenda-setting authority in unquestionable religious sanction?',
    'Textual and historical analysis of whether the divine-blessing framing emerged from the community''s own bottom-up religious experience versus was retroactively imposed by leadership to consolidate authority over the rebuilding process.',
    'If constructed primarily for leadership legitimation, the agenda_setter seat''s directionality should shift toward a more self-interested position and suppression/extraction metrics might need revision upward; if genuine, the low-coercion, high-consent reading holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_blessing_naturality_ambiguity, conceptual, 'Whether the divine-blessing framing is genuine theological grounding or leadership-legitimating construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t8, human_transcendence_pathway__jerusalem_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement_basis(huma_tr_t8, observed).
narrative_ontology:measurement(huma_tr_t16, human_transcendence_pathway__jerusalem_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement_basis(huma_tr_t16, observed).
narrative_ontology:measurement(huma_tr_t24, human_transcendence_pathway__jerusalem_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(huma_tr_t24, observed).
narrative_ontology:measurement(huma_tr_t32, human_transcendence_pathway__jerusalem_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement_basis(huma_tr_t32, observed).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(huma_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t8, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement_basis(huma_be_t8, observed).
narrative_ontology:measurement(huma_be_t16, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement_basis(huma_be_t16, observed).
narrative_ontology:measurement(huma_be_t24, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 24, 0.22).
narrative_ontology:measurement_basis(huma_be_t24, observed).
narrative_ontology:measurement(huma_be_t32, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 32, 0.22).
narrative_ontology:measurement_basis(huma_be_t32, observed).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement_basis(huma_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(human_transcendence_pathway__jerusalem_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__jerusalem_reading, 0.1).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the human_transcendence_pathway kernel per the ε-invariance principle: Babel (unified technological self-sufficiency without transcendence), Jerusalem (this story — plural, participatory communion under divine blessing), and technocratic_vs_incarnational (limit-elimination vs. grace-in-vulnerability). Each carries its own ε, beneficiary/victim structure, and claimed type; they are linked here as a constraint family rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
