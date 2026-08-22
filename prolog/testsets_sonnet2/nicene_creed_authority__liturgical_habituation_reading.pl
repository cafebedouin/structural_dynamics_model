% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity-Boundary Marker
 *   domain: religious/social
 *
 * SUMMARY:
 *   This story authors ONE reading of the Nicene Creed kernel: the creed
 *   considered purely as a liturgical performance that marks identity and
 *   belonging, independent of whether reciters cognitively assent to its
 *   metaphysical content. Under this reading, the creed functions as a
 *   coordination device — a shared, low-cost, portable text that lets
 *   dispersed congregations recognize one another as one body across
 *   centuries and geography. This is structurally distinct from (a) the
 *   strict orthodox reading, which treats the same words as binding
 *   metaphysical assent enforceable by sanction, and (b) the symbolic
 *   confessional reading, which grounds authority in community discernment
 *   and personal faith rather than performance per se. Each reading is a
 *   separate constraint with its own ε; this file's ε stays low and stable
 *   because the referent here is habituated communal recitation, not
 *   doctrinal enforcement.
 *
 * KEY AGENTS:
 *   - worshipping_congregations: primary beneficiaries of shared identity marker (organized/mobile)
 *   - liturgical_clergy: administers the liturgical rhythm, benefits from legible continuity (institutional/mobile)
 *   - denominational_bodies: uses recitation as trans-local coordination device (institutional/mobile)
 *   - doubting_or_dissenting_believers: participate in performance while holding private doubt, unsanctioned under this reading (moderate/mobile)
 *   - strict_orthodox_authorities: excluded voice belonging to the sibling constraint, not this one (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.15).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity-Boundary Marker").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "religious/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, 'eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb').
narrative_ontology:cs_kernel_codification('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', fixed_text).
narrative_ontology:cs_authority_grounding('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', practice).
narrative_ontology:cs_interpretation_layer_present('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb').
narrative_ontology:cs_reading_relation('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', foundational, recitation_constitutes_belonging_independent_of_assent).
narrative_ontology:cs_axiom_status(recitation_constitutes_belonging_independent_of_assent, holdable).
narrative_ontology:cs_axiom_grounding('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', recitation_constitutes_belonging_independent_of_assent, conventional).
narrative_ontology:cs_axiom('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', secondary, identity_boundary_maintained_by_practice_not_verification).
narrative_ontology:cs_axiom_status(identity_boundary_maintained_by_practice_not_verification, holdable).
narrative_ontology:cs_axiom_grounding('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', identity_boundary_maintained_by_practice_not_verification, instrumental).
narrative_ontology:cs_created_at('eb0932a9-4be5-4a7d-b2c4-f4c1116cdcdb', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, worshipping_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, denominational_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, doubting_or_dissenting_believers).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, communal_recitation_constitutes_belonging).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recites the creed communally as part of the liturgy, week after week, generation after generation. The recitation binds the group together as a recognizable body — 'we are the people who say these words together' — regardless of how each individual member privately parses the metaphysical claims. Exit from a given congregation is real and low-cost (denominational shopping, joining a non-creedal tradition, or lapsing entirely); no one is coerced into staying.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, worshipping_congregations, beneficiary,
    organized, generational, mobile, global).

% Leads the recitation, selects liturgical forms, and administers the rhythm of communal worship in which the creed is embedded. Clergy benefit from a stable, recognizable liturgical structure that makes congregational identity legible and transferable across parishes and denominations, but they exercise essentially no coercive sanction over private belief — their administrative role concerns performance and continuity, not metaphysical policing.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_clergy, agenda_setter,
    institutional, generational, mobile, global).

% Uses the shared liturgical text as a low-cost coordination device across thousands of geographically dispersed congregations — a common recitation lets far-flung parishes recognize each other as the same body without requiring uniform private cognition. This is a genuine collective-action solution: coordinating identity at scale would otherwise require constant doctrinal surveillance.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, denominational_bodies, beneficiary,
    institutional, civilizational, mobile, global).

% Recites the creed alongside everyone else while privately holding agnostic, metaphorical, or even dissenting readings of its metaphysical content. Under this reading, the liturgical performance itself confers belonging and continuity of identity, so these believers are not excluded or sanctioned merely for private doubt — they remain full participants in the boundary-marking practice as long as they keep performing it. Their exit option (leaving the tradition) is unconstrained by external force, only by relational and habitual cost.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, doubting_or_dissenting_believers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, doubting_or_dissenting_believers, observer).

% Would object that this reading empties the creed of binding metaphysical force and reduces confession to mere social performance. They are not part of THIS constraint's operation (that is a different reading, a sibling constraint) but they represent the live alternative voice that treats identical liturgical words as cognitively binding rather than merely socially constitutive.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, strict_orthodox_authorities, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, low-overhead ritual text that lets geographically and temporally dispersed worshippers recognize one another as members of the same body without requiring verification of uniform private metaphysical belief — solving the coordination problem of maintaining a recognizable collective identity across scale and centuries.
% TRANSFER_FUNCTION: Moves very little of material value; what it transfers is symbolic — a felt sense of continuity, belonging, and legitimacy accrues to congregations, clergy, and denominational structures from the fact of shared recitation, at negligible cost to any individual participant.
% ABSENT_VOICES: Strict orthodox authorities who hold that the creed's function is cognitive assent to fixed metaphysical propositions are not voices within this reading's operation — they would object that treating the creed as pure performance evacuates its doctrinal force, but that objection belongs to a sibling constraint (the strict orthodox reading), not this one.
% DISAPPEARANCE_RATIONALE: If communal creedal recitation vanished overnight, congregations would lose a low-cost, ready-made marker of trans-local identity and would need to reconstitute belonging through other means (shared hymnody, informal custom, doctrinal statements read individually) — some structure would rearrange. But because this reading holds the function is social rather than metaphysically binding, many participants and observers would say the underlying communities and their faith commitments persist largely unchanged; the parties dispute how much would actually rearrange.
% FOUNDING_PROBLEM: Early Christian communities, dispersed across the Mediterranean and lacking centralized institutional machinery, needed a portable, memorizable, orally transmissible marker that let strangers recognize each other as fellow members of the same religious body during communal worship.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of religion and liturgical historians outside any single denomination's hierarchy (e.g., scholars studying ritual and group cohesion in comparative religion) attest that shared liturgical recitation continues to function as a boundary-marking and cohesion mechanism independent of doctrinal literacy among participants; this corroboration comes from academic observers rather than from the clergy or denominational bodies that benefit from the practice.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, contested).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.08 at interval end) because under this reading nothing coercive rides on the recitation: no material transfer, no sanction for private disbelief, no captured exit. Suppression is low (0.15) because participation is not compelled by force — attendance, recitation, and continued membership are freely exited. Theater ratio is authored moderately high and rising over the long interval (0.20 → 0.42) because as an increasing share of participants recite the creed without engaging its metaphysical content, the practice increasingly resembles pure ritual performance relative to its original catechetical function — this is a genuine drift in the ratio of performative to doctrinally-substantive activity, not a defect in the reading itself. Accessibility collapse is moderate (0.30): alternative forms of communal identity marking exist and are used by other traditions, so alternatives have not collapsed as completely as under a genuine mountain. Resistance is very low (0.10) because almost no one experiences the mere act of recitation as something to resist.
 *
 * PERSPECTIVAL GAP:
 *   From the congregation and clergy seats, communal recitation looks like straightforward, low-friction coordination — a rope. From the excluded strict-orthodox seat (which belongs to a different constraint, not this one), the identical words look like a diluted or evacuated form of doctrinal commitment. The engine computing THIS reading's structural data should return a rope classification; the divergence with the sibling reading's classification is the intended output of the kernel-decomposition, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Congregations, clergy, and denominational bodies are declared beneficiaries because the coordination function genuinely serves them — they get low-cost trans-local identity coherence at negligible individual cost. There are no declared victims because under this reading no one is extracted from: doubting participants are not sanctioned, exit is mobile for everyone, and no material transfer occurs. This keeps directionality clustered near the beneficiary end for all named agents, consistent with the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (portable trans-local identity marker for a dispersed early community) remains live in the sense that denominations still use shared liturgy for exactly this coordination function — the practice has not become a hollow shell defending a vanished need. But the rising theater ratio signals a genuine, worth-tracking drift: an increasing fraction of the constraint's operation is now performative continuity-signaling rather than the original catechetical function of transmitting shared belief content. This is captured honestly in the metrics rather than by inflating extractiveness or suppression, which would misclassify low-coercion ritual drift as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_assent_separability,
    'Is liturgical performance genuinely separable from cognitive metaphysical assent, or does the act of communal recitation itself constitute a weak form of assent that this reading under-describes?',
    'Longitudinal sociological study of self-reported belief among habitual reciters, compared across denominations with differing catechetical emphasis, to determine whether recitation without instruction produces measurable belief formation over time.',
    'If recitation reliably produces belief formation regardless of intent, this reading understates the creed''s cognitive-formative function and the boundary with the strict orthodox reading becomes less structurally distinct than claimed; if recitation and belief are genuinely decoupled, this reading''s low-ε rope classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_assent_separability, empirical, 'Whether liturgical performance causally produces the metaphysical assent it claims to be independent of.').

omega_variable(
    kernel_decomposition_boundary_location,
    'Is the three-way decomposition of the Nicene Creed kernel (liturgical/orthodox/symbolic) the correct carving, or does the liturgical-habituation function actually vary by denomination in ways that would require further splitting (e.g., low-church vs. high-church liturgical traditions have very different theater ratios)?',
    'Comparative ecclesiological survey of creedal recitation practice and theater-ratio proxies (attendance-without-catechesis rates, clergy-reported emphasis on doctrinal instruction) across denominational families.',
    'If theater ratio and coordination function vary substantially by denominational family, this single story averages over structurally distinct sub-constraints and should itself be decomposed further per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_decomposition_boundary_location, conceptual, 'Whether the chosen kernel reading is itself internally homogeneous or should be further decomposed by denominational tradition.').

omega_variable(
    beneficiary_vs_natural_practice_ambiguity,
    'Is communal creedal recitation better understood as a constructed institutional practice with identifiable organizational beneficiaries, or as an emergent, low-agency social habit that no particular party designed or controls?',
    'Historical analysis of the creed''s promulgation (Nicaea 325, Constantinople 381) versus its subsequent organic diffusion into weekly liturgy, distinguishing top-down institutional adoption from bottom-up habitual uptake.',
    'If the practice is substantially organically diffused rather than institutionally engineered, the declared beneficiaries (clergy, denominational bodies) may be overstated as designers/administrators and understated as incidental beneficiaries of an emergent practice — this would push the classification further toward a genuinely low-agenda-setting rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_natural_practice_ambiguity, conceptual, 'Whether declared institutional beneficiaries actually administer the practice or merely benefit from an emergent one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nice_tr_t300, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 300, 0.25).
narrative_ontology:measurement(nice_tr_t700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 700, 0.3).
narrative_ontology:measurement(nice_tr_t1100, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1100, 0.35).
narrative_ontology:measurement(nice_tr_t1500, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1700, 0.42).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(nice_be_t300, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 300, 0.05).
narrative_ontology:measurement(nice_be_t700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 700, 0.06).
narrative_ontology:measurement(nice_be_t1100, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1100, 0.07).
narrative_ontology:measurement(nice_be_t1500, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1700, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nicene_creed_authority__liturgical_habituation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family decomposing the natural-language concept 'the authority of the Nicene Creed' per the ε-invariance principle. This story (liturgical_habituation_reading) supplies the low-ε social substrate that the strict_orthodox_reading (high suppression, enforcement-backed) and the symbolic_confessional_reading (moderate, discernment-grounded) both build upon institutionally. Each sibling has its own ε, its own beneficiary/victim structure, and its own claimed type; they are linked here rather than merged because measuring 'the creed's authority' by enforcement outcomes versus by habitual performance yields incompatible ε values — exactly the signal that triggers decomposition rather than a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
