% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Directive as Historically Bounded and Superseded
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint story models the 'contextual supersession' reading of the
 *   herem (ban/utter destruction) command in Deuteronomy 7 and related texts.
 *   The reading holds that herem was a historically bounded directive for
 *   Israel's settlement period — a specific, time-limited application of
 *   divine judgment against the Canaanite nations — and that it was morally
 *   superseded by the prophetic trajectory toward universalism (Isaiah,
 *   Jonah, Ruth) and definitively terminated by the Christian new covenant.
 *   The constraint as *currently experienced* is not the ancient command but
 *   the *residual enforcement* of it by fundamentalist communities that treat
 *   it as a timeless mandate for separation. The reading's ε is low because
 *   the standing arrangement it describes — the mainstream interpretive
 *   consensus — extracts almost nothing from its adherents; the victims are
 *   only those still coerced by the residual enforcement. The claimed type is
 *   Mountain because the reading presents the supersession as a structural
 *   feature of the tradition's own logic (prophetic universalism *is* the
 *   tradition's self-correction), not a human choice — but FSM evaluation is
 *   warranted because fundamentalist enforcers benefit from presenting the
 *   constraint as still binding.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.08).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.15).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, mountain).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Directive as Historically Bounded and Superseded").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "biblical_hermeneutics/religious_ethics/commitment_system").

domain_priors:emerges_naturally(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'd0928388-a5ca-4666-89ae-a6a8597ec960').
narrative_ontology:cs_kernel_codification('d0928388-a5ca-4666-89ae-a6a8597ec960', fixed_text).
narrative_ontology:cs_authority_grounding('d0928388-a5ca-4666-89ae-a6a8597ec960', lineage).
narrative_ontology:cs_interpretation_layer_present('d0928388-a5ca-4666-89ae-a6a8597ec960').
narrative_ontology:cs_reading_relation('d0928388-a5ca-4666-89ae-a6a8597ec960', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('d0928388-a5ca-4666-89ae-a6a8597ec960', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('d0928388-a5ca-4666-89ae-a6a8597ec960', foundational, prophetic_universalism_supersedes_herem).
narrative_ontology:cs_axiom_status(prophetic_universalism_supersedes_herem, holdable).
narrative_ontology:cs_axiom_grounding('d0928388-a5ca-4666-89ae-a6a8597ec960', prophetic_universalism_supersedes_herem, deontological).
narrative_ontology:cs_axiom('d0928388-a5ca-4666-89ae-a6a8597ec960', foundational, christian_covenant_terminates_herem_mandate).
narrative_ontology:cs_axiom_status(christian_covenant_terminates_herem_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d0928388-a5ca-4666-89ae-a6a8597ec960', christian_covenant_terminates_herem_mandate, theological).
narrative_ontology:cs_reference_frame('d0928388-a5ca-4666-89ae-a6a8597ec960', settlement_period_herem_mandate).
narrative_ontology:cs_drift_state('d0928388-a5ca-4666-89ae-a6a8597ec960', prophetic_universalism_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d0928388-a5ca-4666-89ae-a6a8597ec960', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, coerced_members_of_residual_fundamentalist_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, mainstream_judaism).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, mainstream_christianity).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism_supersedes_ethnic_particularism).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenant_fulfills_and_terminates_herem_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals raised in communities that still treat herem as a binding mandate for separation and who face shunning, loss of family, or spiritual threat if they attempt exit. The constraint they experience is not the ancient text but the living enforcement of it by their community.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, coerced_members_of_residual_fundamentalist_groups, payer,
    powerless, biographical, identity_locked, local).

% Inherits the prophetic universalism reading that supersedes herem; the constraint's removal from binding obligation is a coordination gain — it resolves the tension between election ethics and universal morality without requiring rejection of the tradition.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainstream_judaism, beneficiary,
    institutional, generational, arbitrage, global).

% Reads the new covenant as fulfilling and terminating the herem mandate; the constraint's historical boundedness is a structural feature of the tradition's self-understanding, not a concession to modernity.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainstream_christianity, beneficiary,
    institutional, generational, arbitrage, global).

% Small communities or leaders who continue to enforce herem-like separation mandates (intermarriage bans, shunning, identity policing) as binding divine law. They administer the residual extraction that this reading identifies as the only live coercive remnant.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, residual_fundamentalist_enforcers, agenda_setter,
    organized, biographical, constrained, local).

% Provides the empirical and philological basis for dating herem to the settlement period and tracing its supersession in the prophetic and early Christian corpora. Does not adjudicate normative authority but supplies the historical referent that makes the supersession reading structurally coherent.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, historical_critical_scholarship, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the tension between particular election and universal moral obligation by relocating the constraint from a binding ethnic-separation mandate to a historical artifact — the community coordinates around the *memory* of the mandate without being *governed* by it.
% TRANSFER_FUNCTION: The historical herem transferred life, land, and labor from designated out-groups to Israel; the supersession reading transfers *authority* from the ancient mandate to the prophetic/universalist trajectory, leaving only residual enforcement in fundamentalist pockets as a cost borne by coerced members.
% ABSENT_VOICES: The designated victims of the original herem (Canaanites, Amalekites, etc.) are historically absent and cannot object; within the reading, their absence is acknowledged as part of the moral problem the supersession addresses. The coerced members of residual fundamentalist groups are the *present* absent voice — they are excluded from the interpretive communities that declare the constraint superseded.
% DISAPPEARANCE_RATIONALE: If the supersession reading vanished overnight, mainstream Judaism and Christianity would still regard herem as historically bounded and morally superseded — the reading describes a consensus that is already structurally embedded in the traditions' self-understanding. Only in residual fundamentalist pockets would the disappearance matter, and there the constraint would *intensify*, not vanish.
% FOUNDING_PROBLEM: How can a tradition grounded in a violent, exclusionary settlement narrative (herem) become a vehicle for universal moral obligation without repudiating its own scriptures?
% FOUNDING_PROBLEM_CORROBORATION: The supersession reading is attested by the prophetic corpus itself (Isaiah 2, 56; Jonah; Ruth) and the early Christian kerygma (Acts 10–15; Galatians 3:28; Ephesians 2:14–16) — witnesses *internal* to the tradition but *external* to the fundamentalist enforcers who benefit from the residual mandate. No corroboration exists from the Canaanite/Amalekite side (historically extinct); the reading's claim about them is a retrospective moral judgment, not a negotiated settlement.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_unchanged).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, ExtMetricName, E),
    domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(herem_command_dt7__contextual_supersession_reading),
    narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.08) for the mainstream reading because the constraint has been relocated from ethnic separation to consent/belief boundaries — intermarriage is no longer a herem violation in mainstream Judaism or Christianity. Suppression is low (0.15) because the mainstream traditions do not enforce herem; the only active suppression is in residual fundamentalist pockets. Theater ratio is moderate (0.25) because some communities perform continuity with the ancient mandate (e.g., opposition to intermarriage framed as 'continuity') while functionally operating on consent/belief criteria. Accessibility collapse (0.35) reflects that alternatives (universalist ethics) are readily available in the tradition's own texts. Resistance (0.30) is the pushback from fundamentalist enforcers against the supersession reading.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different effective extractions per seat: for the mainstream beneficiary seats, χ ≈ 0 (the constraint is a mountain — it imposes no cost). For the coerced fundamentalist members, χ is substantial because they experience active suppression of exit and identity-locked coercion. The fundamentalist enforcers sit in a tension: they *claim* the mountain (divine mandate) but *operate* a snare (extraction via shunning). The reading's structural claim is that the mountain claim is false for the residual enforcement — it is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream Judaism and Christianity are beneficiaries (d near 0.0) — the supersession reading *gives* them a coherent tradition without the moral burden of eternal herem. Coerced members of residual fundamentalist groups are payers (d near 1.0) — they bear the extraction of a mandate the reading says is void. Residual fundamentalist enforcers are agenda_setters for the *residual* constraint (d ~0.6) — they administer a mandate the reading declares superseded. Historical scholarship is an analytical observer (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The original herem mandate was built for a specific settlement problem (securing land and identity in a contested ancient Near Eastern context). That problem is dead. The supersession reading declares the mandate resolved by the tradition's own internal development. The residual enforcement in fundamentalist pockets is a mandate without a founding problem — a classic mandatrophy signature. The reading prevents mislabeling by naming the *original* coordination function (settlement survival) and its *supersession* (universalism), making the residual enforcement legible as extraction without founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herem_historical_boundedness_ambiguity,
    'Is the herem command genuinely historically bounded (limited to the settlement period) or does the text itself claim perpetual validity?',
    'Philological analysis of Deuteronomy 7:1–6, 20:16–18 and Joshua''s implementation vs. later prophetic critique; comparison with ANE treaty curses that are explicitly time-limited.',
    'If the text claims perpetual validity, the supersession reading requires a stronger theory of moral development within the tradition; if historically bounded, the supersession reads the text on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herem_historical_boundedness_ambiguity, empirical, 'Whether the herem texts themselves support historical boundedness or require external moral supersession.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by coerced fundamentalist members structural (community enforcement, economic dependency) or internalized (belief that the mandate is divine and exit is apostasy)?',
    'Post-exit trajectory studies: if suppression persists after leaving the community (guilt, identity fragmentation), the internalized component is significant; if suppression lifts, it is primarily structural.',
    'If internalized, the effective suppression is higher than structural measures suggest — the constraint travels with the agent. This affects the directionality derivation for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in residual fundamentalist enforcement of herem-like mandates.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''herem_command_dt7'' refer to the *textual command* (Deut 7, 20), the *historical practice* (Joshua''s campaigns), or the *ongoing interpretive mandate* (separation ethics)?',
    'Disambiguate by writing separate constraint stories for each referent and linking them via network.affects_constraints. This story addresses the interpretive mandate as superseded.',
    'If the kernel is the textual command, the supersession reading is a hermeneutical move; if the kernel is the ongoing mandate, the supersession reading is a structural claim about the constraint''s current operation. Different ε values follow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing under-determination of the herem kernel across textual, historical, and mandate referents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 2800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(here_tr_t800, herem_command_dt7__contextual_supersession_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement(here_tr_t1400, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1400, 0.25).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(here_tr_t2800, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2800, 0.25).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(here_be_t800, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 800, 0.65).
narrative_ontology:measurement(here_be_t1400, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1400, 0.35).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(here_be_t2800, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2800, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(here_su_t800, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(here_su_t1400, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1400, 0.4).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(here_su_t2800, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2800, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__contextual_supersession_reading, 0.05).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The herem_command_dt7 kernel decomposes into three readings with distinct ε profiles: durable_separation_reading (ε≈0.7, snare/tangled_rope), allegorical_displacement_reading (ε≈0.2, rope/mountain), contextual_supersession_reading (this story, ε≈0.08, mountain). The supersession reading treats the durable mandate as a false summit (FSM candidate) and the allegorical reading as a compatible but distinct hermeneutical strategy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, organized, 0.6).
constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
