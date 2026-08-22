% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Passover as Hybrid Mourning-and-Survival Rehearsal Ritual
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This story instantiates the hybrid_transformation_reading of the
 *   catastrophe_memory_function kernel: the seder is read as ONE structure
 *   that co-encodes both a D1/D4 mourning-and-boundary function (bitter
 *   herbs, recounting of slavery, retention of group-marking obligation) and
 *   a D5 survival-competence function (the seder's performative structure —
 *   leading, improvising, transmitting practical household-organization skill
 *   under a fixed but adaptable script) in a single ritual event, rather than
 *   as two separable functions that happen to share a calendar date. The
 *   referent for extraction is the standing ritual arrangement as currently
 *   practiced and administered by the rabbinic interpretive tradition,
 *   assessed on this reading's own terms — not the thinner, decomposed
 *   alternative that a purely mourning-only or purely survival-only reading
 *   would endorse.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: primary bearers and beneficiaries of the dual-function transmission (moderate power, identity-locked exit)
 *   - household_ritual_leaders: administer both halves of the hybrid function at the point of practice (moderate power, agenda_setter)
 *   - rabbinic_interpretive_tradition: fixes the legitimate range of the ritual text and benefits from the coherence claim that both functions co-occur (institutional power)
 *   - younger_generation_participants: receive the hybrid transmission without full choice (powerless, constrained exit)
 *   - assimilation_pressured_members: excluded voice who might prefer a thinner, decomposed ritual
 *   - comparative_ritual_scholars: analytical observers testing whether the hybrid claim is structurally accurate or an artifact of after-the-fact reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.32).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Passover as Hybrid Mourning-and-Survival Rehearsal Ritual").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '09e484fd-aef4-40c7-93a8-dd31697b6eb0').
narrative_ontology:cs_kernel_codification('09e484fd-aef4-40c7-93a8-dd31697b6eb0', fixed_text).
narrative_ontology:cs_authority_grounding('09e484fd-aef4-40c7-93a8-dd31697b6eb0', lineage).
narrative_ontology:cs_interpretation_layer_present('09e484fd-aef4-40c7-93a8-dd31697b6eb0').
narrative_ontology:cs_reading_relation('09e484fd-aef4-40c7-93a8-dd31697b6eb0', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('09e484fd-aef4-40c7-93a8-dd31697b6eb0', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('09e484fd-aef4-40c7-93a8-dd31697b6eb0', foundational, functions_are_structurally_co_dependent).
narrative_ontology:cs_axiom_status(functions_are_structurally_co_dependent, holdable).
narrative_ontology:cs_axiom_grounding('09e484fd-aef4-40c7-93a8-dd31697b6eb0', functions_are_structurally_co_dependent, conventional).
narrative_ontology:cs_axiom('09e484fd-aef4-40c7-93a8-dd31697b6eb0', secondary, single_ritual_occasion_binds_dual_transmission).
narrative_ontology:cs_axiom_status(single_ritual_occasion_binds_dual_transmission, holdable).
narrative_ontology:cs_axiom_grounding('09e484fd-aef4-40c7-93a8-dd31697b6eb0', single_ritual_occasion_binds_dual_transmission, instrumental).
narrative_ontology:cs_reference_frame('09e484fd-aef4-40c7-93a8-dd31697b6eb0', post_temple_diaspora_reconstitution).
narrative_ontology:cs_drift_state('09e484fd-aef4-40c7-93a8-dd31697b6eb0', contemporary_stable_diaspora, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('09e484fd-aef4-40c7-93a8-dd31697b6eb0', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, household_ritual_leaders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_interpretive_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, younger_generation_participants).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, younger_generation_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, ritual_dual_function_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform the seder annually across dispersed and often precarious settings. The ritual holds mourning content (bitter herbs, recounting of slavery and loss) alongside a rehearsed sequence of household-scale self-organization (leading a meal, transmitting law, improvising under constraint) that has repeatedly served as functional practice for maintaining community cohesion without centralized institutions during expulsion, hiding, or dispersion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_communities, beneficiary,
    moderate, civilizational, identity_locked, global).

% Run the seder itself — set the order, assign roles, adapt the fixed liturgical script to their household's circumstances. They administer both halves of the hybrid function: they are custodians of the mourning content and are also the ones who must improvise the survival-relevant coordination (feeding, protecting, transmitting law) under whatever conditions the household actually faces that year.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, household_ritual_leaders, agenda_setter,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, household_ritual_leaders, beneficiary).

% Produces and maintains the Haggadah text and its permitted variations, adjudicating which readings of the ritual are legitimate. Benefits from the hybrid reading's coherence: it lets the tradition claim the ritual serves both sacred memorial function and practical communal resilience, which broadens its authority base across both liturgical and communal-survival domains.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_interpretive_tradition, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_interpretive_tradition, beneficiary).

% Receive the transmitted content largely without choosing it — asking the Four Questions, learning the narrative, absorbing both the mourning register and the practical rehearsal of household leadership. They pay in the sense of ritual obligation and time, but structurally acquire competence (organizational, narrative, leadership) they did not seek out independently.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, younger_generation_participants, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, younger_generation_participants, payer).

% Members drifting from or ambivalent about observance experience the ritual's dual claim (sacred memorial AND practical survival training) as a heavier obligation than either function alone would impose — if they wanted only the memorial or only the practical content, no thinner version is legitimated. They are rarely consulted on whether the hybrid framing itself should be preserved.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, assimilation_pressured_members, excluded,
    powerless, biographical, constrained, local).

% Study the seder as a case of dual-encoded ritual, comparing it to other catastrophe-commemoration structures to test whether the D1/D4 (mourning/boundary) and D5 (survival-competence) functions are genuinely co-present in one structure or are separable readings imposed after the fact by different observers.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__hybrid_transformation_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__hybrid_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves two coordination problems inside one recurring structure: it keeps a dispersed group's collective memory of catastrophe alive across generations (so the loss is not forgotten and boundary-norms are reaffirmed) AND it rehearses, at fixed intervals, the practical skills of decentralized household-level organization, leadership improvisation, and law-transmission that historically mattered for group survival under conditions of dispersion or crisis.
% TRANSFER_FUNCTION: Moves narrative content, liturgical competence, and organizational practice from elders and textual authorities to younger participants and newer households; moves interpretive authority over what counts as a legitimate seder from the household level upward to the rabbinic tradition that fixes the Haggadah's permitted range.
% ABSENT_VOICES: Members drifting from observance who might prefer a thinner ritual (memorial only, or practical-skill-transmission only, stripped of religious framing) are rarely in the room when the hybrid reading is reaffirmed as the authoritative account of what the ritual 'really' does; comparative-religion perspectives that would decompose the ritual into separable, non-necessarily-co-occurring functions are outside the tradition's own self-description.
% DISAPPEARANCE_RATIONALE: If the hybrid seder structure vanished, the community would lose a single recurring occasion that does both jobs at once — mourning-memory transmission would have to find another vehicle, and the tacit rehearsal of household-scale organizational competence (which has mattered during real historical dispersions) would have to be re-invented elsewhere or would atrophy; the two functions do not have an obvious independent substitute that reliably co-occurs.
% FOUNDING_PROBLEM: A dispersed and periodically persecuted community needed a repeatable structure that would (a) keep the memory of a formative catastrophe from fading and (b) keep transmitting the practical competence to reconstitute community life quickly and locally without dependence on a central temple or state, since both had been or could be lost.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the post-Temple diaspora and scholars of ritual studies outside the rabbinic tradition itself (e.g., comparative work on catastrophe ritualization in other diaspora and refugee communities) corroborate that decentralized household-based ritual structures functioned as organizational rehearsal during periods when centralized institutions were unavailable; the rabbinic tradition itself, as a benefiting party, asserts the founding problem (memory-plus-competence) remains fully live, which is exactly the corroboration this reading needs from outside the interpretive authority that also administers the text.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) and rises only mildly across the measured interval: the ritual imposes real obligation-cost (time, liturgical labor, generational pressure to conform) but the overwhelming character of the arrangement is coordination rather than extraction — there is no identifiable party that captures rents from imposing this specific dual-reading on others. Suppression (0.32) reflects the real but moderate social pressure toward participation and against thinner, decomposed alternatives, not coercive enforcement. Accessibility collapse (0.4) is moderate: alternative, decomposed readings of the ritual (memorial-only, or practical-only) are conceivable and practiced in some liberal contexts, so alternatives are not fully foreclosed the way a natural law would be. Resistance (0.3) reflects real but limited pushback from assimilation-pressured members and some liberal-reform currents that favor a thinner ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (diaspora communities, household leaders, rabbinic tradition) sit near the coordination end of directionality: they receive the transmitted competence and the memorial function as a package, and the household leaders and rabbinic tradition additionally administer the arrangement. Younger participants are dual-positioned — beneficiaries of transmitted competence, minor payers of ritual obligation, but with constrained rather than trapped exit given the plurality of observance levels available in most diaspora contexts today. No agent is authored as a pure victim: this reading claims the coordination function (both halves) is genuine, which is why victims[] is empty and requires_active_enforcement is false — this is deliberately NOT a tangled_rope reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe memory + decentralized survival competence) remains contested rather than clearly live or clearly dead: for communities still experiencing dispersion pressure or precarity, the survival-competence half plausibly remains functionally live; for well-established, stable diaspora communities, the survival-rehearsal function may have become vestigial while the memorial function persists on its own terms. The hybrid reading resists mislabeling the ritual as pure extraction (since there is no capturing party) while also resisting a naive claim that the arrangement is costless coordination — the moderate suppression and accessibility-collapse figures register the real, if soft, pressure toward conformity that any long-running commemorative practice exerts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_vs_decomposed_reading,
    'Is the seder genuinely ONE structure that co-encodes mourning and survival-competence functions inseparably, or are these two functions that merely co-occur on the same calendar occasion and could be analytically and practically decomposed (as the mourning_practice_reading and survival_competence_reading each claim)?',
    'Comparative ritual-studies analysis of communities that have historically stripped one function while retaining the other (e.g., secular Passover seders retaining organizational/narrative structure without theological mourning content, or purely liturgical observance stripped of household-leadership rehearsal) — if both stripped versions remain functionally coherent and stable over generations, decomposition is supported; if stripped versions consistently degrade or get re-supplemented with the missing function, the hybrid claim is supported.',
    'If decomposition is supported, this hybrid_transformation_reading constraint should be understood as an artifact of a particular interpretive lens rather than a structurally distinct claim from its two siblings, and network analysis should treat all three readings as more tightly coupled than independent. If the hybrid claim holds, the three readings remain genuinely distinct constraints with different beneficiary structures and different disappearance consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_decomposed_reading, conceptual, 'Whether mourning and survival-competence functions are structurally fused or merely co-occurring in the ritual.').

omega_variable(
    founding_problem_liveness_variance,
    'Does the survival-competence half of the founding problem remain live for all diaspora communities equally, or has it become vestigial for stable, institutionally secure communities while remaining functionally active for communities under ongoing dispersion or precarity pressure?',
    'Cross-community comparison of ritual practice intensity and content correlated with measures of institutional security/precarity across different diaspora populations over the same historical interval.',
    'If liveness varies by community security, the hybrid reading''s claimed_type and extraction profile may need to be community-indexed rather than treated as a single global constraint — this would motivate further decomposition beyond the current three-reading kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_liveness_variance, empirical, 'Whether the survival-competence function''s liveness varies systematically with community precarity.').

omega_variable(
    rabbinic_authority_beneficiary_status,
    'Does the rabbinic interpretive tradition''s benefit from the hybrid reading''s broader authority claim (covering both sacred and practical domains) constitute genuine coordination benefit, or does it edge toward institutional rent-seeking — expanding the tradition''s jurisdictional reach by claiming credit for a practical-competence function it did not design and does not administer directly?',
    'Historical analysis of whether rabbinic authorities actively shaped the seder''s organizational-rehearsal content (supporting genuine administrative claim) or whether that content emerged from household practice independent of rabbinic design and was only retrospectively folded into official interpretation (supporting a thinner, more honorific benefit claim).',
    'If the survival-competence content emerged independently of rabbinic design, the rabbinic tradition''s beneficiary status should be reduced in weight relative to household_ritual_leaders, and the constraint''s classification would drift toward a more purely emergent-rope reading with weaker institutional agenda-setting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_beneficiary_status, empirical, 'Whether rabbinic authority''s benefit from claiming the dual function reflects genuine administration or retrospective jurisdictional expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 60, 0.26).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 80, 0.27).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__hybrid_transformation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_memory_function kernel, all describing the same Passover seder ritual but authoring different beneficiary structures and different claimed types depending on which function(s) each reading treats as structurally real. hybrid_transformation_reading claims both D1/D4 (mourning) and D5 (survival-competence) functions are co-present and mutually constitutive within one structure; mourning_practice_reading claims only the D1/D4 function is structurally real (survival-rehearsal content is incidental or overread); survival_competence_reading claims only the D5 function is structurally real (mourning content is incidental scaffolding around the practical rehearsal). Each story authors its own epsilon per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
