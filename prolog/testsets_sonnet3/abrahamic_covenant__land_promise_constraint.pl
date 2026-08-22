% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Territorial Grant Reading of the Abrahamic Covenant (Land of Canaan)
 *   domain: religious/political
 *
 * SUMMARY:
 *   This story isolates one specific claim conflated under 'the Genesis
 *   covenant': that the covenant text includes a durable territorial grant to
 *   a specific land (Canaan), and that this grant is fulfilled or ongoing
 *   rather than merely historical or conditional. This is structurally
 *   distinct from the lineage-transmission question (who legitimately
 *   inherits the covenant — Isaac's line or Ishmael's line, treated in
 *   sibling stories) even though all three readings draw on the same Genesis
 *   kernel. The territorial-grant reading, once picked up by a state actor as
 *   a legitimating argument for sovereignty and settlement, produces a
 *   materially different structure than either lineage reading alone: it has
 *   an identifiable extraction mechanism (land, residency, movement) and an
 *   identifiable victim class (displaced and marginalized communities without
 *   covenantal standing in the discourse), which the lineage-only readings do
 *   not have on their own.
 *
 * KEY AGENTS:
 *   - state_actors_claiming_covenantal_title: primary beneficiary (institutional/arbitrage) — leverages the reading for territorial legitimacy
 *   - displaced_palestinian_communities: primary target (powerless/trapped) — bears the material extraction
 *   - religious_authorities_certifying_the_reading: co-agenda-setter (organized/identity_locked) — supplies doctrinal cover
 *   - conditional_reading_theologians: excluded voice (moderate/constrained) — holds a textually grounded but politically marginal counter-reading
 *   - international_legal_bodies: analytical observer (institutional/analytical) — adjudicates by instruments that do not recognize the covenant as valid title
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.81).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.72).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.81).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Territorial Grant Reading of the Abrahamic Covenant (Land of Canaan)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/political").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '13792c94-02fb-4c5b-8e50-21b298234ad1').
narrative_ontology:cs_kernel_codification('13792c94-02fb-4c5b-8e50-21b298234ad1', fixed_text).
narrative_ontology:cs_authority_grounding('13792c94-02fb-4c5b-8e50-21b298234ad1', lineage).
narrative_ontology:cs_interpretation_layer_present('13792c94-02fb-4c5b-8e50-21b298234ad1').
narrative_ontology:cs_reading_relation('13792c94-02fb-4c5b-8e50-21b298234ad1', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('13792c94-02fb-4c5b-8e50-21b298234ad1', abrahamic_covenant__ishmael_covenant_reading, influences).
narrative_ontology:cs_axiom('13792c94-02fb-4c5b-8e50-21b298234ad1', foundational, territorial_grant_is_covenantally_conditional).
narrative_ontology:cs_axiom_status(territorial_grant_is_covenantally_conditional, holdable).
narrative_ontology:cs_axiom_grounding('13792c94-02fb-4c5b-8e50-21b298234ad1', territorial_grant_is_covenantally_conditional, deontological).
narrative_ontology:cs_axiom('13792c94-02fb-4c5b-8e50-21b298234ad1', secondary, sacred_text_does_not_ground_modern_sovereign_title).
narrative_ontology:cs_axiom_status(sacred_text_does_not_ground_modern_sovereign_title, holdable).
narrative_ontology:cs_axiom_grounding('13792c94-02fb-4c5b-8e50-21b298234ad1', sacred_text_does_not_ground_modern_sovereign_title, conventional).
narrative_ontology:cs_reference_frame('13792c94-02fb-4c5b-8e50-21b298234ad1', covenantal_land_grant_as_conditional_promise).
narrative_ontology:cs_drift_state('13792c94-02fb-4c5b-8e50-21b298234ad1', contemporary_state_sovereignty_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('13792c94-02fb-4c5b-8e50-21b298234ad1', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, state_actors_claiming_covenantal_title).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_palestinian_communities).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, non_state_claimants_without_covenant_leverage).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, divine_territorial_grant_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes the territorial-grant reading of the covenant text as legitimating grounds for settlement, annexation, and border policy, layering it alongside international-law and security arguments. Controls the apparatus (military, legal, administrative) that converts the textual claim into facts on the ground, and can select among conditional/fulfilled/ongoing readings depending on the policy need of the moment.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, state_actors_claiming_covenantal_title, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, state_actors_claiming_covenantal_title, agenda_setter).

% Bears the material consequence of the territorial-grant reading being operationalized: loss of land, restricted movement, and contested residency status. Has no standing within the covenant's own textual authority structure to contest the reading, since the text's beneficiaries are defined by lineage the community does not claim, and exit from the affected territory is frequently not available.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_palestinian_communities, payer,
    powerless, generational, trapped, regional).

% Includes smaller religious and ethnic communities within the contested territory whose land and residency claims carry no covenantal argument to counter the dominant reading; their claims are litigated purely on secular or customary grounds while the opposing claim can additionally invoke sacred title.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, non_state_claimants_without_covenant_leverage, payer,
    powerless, biographical, constrained, regional).

% Rabbinic, and in parallel Christian Zionist, institutions produce and circulate doctrinal support for the territorial-grant reading as fulfilled-or-ongoing rather than conditional or superseded, reinforcing the state actor's legitimating claim. Their institutional identity and theological tradition are substantially built around this reading, making its abandonment costly to the institution's own self-understanding, not merely to policy.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_authorities_certifying_the_reading, agenda_setter,
    organized, civilizational, identity_locked, global).

% Jewish, Christian, and Muslim scholars who hold that the territorial grant in the text is explicitly conditioned on covenantal fidelity (obedience, justice, treatment of the stranger) and therefore cannot be read as an unconditional, self-executing land title. Their reading is marginal in the political discourse that actually adjudicates territorial claims, even though it is textually well-attested and widely held within religious scholarship.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, conditional_reading_theologians, excluded,
    moderate, generational, constrained, global).

% Adjudicate territorial disputes using instruments (UN resolutions, occupation law, self-determination doctrine) that formally do not recognize sacred-text title as a valid basis for sovereignty claims, yet must operate in a political environment where the covenant reading functions as a powerful domestic and diasporic legitimating narrative regardless of its non-recognition in international law.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, state_actors_claiming_covenantal_title).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At its narrowest, the covenant narrative coordinates a diasporic and religious community's sense of continuity, historical memory, and claim to a homeland — a genuine identity and belonging function independent of any specific border.
% TRANSFER_FUNCTION: The territorial-grant reading, once operationalized by a state actor, moves land, residency rights, and freedom of movement away from communities without covenantal standing in the discourse and toward the state actor and settler populations who can invoke the reading as legitimating title.
% ABSENT_VOICES: Conditional-reading theologians within the tradition itself, and Palestinian religious and secular voices who reject the premise that sacred text can adjudicate modern sovereignty, are structurally absent from the political and legal forums where the territorial-grant reading is actually deployed as an argument.
% DISAPPEARANCE_RATIONALE: If the territorial-grant reading vanished as a legitimating narrative overnight, the underlying geopolitical dispute over land and security would not disappear — it long predates and exceeds the theological argument — but a specific, powerful strand of domestic and diasporic legitimation would lose force, altering the political coalition and rhetoric supporting continued settlement expansion. Whether 'the world rearranges' or 'the world stays the same' therefore depends on which layer of the conflict is being asked about; the parties dispute which layer is doing the real work.
% FOUNDING_PROBLEM: The narrative was originally transmitted to explain a specific ancient people's claim to a specific ancient land as part of a covenantal relationship with a deity, embedded in a text about faithfulness, exile, and return, not as a legal instrument for adjudicating twentieth- and twenty-first-century sovereign borders.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical biblical scholars and conditional-reading theologians, working outside the political coalitions that benefit from the territorial-grant reading, attest that the text's own internal logic ties the land grant to covenantal fidelity and treats exile as a live covenantal consequence — i.e., the founding problem was conditional and dynamic, not a fixed unconditional deed. State actors and allied religious authorities who benefit from the reading attest instead that the promise is either already fulfilled or presently ongoing and unconditional; no source outside these benefiting parties corroborates the unconditional-ongoing reading as the text's original sense.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, contested).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.42 to 0.81 across the interval) because the territorial-grant reading has been increasingly operationalized into concrete land and residency outcomes rather than remaining a purely devotional or historical claim. Suppression is substantial (0.72) because the reading's persistence depends on excluding the well-attested conditional reading from the forums that actually decide sovereignty, and on state enforcement machinery (settlement policy, administrative control) rather than on textual consensus. Theater ratio is moderate (0.38): a real coordination/identity function (diasporic continuity, historical memory) persists underneath the extraction, so this is not pure theater, but a rising share of the doctrinal apparatus function is legitimating rather than devotional.
 *
 * DIRECTIONALITY LOGIC:
 *   The state actor is the structural beneficiary: it collects the legitimating value of the reading and controls the apparatus that converts it into territorial fact, so directionality sits near the full-beneficiary end. Displaced communities and non-state claimants sit near the full-target end — they bear the transfer, have no covenantal standing to contest it in the terms the discourse uses, and often cannot exit the affected territory. Religious authorities occupy a beneficiary/agenda-setter hybrid: they do not receive land directly but their institutional legitimacy is bound up with certifying the reading, which is why an identity-lock exit option is more accurate for them than a simple beneficiary classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a covenantal people's claim to a land, textually conditioned on fidelity and framed around exile-and-return) is contested as to whether it is live, dead, or merely repurposed. Classifying this as a snare rather than dismissing the entire covenant tradition as extraction prevents overreach: the underlying identity/coordination function (diasporic memory, religious meaning) is real and is not what is being condemned here. What is classified as extractive is specifically the operationalization of one contested reading of a conditional promise into an unconditional territorial title enforced against powerless non-covenantal claimants — a much narrower and more falsifiable claim than 'religion is extraction.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditional_vs_unconditional_land_grant,
    'Does the Genesis text itself present the territorial grant as unconditional and self-executing, or as conditioned on covenantal fidelity and therefore capable of lapsing (as exile narratives within the same textual tradition suggest)?',
    'Comparative textual-critical analysis of the covenant renewal passages (Genesis 15, 17, 22) against the exile/curse passages (Deuteronomy 28, Leviticus 26) that explicitly tie land retention to obedience, conducted by scholars outside the political coalitions that benefit from either reading.',
    'If the text supports only a conditional reading, the unconditional-ongoing reading used to legitimate modern territorial claims is a constructed extension beyond the kernel''s own terms, strengthening the snare classification. If the text genuinely supports an unconditional reading, the beneficiary reading has stronger internal textual warrant, though this would not resolve the separate question of whether ancient covenantal title has any standing in modern international law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_vs_unconditional_land_grant, conceptual, 'Whether the land promise is textually conditional or unconditional.').

omega_variable(
    sacred_title_vs_modern_sovereignty,
    'Can a claim grounded in an ancient covenantal text function as valid title for modern state sovereignty at all, independent of how the covenant itself is read?',
    'Comparative analysis of how international law and comparative constitutional theory treat sacred-text-based territorial claims generally (e.g., other religio-territorial disputes), assessed by legal scholars outside the specific conflict.',
    'If sacred title categorically cannot ground modern sovereignty claims, the entire beneficiary structure of this constraint rests on a category error regardless of which internal reading (conditional/unconditional/fulfilled) is correct — strengthening the case that the reading functions purely as legitimating cover for a claim actually grounded in force and settlement. If sacred title can carry legal or quasi-legal weight in some contexts, the constraint''s extractive character depends more heavily on the internal-reading question above.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacred_title_vs_modern_sovereignty, conceptual, 'Whether covenantal text can ground modern territorial sovereignty in principle.').

omega_variable(
    committer_structure_reading_divergence,
    'This constraint is one reading (land_promise_constraint) of the abrahamic_covenant kernel; the sibling readings (isaac_covenant_reading, ishmael_covenant_reading) concern lineage-transmission rather than territory. Where exactly does the political deployment of this constraint borrow legitimacy from the lineage readings, and would foreclosing the Isaac-line reading (e.g., an inclusive-covenant finding) structurally weaken the territorial claim examined here, or leave it fully intact?',
    'Trace specific political and legal arguments (settlement policy statements, religious-nationalist doctrine) to determine whether they cite lineage exclusivity as a necessary premise for territorial title, or treat the territorial grant as free-standing regardless of lineage outcome.',
    'If the territorial claim is logically independent of the lineage question, this story''s classification is robust to how the sibling kernel readings resolve. If the territorial claim actually depends on the Isaac-line exclusivity reading as a hidden premise, the two constraints are more tightly coupled than the ε-invariance decomposition assumes, and the network edge should carry stronger weight in downstream analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_reading_divergence, conceptual, 'Structural dependency between the territorial reading and the sibling lineage readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__land_promise_constraint, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(abra_tr_t0, observed).
narrative_ontology:measurement(abra_tr_t15, abrahamic_covenant__land_promise_constraint, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(abra_tr_t15, observed).
narrative_ontology:measurement(abra_tr_t30, abrahamic_covenant__land_promise_constraint, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(abra_tr_t30, observed).
narrative_ontology:measurement(abra_tr_t45, abrahamic_covenant__land_promise_constraint, theater_ratio, 45, 0.33).
narrative_ontology:measurement_basis(abra_tr_t45, observed).
narrative_ontology:measurement(abra_tr_t60, abrahamic_covenant__land_promise_constraint, theater_ratio, 60, 0.36).
narrative_ontology:measurement_basis(abra_tr_t60, observed).
narrative_ontology:measurement(abra_tr_t75, abrahamic_covenant__land_promise_constraint, theater_ratio, 75, 0.38).
narrative_ontology:measurement_basis(abra_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__land_promise_constraint, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(abra_be_t0, observed).
narrative_ontology:measurement(abra_be_t15, abrahamic_covenant__land_promise_constraint, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(abra_be_t15, observed).
narrative_ontology:measurement(abra_be_t30, abrahamic_covenant__land_promise_constraint, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(abra_be_t30, observed).
narrative_ontology:measurement(abra_be_t45, abrahamic_covenant__land_promise_constraint, base_extractiveness, 45, 0.74).
narrative_ontology:measurement_basis(abra_be_t45, observed).
narrative_ontology:measurement(abra_be_t60, abrahamic_covenant__land_promise_constraint, base_extractiveness, 60, 0.78).
narrative_ontology:measurement_basis(abra_be_t60, observed).
narrative_ontology:measurement(abra_be_t75, abrahamic_covenant__land_promise_constraint, base_extractiveness, 75, 0.81).
narrative_ontology:measurement_basis(abra_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__land_promise_constraint, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(abra_su_t0, observed).
narrative_ontology:measurement(abra_su_t15, abrahamic_covenant__land_promise_constraint, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(abra_su_t15, observed).
narrative_ontology:measurement(abra_su_t30, abrahamic_covenant__land_promise_constraint, suppression_requirement, 30, 0.63).
narrative_ontology:measurement_basis(abra_su_t30, observed).
narrative_ontology:measurement(abra_su_t45, abrahamic_covenant__land_promise_constraint, suppression_requirement, 45, 0.67).
narrative_ontology:measurement_basis(abra_su_t45, observed).
narrative_ontology:measurement(abra_su_t60, abrahamic_covenant__land_promise_constraint, suppression_requirement, 60, 0.7).
narrative_ontology:measurement_basis(abra_su_t60, observed).
narrative_ontology:measurement(abra_su_t75, abrahamic_covenant__land_promise_constraint, suppression_requirement, 75, 0.72).
narrative_ontology:measurement_basis(abra_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is the territorial-grant reading within the three-member abrahamic_covenant kernel family. isaac_covenant_reading and ishmael_covenant_reading concern who legitimately inherits the covenant (lineage-transmission); this story concerns what the covenant grants (land) and whether that grant is conditional, fulfilled, or ongoing. The three stories share a kernel but have structurally distinct claims, distinct beneficiary/victim sets, and distinct epsilon values — this story's epsilon (0.81) reflects a materially operationalized, high-suppression extraction structure with identifiable victims, which the lineage-only readings do not independently carry. Political deployments frequently bundle the territorial claim with the Isaac-line lineage claim rhetorically; the network edge records this coupling without merging the constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
