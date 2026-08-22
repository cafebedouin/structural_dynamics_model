% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Performance-Only Reading of the Sacrifice Obligation (Study as Preparation, Not Fulfillment)
 *   domain: religious law / ritual studies / textual tradition
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), the sacrificial
 *   commandments lost their performance site, and the tradition fragmented
 *   into rival answers to a single question: what do these commandments
 *   demand of the generations who cannot perform them? This file instantiates
 *   the strictest answer — the performance_only reading: the obligation
 *   remains binding now, physical performance is the only act that satisfies
 *   it, and study of the sacrificial laws is preparation for a future
 *   restoration, not fulfillment. Under this reading the current generation
 *   of obligation-bearers carries a live commandment it cannot discharge:
 *   every year without a Temple accrues deficiency that no available act
 *   removes, and the reading explicitly prices study — the one available
 *   engagement — as non-satisfaction. The rival answers
 *   (study-as-fulfillment, messianic suspension, archival dissolution) are
 *   separate constraint stories linked through the network block. The epsilon
 *   referent is the standing arrangement under contest: the binding
 *   obligation on the current generation as this reading itself frames it,
 *   assessed by the reading's own lights — never the rival arrangements the
 *   sibling readings would put in its place. The claimed type and the metrics
 *   are authored independently: the claim is tangled_rope (a genuine
 *   coordination function plus asymmetric extraction); the metrics describe
 *   the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - restoration_movement_institutions: agenda-setting beneficiary (organized / identity_locked) — administers the reading's practical machinery and collects purpose, funding, and institutional warrant from the obligation's live status
 *   - strict_reading_rabbinic_authorities: agenda-setting beneficiary with a payer burden (institutional / identity_locked) — rules the obligation live and personally bears the same undischarged duty
 *   - current_generation_obligation_bearers: primary target (moderate / constrained) — carries the unfulfillable obligation; the reading's guilt falls on them without remedy
 *   - priestly_lineage_families: secondary beneficiary (moderate / constrained) — lineage salience and forward-looking role sustained by the obligation remaining live
 *   - study_as_performance_communities: excluded rival (institutional / mobile) — holds the sibling answer that study itself fulfills; outside the strict communities' conversation by the boundary this reading polices
 *   - comparative_ritual_scholars: analytical observer (analytical / analytical) — sees the full structure: the founding crisis, the continuity function, the guilt mechanism, and the rival readings as alternative solutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.78).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.62).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Performance-Only Reading of the Sacrifice Obligation (Study as Preparation, Not Fulfillment)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious law / ritual studies / textual tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '3a294878-ce25-440a-a7a0-e02f23544c7a').
narrative_ontology:cs_kernel_codification('3a294878-ce25-440a-a7a0-e02f23544c7a', fixed_text).
narrative_ontology:cs_authority_grounding('3a294878-ce25-440a-a7a0-e02f23544c7a', lineage).
narrative_ontology:cs_interpretation_layer_present('3a294878-ce25-440a-a7a0-e02f23544c7a').
narrative_ontology:cs_reading_relation('3a294878-ce25-440a-a7a0-e02f23544c7a', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('3a294878-ce25-440a-a7a0-e02f23544c7a', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('3a294878-ce25-440a-a7a0-e02f23544c7a', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('3a294878-ce25-440a-a7a0-e02f23544c7a', foundational, physical_performance_required_for_satisfaction).
narrative_ontology:cs_axiom_status(physical_performance_required_for_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('3a294878-ce25-440a-a7a0-e02f23544c7a', physical_performance_required_for_satisfaction, theological).
narrative_ontology:cs_axiom('3a294878-ce25-440a-a7a0-e02f23544c7a', foundational, obligation_binding_without_temple).
narrative_ontology:cs_axiom_status(obligation_binding_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('3a294878-ce25-440a-a7a0-e02f23544c7a', obligation_binding_without_temple, theological).
narrative_ontology:cs_reference_frame('3a294878-ce25-440a-a7a0-e02f23544c7a', eternal_binding_sacrificial_order).
narrative_ontology:cs_drift_state('3a294878-ce25-440a-a7a0-e02f23544c7a', post_destruction_interim, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3a294878-ce25-440a-a7a0-e02f23544c7a', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, restoration_movement_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, strict_reading_rabbinic_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, priestly_lineage_families).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_obligation_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, strict_reading_rabbinic_authorities).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, eternal_validity_of_sacrificial_commandments).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, future_temple_restoration).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, study_as_preparation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the practical machinery of the reading: train candidates for priestly service, fabricate vessels and vestments to textual specification, publish curricula teaching that the sacrificial obligation is live and undischarged, and raise support for rebuilding. Their institutional purpose exists only because the obligation is framed as binding and unperformable; under a rival reading their warrant dissolves. Exit would mean unmaking the institutions' reason for being.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, restoration_movement_institutions, agenda_setter,
    organized, generational, identity_locked, global).

% Rule that the sacrificial commandments remain binding and that study does not discharge them; their rulings sustain the reading's normative force and police its boundary against softer answers. They personally bear the same undischarged duty they administer: each ruling that keeps the obligation live deepens their own unremedied deficiency. Their standing in the strict communities rests on holding the demanding reading; softening it would dissolve the authority the demanding reading sustains.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, strict_reading_rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, strict_reading_rabbinic_authorities, payer).

% Observant Jews in communities that hold the reading. They carry a live commandment they cannot perform, renewed daily by liturgical recitation of the sacrificial order, with no act available to discharge it; the reading prices their situation as deficiency rather than exemption. Exit means joining communities that hold a softer answer — available in principle, costly in community, family, and self-concept — or leaving observance altogether.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_obligation_bearers, payer,
    moderate, biographical, constrained, global).

% Families of priestly descent whose lineage retains live salience only while the sacrificial obligation stands undischarged: the reading preserves their future role, sustains lineage consciousness and the marital restrictions that mark the line, and positions them for service upon restoration. If the obligation were read as dissolved, their distinguishing status would lose its forward-looking ground.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, priestly_lineage_families, beneficiary,
    moderate, generational, constrained, global).

% The mainstream communities holding the sibling answer that study of the sacrificial laws is itself fulfillment. They would tell the strict communities that the tradition already supplied satisfaction and that the strict reading manufactures undischarged guilt where none is entailed. They are outside the strict communities' conversation by the boundary the reading polices; they exited the arrangement by adopting a different answer to the same question and bear none of its burden.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, study_as_performance_communities, excluded,
    institutional, generational, mobile, global).

% Study how textual traditions keep unperformable rites normatively alive across institutional collapse, comparing the sacrificial-law case with parallel cases of suspended ritual obligation. They see the whole structure at once: the founding crisis, the continuity function, the guilt mechanism, and the rival readings as alternative solutions. They collect nothing and owe nothing under the arrangement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, restoration_movement_institutions).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the sacrificial commandments' binding continuity across the Temple-less interval: keeps the law corpus normatively live rather than archival, coordinates collective preparation (priestly training, vessel fabrication, liturgical rehearsal of the order) toward a future resumption, and holds the community's commitment structure intact so that a rebuilt cult would resume into an unbroken obligation.
% TRANSFER_FUNCTION: Moves an undischargeable compliance-debt onto the current generation — each year without performance accrues deficiency that no available act discharges — and moves legitimacy, funding, institutional purpose, and forward-looking status to the restoration institutions, the ruling class, and the priestly lineages; readiness (trained candidates, fabricated vessels) accumulates for the future cult.
% ABSENT_VOICES: The study-as-fulfillment mainstream is the structurally absent voice: it holds a rival answer to the same question and would testify that the tradition already supplied satisfaction, making the strict reading's guilt manufactured rather than entailed. Within the strict communities themselves, members who experience the unremediability of their situation have no sanctioned voice — framing it as a problem of the reading rather than of the exile is itself a breach of the boundary the reading polices.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the communities holding it would rearrange around one of the sibling answers: the restoration institutions would lose their normative warrant (preparation for an obligation nobody owes), the guilt burden on the current generation would lift or convert into study-as-satisfaction, priestly lineage salience would fade, and the strict communal boundary would dissolve into the surrounding mainstream. The arrangement — not the underlying texts — is what holds these positions apart.
% FOUNDING_PROBLEM: The destruction of the Temple created a crisis of continuity: the sacrificial commandments presuppose a standing cult, and a binding commandment that cannot be performed threatens the tradition's claim that its law is eternally valid and fully livable. This arrangement was built to keep the obligation's binding force intact across the interval of destruction — neither fulfilled, nor suspended, nor dissolved — so that restoration would resume the demand rather than recreate it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's historical reality is corroborated from outside the benefiting parties: academic historians of Second Temple Judaism and the rival-reading communities alike attest that 70 CE produced a genuine crisis of an unperformable sacrificial law, and the very existence of the three sibling readings attests that the problem demanded an answer. But no source outside the strict communities attests that the problem remains live in this reading's form — the sibling readings attest precisely that they regard better answers as available. The live status, as this reading frames it, is attested only by the parties the reading sustains; that asymmetry is itself signal and is carried in the kernel omega.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the arrangement converts ordinary time into accruing deficiency: the obligation is live, performance is impossible, and the reading explicitly denies satisfaction to the one available act. There is no discharge mechanism — the burden has no internal remedy, which is what distinguishes this reading's operation from a transitional cost. Suppression (0.62) is the cost of holding the boundary: the reading persists against the mainstream drift toward softer sibling answers through communal education, authority structure, and identity pressure; exits exist (joining a rival-reading community) but are priced in community, family, and self-concept, so the suppression is partly structural and partly internalized (see the omega). Theater ratio (0.32) reflects the preparation activity's ambiguous function: vessel fabrication, vestment-making, and priestly training have no proximate use while the Temple stands unbuilt, and the daily liturgical recitation of the sacrificial order is placeholder activity by the reading's own declaration — it maintains the obligation's salience without satisfying it. Accessibility collapse (0.68): within the reading's framework, once physical performance is fixed as the satisfaction condition, the study-as-satisfaction exit collapses logically; the remaining exit is meta-level (adopting a different reading), which is exactly what the boundary machinery suppresses. Resistance (0.55): the sibling readings are the resistance — the tradition's mainstream moved to softer answers to avoid precisely this outcome, and this reading persists as a strict-minority position maintained against that drift. Boltzmann coordination type is identity_coordination: the function whose failure would break the arrangement is the community's commitment structure — if members stopped holding the obligation as binding, the continuity the reading exists to preserve would lapse. The identity framing is not mere cover — the strict boundary is real — but the guilt mechanism rides on it, which is why the suppression-mechanism omega is load-bearing. The measurement series share one grid (interval 0-60 maps to 1966-2026, the period in which the reading became institutionally embodied): extractiveness climbs as the restoration movement institutionalizes and the denial of study-as-satisfaction sharpens; the suppression requirement climbs because this story specifically tracks enforcement-capacity change — the boundary machinery (curricula, institutions, polemics) matured against proliferating softer readings; theater climbs as preparation activity scales without a proximate function. The trajectories are monotonic rather than cyclical because the burden accrues with elapsed time instead of oscillating.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seats compute differently from the same structure. From the obligation-bearers' position, the arrangement is a demand that converts their piety into perpetual deficiency: they perform everything available (study, liturgy, longing) and the reading rules all of it non-satisfying — from that seat the structure is extraction with no offsetting benefit. From the restoration institutions' position, the same structure is the tradition's lifeline: it is the only frame under which their preparation has meaning and under which a rebuilt cult would resume into an unbroken obligation rather than a revived memory. The rabbinic seat is genuinely split — the authorities administer the demand and personally bear it — and the reading's internal discourse is full of consolation mechanisms (mourning practices, the merit of intention, longing as devotion) that function as partial refunds of the burden their own ruling sustains. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map to directionality as follows. current_generation_obligation_bearers are the declared victims with constrained exit — they sit near the full-target end, and the unfulfillable structure amplifies their effective burden because no act reduces it. restoration_movement_institutions sit near the full-beneficiary end: agenda-setters whose institutional existence is constituted by the reading, with identity-locked exit — the arrangement subsidizes them with purpose, membership, and resources. priestly_lineage_families are beneficiaries with constrained exit — low directionality, modest gains (status and forward-looking role). strict_reading_rabbinic_authorities are the genuinely dual-positioned seat: institutionally they are agenda-setting beneficiaries (the reading sustains their authority), but personally they sit inside the victim class — every ruling they issue deepens their own undischarged deficiency. The structural derivation reads their beneficiary/agenda-setter position; the secondary payer role records the personal burden, and their effective directionality sits between the pure-beneficiary institutions and the pure-target laity, nearer the beneficiary end institutionally. study_as_performance_communities are excluded rather than coordinated: they bear nothing and collect nothing under this arrangement — they exited it by adopting a rival answer to the same question, and their position defines the boundary this reading polices. No directionality overrides are authored: the role and exit declarations carry the signal, and the override mechanism (keyed by power atom) could not separate the two institutional seats with different structural relationships without misapplying to the excluded rival.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping a binding sacrificial obligation intact across the destruction interval so that restoration would resume into continuity rather than revive a memory — is live: the Temple is not rebuilt, and within the reading's own framework the problem it was built to solve persists. This is not a mandate outliving its function, so no mandatrophy resolution is declared. The classification risk runs in both directions: labeling the arrangement pure extraction would erase the genuine coordination function (normative continuity across a two-millennium discontinuity, collective preparation, communal boundary), and labeling it pure coordination would erase the undischarged guilt the current generation carries — a cost with no internal remedy, imposed by a ruling that identifiable authorities could soften at the price of their own position. The tangled_rope claim keeps both faces visible. The arrangement is contingent on the interval it bridges rather than on inertia: if restoration occurred, the founding problem would resolve and the interim structure (preparation-as-substitute, guilt without performance) would dissolve into the performance regime it anticipates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the performance_only reading of the sacrifice_obligation_continuity kernel — is that the correct instantiation, or does one of the sibling readings (study_as_performance, messianic_suspension, archival_preservation) hold the kernel''s true structure?',
    'Internal halakhic analysis of the satisfaction condition (whether any textual or rational ground makes study count as performance) and of the obligation''s status during impossibility (whether binding force survives impossibility as live deficiency or whether impossibility suspends it); the sibling readings are the live rival answers and each is authored as its own constraint story.',
    'If study_as_performance is correct, the victim set empties — no undischarged obligation, no guilt without remedy — and this constraint''s extractiveness collapses toward the coordination floor. If messianic_suspension is correct, the victim set also empties (no violation accrues) and the arrangement becomes pure readiness coordination. If archival_preservation is correct, the constraint dissolves entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Which reading correctly instantiates the sacrifice-obligation kernel; the disagreement is located on the satisfaction condition and on the obligation''s present normative status.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the current generation''s continued bearing of the undischargeable obligation held in place by structural enforcement (communal boundary, education, authority structure) or by internalized identity (the bearer cannot conceive exit without self-loss)?',
    'Post-exit suppression trajectory: track leavers who adopt a sibling-reading community or leave observance — if obligation-salience and guilt persist after exit, the suppression is substantially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — the burden travels with the bearer after exit — and the bearers are effectively more trapped than the constrained exit atom records; if structural, softening communal enforcement would release the burden quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism for the obligation-bearers.').

omega_variable(
    impossibility_exemption_question,
    'Within the reading''s own halakhic framework, does impossibility exempt (no deficiency accrues for what cannot be done) or does the binding obligation survive impossibility as live deficiency? The reading''s extraction intensity hinges on this internal question.',
    'Analysis of the strict communities'' own rulings on impossibility as applied to Temple-dependent commandments: whether the destruction is framed as suspending liability or as a deficiency the community actively mourns and owes.',
    'If impossibility exempts, the guilt-without-remedy structure weakens substantially and extractiveness drops toward a transition-cost reading; if the obligation survives as live deficiency, the extraction is as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impossibility_exemption_question, conceptual, 'Whether the reading''s own framework exempts the impossible performance or accrues it as live deficiency.').

omega_variable(
    restoration_horizon_openness,
    'Is the restoration horizon within this reading open-ended or credibly proximate? An open-ended horizon makes the interim guilt permanent; a credible proximate horizon makes it transitional.',
    'The reading''s own discourse: whether its authorities commit to testable restoration claims or explicitly refuse horizons; movement rhetoric and educational materials across the interval.',
    'A credible proximate horizon would reframe the arrangement toward transitional support (preparation for imminent resumption); an open-ended horizon entrenches the guilt-without-remedy structure and supports the higher-extractiveness reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_horizon_openness, empirical, 'Openness of the restoration horizon and its effect on whether the interim burden is transitional or permanent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_obligation_continuity__performance_only, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(sacr_tr_t12, observed).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_obligation_continuity__performance_only, theater_ratio, 24, 0.23).
narrative_ontology:measurement_basis(sacr_tr_t24, observed).
narrative_ontology:measurement(sacr_tr_t36, sacrifice_obligation_continuity__performance_only, theater_ratio, 36, 0.27).
narrative_ontology:measurement_basis(sacr_tr_t36, observed).
narrative_ontology:measurement(sacr_tr_t48, sacrifice_obligation_continuity__performance_only, theater_ratio, 48, 0.3).
narrative_ontology:measurement_basis(sacr_tr_t48, observed).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__performance_only, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(sacr_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t12, sacrifice_obligation_continuity__performance_only, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(sacr_be_t12, observed).
narrative_ontology:measurement(sacr_be_t24, sacrifice_obligation_continuity__performance_only, base_extractiveness, 24, 0.7).
narrative_ontology:measurement_basis(sacr_be_t24, observed).
narrative_ontology:measurement(sacr_be_t36, sacrifice_obligation_continuity__performance_only, base_extractiveness, 36, 0.73).
narrative_ontology:measurement_basis(sacr_be_t36, observed).
narrative_ontology:measurement(sacr_be_t48, sacrifice_obligation_continuity__performance_only, base_extractiveness, 48, 0.76).
narrative_ontology:measurement_basis(sacr_be_t48, observed).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__performance_only, base_extractiveness, 60, 0.78).
narrative_ontology:measurement_basis(sacr_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t12, sacrifice_obligation_continuity__performance_only, suppression_requirement, 12, 0.49).
narrative_ontology:measurement_basis(sacr_su_t12, observed).
narrative_ontology:measurement(sacr_su_t24, sacrifice_obligation_continuity__performance_only, suppression_requirement, 24, 0.53).
narrative_ontology:measurement_basis(sacr_su_t24, observed).
narrative_ontology:measurement(sacr_su_t36, sacrifice_obligation_continuity__performance_only, suppression_requirement, 36, 0.56).
narrative_ontology:measurement_basis(sacr_su_t36, observed).
narrative_ontology:measurement(sacr_su_t48, sacrifice_obligation_continuity__performance_only, suppression_requirement, 48, 0.59).
narrative_ontology:measurement_basis(sacr_su_t48, observed).
narrative_ontology:measurement(sacr_su_t60, sacrifice_obligation_continuity__performance_only, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(sacr_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, archival_preservation).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrifice obligation after the Temple' covers four structurally distinct claims with different victim sets and different epsilon values. This file instantiates performance_only (obligation binds, performance required, current generation in undischarged deficiency). study_as_performance empties the victim set by redefining satisfaction; messianic_suspension empties it by pausing the obligation; archival_preservation dissolves the constraint entirely. The four stories form one constraint family linked through affects_constraints; performance_only is the strictest frame and the baseline against which the other three define their relaxations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
