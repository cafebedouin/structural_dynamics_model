% ============================================================================
% CONSTRAINT STORY: unaudited_contentment_as_entitlement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unaudited_contentment_as_entitlement, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unaudited_contentment_as_entitlement
 *   human_readable: Unaudited Contentment as Entitlement (Anton/Klára Domestic Ledger)
 *   domain: interpersonal/domestic
 *
 * SUMMARY:
 *   Anton keeps an unspoken internal accounting of the marriage's health,
 *   crediting the arrangement whenever Klára shows no visible sign of
 *   distress. He never audits this belief by asking her directly what the
 *   arrangement costs her interior life; he infers contentment from the
 *   absence of complaint and treats that inference as settled fact, which
 *   exempts him from ever having to investigate further. Klára, meanwhile,
 *   manages her own visible affect partly from exhaustion and partly because
 *   visible distress has historically produced defensiveness rather than
 *   change, so she has learned to keep the ledger looking balanced from
 *   outside. An accident — illness, an overheard remark, some rupture in the
 *   ordinary opacity of the household — eventually forces a fragment of her
 *   actual cost into view, and Anton partially recognizes it, but the
 *   recognition is incomplete: it addresses the visible incident rather than
 *   the standing arrangement that produced years of unexamined cost.
 *
 * KEY AGENTS:
 *   - anton: beneficiary/agenda_setter (moderate/mobile) — collects the benefit of an apparently smooth household while structurally exempting himself from inquiry
 *   - klara_labor: payer (moderate/identity_locked) — bears the suppressed cost, manages her own affect to preserve the appearance the arrangement depends on
 *   - her_sister: excluded (powerless/constrained) — the closest thing to an outside witness, without standing to compel change
 *   - the_accident_of_visibility: observer, non-agent — the narrative event that forces partial, incomplete recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unaudited_contentment_as_entitlement, 0.71).
domain_priors:suppression_score(unaudited_contentment_as_entitlement, 0.62).
domain_priors:theater_ratio(unaudited_contentment_as_entitlement, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unaudited_contentment_as_entitlement, extractiveness, 0.71).
narrative_ontology:constraint_metric(unaudited_contentment_as_entitlement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unaudited_contentment_as_entitlement, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unaudited_contentment_as_entitlement, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unaudited_contentment_as_entitlement, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unaudited_contentment_as_entitlement, snare).
narrative_ontology:human_readable(unaudited_contentment_as_entitlement, "Unaudited Contentment as Entitlement (Anton/Klára Domestic Ledger)").
narrative_ontology:topic_domain(unaudited_contentment_as_entitlement, "interpersonal/domestic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unaudited_contentment_as_entitlement, anton).
narrative_ontology:constraint_victim(unaudited_contentment_as_entitlement, klara_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs an informal internal ledger crediting the marriage as 'fine' whenever Klára does not visibly complain. He infers her interior state entirely from the absence of friction — no crying, no raised voice, meals appear, the household functions — and treats that absence as audited proof the arrangement costs her nothing. He never asks her directly what the arrangement costs; asking would require him to potentially owe something in response, and the silence lets him keep believing the balance is settled. He sets the terms of what counts as evidence and never has to defend that standard because no one demands he defend it.
narrative_ontology:constraint_stakeholder(unaudited_contentment_as_entitlement, anton, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(unaudited_contentment_as_entitlement, anton, agenda_setter).

% Performs the invisible labor of managing the household's emotional and logistical continuity, and manages her own visible affect so as not to disturb the arrangement's apparent smoothness — partly from exhaustion, partly because she has learned that visible distress produces defensiveness rather than change. Her silence is read by Anton as contentment; she experiences it as unlanguaged cost she has stopped expecting anyone to ask about. Exit would require dismantling a shared identity, home, and social world built around the marriage looking fine from outside, which is precisely what the silence has been protecting.
narrative_ontology:constraint_stakeholder(unaudited_contentment_as_entitlement, klara_labor, payer,
    moderate, biographical, identity_locked, local).

% Notices exhaustion in Klára during visits and has said something once, obliquely, but is not inside the marriage and has no standing to compel Anton to inquire. Her observations are the closest thing to an outside audit, but they arrive as gossip-adjacent concern rather than as evidence Anton is structurally obligated to weigh.
narrative_ontology:constraint_stakeholder(unaudited_contentment_as_entitlement, her_sister, excluded,
    powerless, immediate, constrained, local).

% Not an agent but the narrative hinge: a fever, a dropped tray, an overheard call, a diary left open — some accident that forces a fragment of Klára's interior cost briefly into view. It is included for narrative completeness because the six-questions and mandatrophy analysis depend on distinguishing partial forced recognition from a genuine audit.
narrative_ontology:constraint_stakeholder(unaudited_contentment_as_entitlement, the_accident_of_visibility, observer,
    analytical, immediate, analytical, local).
narrative_ontology:stakeholder_non_agent(unaudited_contentment_as_entitlement, the_accident_of_visibility).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unaudited_contentment_as_entitlement, anton).
narrative_ontology:fixing_cost_class(unaudited_contentment_as_entitlement, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its most charitable reading, the arrangement lets a household run without constant renegotiation: each party has an implicit role, and stability is read as a signal that the division of labor is working well enough not to need re-examination.
% TRANSFER_FUNCTION: Moves the cost of emotional labor, self-monitoring, and grievance-suppression from Anton to Klára, while moving the benefit of an apparently well-functioning household and a clean conscience to Anton, who never has to reconcile the ledger because he never opens it.
% ABSENT_VOICES: Klára herself is present but structurally silenced within the arrangement — her complaint, if voiced, would be read as a violation of the contentment the marriage depends on rather than as data. Her sister is proximate but excluded from standing. A marriage counselor or outside observer who could formally interrogate the ledger has never been brought in.
% DISAPPEARANCE_RATIONALE: If Anton lost the ability to treat silence as proof of adequacy — if he were structurally required to inquire rather than infer — the household would have to renegotiate labor and acknowledgment on different terms; Klára's cost would become visible and negotiable rather than assumed away, and Anton's exemption from investigation would end.
% FOUNDING_PROBLEM: The arrangement was built to solve the ordinary problem of running a shared household without exhausting both parties in constant negotiation — some things need to be assumed stable so life can proceed.
% FOUNDING_PROBLEM_CORROBORATION: Klára's sister, an outsider to the marriage, attests the stability is now maintained by Klára's suppression rather than by mutual adequacy; Klára herself, when the accident of visibility briefly forces the topic, corroborates this from inside. No party benefiting from the current arrangement — Anton — has independently corroborated that the founding problem (avoiding needless renegotiation) still describes what is actually happening; his account rests entirely on inference from her silence, which is the mechanism under examination.
narrative_ontology:disappearance_verdict(unaudited_contentment_as_entitlement, world_rearranges).
narrative_ontology:founding_problem_status(unaudited_contentment_as_entitlement, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unaudited_contentment_as_entitlement, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-09',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(unaudited_contentment_as_entitlement, 'none', 1).
narrative_ontology:epsilon_provenance(unaudited_contentment_as_entitlement, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unaudited_contentment_as_entitlement_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unaudited_contentment_as_entitlement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unaudited_contentment_as_entitlement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71) because the cost transferred from Klára to Anton — years of self-monitoring, suppressed grievance, and unacknowledged labor — is substantial and sustained, not a one-time favor. Suppression is authored at 0.62 rather than higher because the suppression here is partly internalized (Klára has learned defensiveness follows complaint, so she pre-empts it herself) rather than externally coerced by explicit threat — this is exactly the ambiguity captured in the suppression-mechanism omega below. Theater ratio is low-moderate (0.28): the household genuinely functions, meals are made, routines hold — the coordination is real, which is precisely what makes the extraction easy to mistake for pure coordination. Accessibility collapse (0.58) reflects that alternatives (direct inquiry, renegotiation, outside mediation) are not physically blocked but have become psychologically unavailable to both parties over time. Resistance is authored low (0.34) because Klára's resistance is muted by design — the arrangement's whole mechanism is that resistance doesn't surface until an accident forces it.
 *
 * PERSPECTIVAL GAP:
 *   From Anton's seat, the marriage looks like a rope: stable, functioning, no complaints, therefore no problem, therefore his ledger is honestly balanced. From Klára's seat, the same structure is a snare: her silence is not evidence of adequacy but the cost itself, extracted continuously and rendered invisible by the very absence of complaint Anton reads as proof. The engine should compute these as genuinely different seat-types from the same structural data — that divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Anton is declared the beneficiary: he receives the benefit of a household that looks fine and a conscience he never has to examine, and his exit options are comparatively mobile (he could leave, seek counseling, or simply ask — none of these are foreclosed to him). Klára is declared the victim: she bears the suppressed cost and her exit is identity-locked — leaving would mean dismantling a shared identity and social presentation built around exactly the contentment her silence produces. The directionality derivation should place Anton near the low-d beneficiary end and Klára near the high-d target end; no override is needed since the beneficiary/victim declarations and exit-option asymmetry already capture the real relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure coordination (rope) precisely because a rope requires that participants be net beneficiaries and that alternatives not be suppressed — neither holds here once Klára's interior cost is taken seriously. It also prevents mislabeling it as pure theater (piton), because the household's coordination function is genuinely real and Anton genuinely benefits in a concentrated way — a piton requires no concentrated beneficiary, and here there plainly is one. The snare classification is warranted by the combination of a real (if partial) coordination story used as cover, an identifiable victim, and a mechanism — unaudited inference from silence — that persists specifically because it exempts the beneficiary from ever having to investigate its cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is Klára''s silence-as-suppression structural (fear of Anton''s defensiveness, economic/social dependency on the marriage) or internalized (she has come to believe her own contentment ought to be inferable this way, or that voicing cost would be illegitimate)?',
    'Post-accident trajectory: if Klára''s suppression persists even after Anton''s partial recognition removes the immediate risk of defensiveness, that indicates internalization; if it lifts once the external risk is addressed, it was primarily structural.',
    'If substantially internalized, the effective suppression is higher than the structural indicators alone suggest, and the accident of visibility will produce only temporary, incomplete correction rather than durable renegotiation — consistent with the story''s premise of partial recognition without full correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether Klára''s silence is externally coerced, self-imposed, or both, and in what proportion.').

omega_variable(
    inference_vs_inquiry_threshold,
    'Is there a version of Anton''s behavior — some threshold of attentiveness short of a full explicit audit — that would count as genuine inquiry rather than mere inference from absence of complaint, and did he ever cross it before the accident?',
    'A detailed behavioral history of whether Anton ever directly asked Klára an open-ended question about her experience of the arrangement, versus only reading her behavior and drawing conclusions.',
    'If he never once directly inquired, the snare classification is strongly supported — the exemption from investigation is total and structural. If he inquired occasionally but she deflected, the picture shifts toward a more genuinely coordinated but still asymmetric arrangement, closer to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inference_vs_inquiry_threshold, conceptual, 'Whether Anton''s conduct crosses from passive inference into active (if imperfect) inquiry at any point prior to the forcing accident.').

omega_variable(
    accident_correction_completeness,
    'Does the accident of visibility produce a durable renegotiation of the ledger, or only a temporary, incident-specific concession that leaves the underlying inferential mechanism (silence = proof of adequacy) intact for future use?',
    'Observe whether Anton institutes any standing practice of inquiry after the accident, versus reverting to inference-from-silence once the immediate visible crisis passes.',
    'A reversion would confirm the constraint''s persistence mechanism is robust to isolated disruptions — the snare survives partial exposure. A durable change would indicate the accident functioned as a genuine correction, shifting the classification toward scaffold (a transitional crisis resolving into renegotiated terms).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accident_correction_completeness, empirical, 'Whether partial recognition triggers durable structural change or reverts to the prior inferential regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unaudited_contentment_as_entitlement, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unau_tr_t0, unaudited_contentment_as_entitlement, theater_ratio, 0, 0.1).
narrative_ontology:measurement(unau_tr_t4, unaudited_contentment_as_entitlement, theater_ratio, 4, 0.13).
narrative_ontology:measurement(unau_tr_t8, unaudited_contentment_as_entitlement, theater_ratio, 8, 0.17).
narrative_ontology:measurement(unau_tr_t12, unaudited_contentment_as_entitlement, theater_ratio, 12, 0.2).
narrative_ontology:measurement(unau_tr_t16, unaudited_contentment_as_entitlement, theater_ratio, 16, 0.23).
narrative_ontology:measurement(unau_tr_t20, unaudited_contentment_as_entitlement, theater_ratio, 20, 0.26).
narrative_ontology:measurement(unau_tr_t24, unaudited_contentment_as_entitlement, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(unau_be_t0, unaudited_contentment_as_entitlement, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unau_be_t4, unaudited_contentment_as_entitlement, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(unau_be_t8, unaudited_contentment_as_entitlement, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(unau_be_t12, unaudited_contentment_as_entitlement, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(unau_be_t16, unaudited_contentment_as_entitlement, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(unau_be_t20, unaudited_contentment_as_entitlement, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(unau_be_t24, unaudited_contentment_as_entitlement, base_extractiveness, 24, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unaudited_contentment_as_entitlement, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unaudited_contentment_as_entitlement, attachment_coordination).
narrative_ontology:boltzmann_floor_override(unaudited_contentment_as_entitlement, 0.08).

% DUAL FORMULATION NOTE:
% This story is authored as a standalone interpersonal constraint. It could form a family with a sibling story examining the sister's excluded-observer position as its own constraint (her limited standing to compel audit), but no such sibling exists in this corpus batch; the affects_constraints array is left empty rather than inventing an unauthored link.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
