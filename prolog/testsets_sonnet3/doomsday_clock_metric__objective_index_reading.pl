% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Existential-Risk Index
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Bulletin of the Atomic Scientists sets the Doomsday Clock's minute
 *   hand annually, framing the decision as expert synthesis of quantifiable
 *   existential-risk indicators across nuclear, climate, biological, and
 *   technological domains. Under the objective-index reading, this framing is
 *   taken at its own word: the setting purports to measure a real, external
 *   quantity (aggregate existential risk) through disciplined expert
 *   judgment, analogous to an index number. The structural consequence of
 *   this framing — regardless of whether the underlying judgments are sound —
 *   is that indicator selection, weighting, and synthesis become unreviewable
 *   expert prerogatives, and any public or political contestation of the
 *   number is cast as scientific illiteracy rather than legitimate
 *   disagreement about values and priorities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.58).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.71).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Existential-Risk Index").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '44218350-fcf2-4f07-85d6-abe4686d49e7').
narrative_ontology:cs_kernel_codification('44218350-fcf2-4f07-85d6-abe4686d49e7', formalized).
narrative_ontology:cs_authority_grounding('44218350-fcf2-4f07-85d6-abe4686d49e7', expertise).
narrative_ontology:cs_interpretation_layer_present('44218350-fcf2-4f07-85d6-abe4686d49e7').
narrative_ontology:cs_reading_relation('44218350-fcf2-4f07-85d6-abe4686d49e7', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('44218350-fcf2-4f07-85d6-abe4686d49e7', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('44218350-fcf2-4f07-85d6-abe4686d49e7', foundational, risk_synthesis_is_measurement_not_judgment).
narrative_ontology:cs_axiom_status(risk_synthesis_is_measurement_not_judgment, holdable).
narrative_ontology:cs_axiom_grounding('44218350-fcf2-4f07-85d6-abe4686d49e7', risk_synthesis_is_measurement_not_judgment, empirically_contingent).
narrative_ontology:cs_axiom('44218350-fcf2-4f07-85d6-abe4686d49e7', secondary, expert_weighting_requires_no_external_review).
narrative_ontology:cs_axiom_status(expert_weighting_requires_no_external_review, holdable).
narrative_ontology:cs_axiom_grounding('44218350-fcf2-4f07-85d6-abe4686d49e7', expert_weighting_requires_no_external_review, conventional).
narrative_ontology:cs_reference_frame('44218350-fcf2-4f07-85d6-abe4686d49e7', instrumentalist_measurement_authority).
narrative_ontology:cs_drift_state('44218350-fcf2-4f07-85d6-abe4686d49e7', contemporary_multi_domain_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('44218350-fcf2-4f07-85d6-abe4686d49e7', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, credentialed_risk_expert_class).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_publics).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, elected_policymakers_outside_the_board).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, lay_risk_communicators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the minute-hand position annually, presenting the decision as expert synthesis of nuclear, climate, biological, and disruptive-technology indicators into a single measurable reading. Controls the methodology, the indicator weighting, and the final announcement; no external body reviews or can override the setting. Its authority and continued relevance as an institution depend on the clock being read as objective science rather than as an editorial or advocacy signal.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board, agenda_setter,
    institutional, generational, arbitrage, global).

% Physicists, security scholars, and risk scientists who serve on or advise the board gain outsized epistemic authority and media platform from the clock's framing as a measurable index. Their standing as the legitimate interpreters of existential risk is reinforced each time the setting is treated as a finding rather than a judgment call.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, credentialed_risk_expert_class, beneficiary,
    organized, generational, arbitrage, global).

% Receive the clock's setting as a settled fact about how close humanity is to catastrophe and calibrate anxiety, donations, and political attention accordingly. Have no mechanism to contest the indicator weights or challenge the synthesis; their only options are to accept the number or dismiss the whole exercise, since the underlying normative judgments are presented as measurement rather than as decisions open to public deliberation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_publics, payer,
    powerless, biographical, trapped, global).

% Face constituent and media pressure keyed to the clock's yearly setting without having participated in or being able to audit the weighting choices behind it. Must respond to a metric whose construction they do not control, ceding a portion of the risk-framing agenda to an unelected expert board.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, elected_policymakers_outside_the_board, payer,
    powerful, biographical, constrained, national).

% Journalists, educators, and science communicators who must translate the clock setting for the public are structurally required to defer to the board's synthesis rather than exercise independent judgment about how to characterize existential risk, since departing from the 'expert index' framing costs them credibility.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, lay_risk_communicators, payer,
    moderate, biographical, constrained, global).

% Other institutions producing existential-risk assessments (intergovernmental panels, independent research centers) using different methodologies are not part of the clock-setting process and receive far less public attention; their competing objective claims are structurally sidelined by the clock's singular, memorable index format.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, rival_risk_assessment_bodies, excluded,
    organized, generational, constrained, global).

% Study the clock's methodology and public reception, documenting how the objective-index framing forecloses public deliberation about which indicators should count and how they should be weighted.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synthesizes disparate technical indicators (nuclear posture, climate trajectories, biosecurity incidents, disruptive technology risk) into a single, memorable, trackable figure that lets non-experts monitor aggregate existential risk trends without parsing each domain's technical literature.
% TRANSFER_FUNCTION: Moves interpretive authority over what counts as dangerous, and by how much, from democratically accountable bodies and the general public to a closed expert board; moves public attention and emotional register (anxiety, complacency) in whatever direction the annual setting indicates, without the public having input into the weighting.
% ABSENT_VOICES: Rival risk-assessment institutions with different methodologies are not consulted or credited; ordinary publics whose anxiety and civic attention are calibrated by the number have no channel to contest the indicator weights; elected policymakers do not co-author the synthesis they are then judged against.
% DISAPPEARANCE_RATIONALE: The board and its expert allies would say existential risk communication would fragment and lose its shared reference point, with real coordination loss. Policymakers, communicators, and critics would say public risk discourse would become more genuinely pluralistic and contestable, with technical risk assessment continuing through peer-reviewed channels that already exist independent of the clock — the world's underlying risk indicators are unaffected either way, only the singular authoritative narration of them disappears.
% FOUNDING_PROBLEM: In 1947, scientists who had worked on the atomic bomb wanted a simple, durable device to keep nuclear danger vivid in public consciousness against the drift of complacency and forgetting.
% FOUNDING_PROBLEM_CORROBORATION: The board itself attests the problem (existential risk complacency) remains fully live and has expanded to new domains. Independent science-communication researchers and several political scientists outside the board attest that the clock has shifted from a memory-aid against complacency into a self-perpetuating authority claim whose annual ritual sustains the board's platform independent of whether the underlying indicator synthesis is more accurate than competing assessments.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) is substantial but not extreme: the clock does synthesize real technical information and the board includes genuine subject-matter experts, so there is real coordination value. But the objective-index framing converts what are inescapably normative choices (how to weight nuclear risk against climate risk against biosecurity risk, how to translate qualitative judgment into a minute-hand position) into claims of measurement, and that conversion is what generates the extraction: it forecloses public and political contestation of choices that are not, in fact, purely empirical. Suppression (0.71) is high because the reading requires suppressing the visibility of the normative content entirely — any acknowledgment that indicator weighting is a value judgment rather than a measurement would collapse the reading's core claim. Theater ratio rises over the measured interval (0.18 to 0.42) as the annual announcement ritual and media cycle increasingly substitute for demonstrated predictive or calibration validity of the index itself.
 *
 * PERSPECTIVAL GAP:
 *   From the board's own seat, the objective-index reading is simply an accurate description of what expert synthesis does — apply disciplined judgment to empirical indicators, exactly as any scientific index does. From the payer seats, the same activity looks like a closed epistemic monopoly dressed in the language of measurement to foreclose exactly the kind of public deliberation that decisions with major normative stakes ought to receive. The engine computes these as different per-seat classifications from the same structural facts; the divergence is the object of study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The board and the credentialed expert class are structural beneficiaries: the objective-index framing is what generates their outsized platform and unreviewable authority, so their directionality sits near the full-beneficiary end. Democratic publics are the clearest targets — they receive the number as settled fact with no channel to contest its construction, and are trapped in the sense that they cannot opt out of a media environment saturated with the clock's framing. Elected policymakers and lay communicators are payers with somewhat more mobility (constrained rather than trapped) because they can, at real reputational cost, publicly dissent from the number's authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nuclear complacency, 1947) is genuinely contested as live or dead: nuclear risk persists, but the objective-index apparatus has expanded far beyond its founding scope into climate, biosecurity, and AI risk domains where the board's comparative expertise and its right to unreviewable synthesis are much less established. The mandatrophy question is whether the apparatus retains legitimacy as it scales its claimed jurisdiction over the definition of existential risk without a corresponding expansion of accountability — the objective-index reading is the reading most vulnerable to mandatrophy because it is the reading that most explicitly denies its own normative content is subject to any external correction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Is the doomsday_clock_metric kernel''s setting genuinely an objective empirical synthesis (this reading), a strategically chosen mobilization device (performative_tool_reading), or an irreducible entanglement of empirical and normative judgment that cannot be cleanly separated (hybrid_legitimacy_reading)?',
    'Compare the board''s internal deliberation records (if disclosed) against its public ''objective synthesis'' framing: if indicator weights are demonstrably adjusted to produce headline-friendly minute-hand movements independent of underlying indicator changes, that evidences the performative reading; if weights track a stable, disclosed methodology insensitive to media-cycle considerations, that evidences this reading; if the board''s own statements acknowledge irreducible value judgments in weighting while still claiming synthesis authority, that evidences the hybrid reading.',
    'The location of the true reading determines whether the constraint''s extraction is a byproduct of a mistaken (but sincere) objectivity claim, a deliberate persuasion strategy, or an unavoidable feature of any expert existential-risk communication effort — each implies a different remedy (methodological transparency, disclosure of strategic intent, or acceptance of hybrid governance with added accountability mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Where among the three kernel readings the true structure of clock-setting actually sits.').

omega_variable(
    indicator_weighting_replicability,
    'Would independent experts applying the board''s stated methodology to the same underlying data reliably reproduce the same minute-hand setting, or does the synthesis require irreducible, non-replicable judgment calls?',
    'Blind replication study: give the disclosed indicator set and methodology to an independent panel of comparable experts and compare their derived settings to the board''s actual announced setting across several years.',
    'High replicability would substantially support the objective-index reading and lower the justified extraction/suppression scores; low replicability would undermine the reading''s core claim and support reclassification toward hybrid_legitimacy or performative_tool framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indicator_weighting_replicability, empirical, 'Whether the claimed objective synthesis methodology is actually replicable by independent experts.').

omega_variable(
    expert_authority_beneficiary_scope,
    'Does the credentialed risk expert class benefit primarily through genuine field-building (more resources and attention to existential risk research generally) or primarily through personal/institutional prestige capture that would persist even if the underlying risk assessments were less accurate?',
    'Track whether funding and attention for existential-risk research broadly (including work outside the board and its methodology) has grown in step with the board''s own platform growth, or whether the board''s platform has grown disproportionately relative to the wider field.',
    'If the wider field benefits proportionally, the beneficiary structure is closer to a genuine positive externality of Rope-like coordination; if the board''s platform outpaces field-wide growth, the tangled_rope classification is reinforced with the extraction more clearly a private capture of collective attention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expert_authority_beneficiary_scope, empirical, 'Whether expert-class benefit is field-wide coordination gain or concentrated institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(doom_tr_t8, doomsday_clock_metric__objective_index_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(doom_tr_t16, doomsday_clock_metric__objective_index_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(doom_tr_t24, doomsday_clock_metric__objective_index_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(doom_tr_t32, doomsday_clock_metric__objective_index_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(doom_tr_t40, doomsday_clock_metric__objective_index_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(doom_be_t8, doomsday_clock_metric__objective_index_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(doom_be_t16, doomsday_clock_metric__objective_index_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(doom_be_t24, doomsday_clock_metric__objective_index_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(doom_be_t32, doomsday_clock_metric__objective_index_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(doom_be_t40, doomsday_clock_metric__objective_index_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(doom_su_t8, doomsday_clock_metric__objective_index_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(doom_su_t16, doomsday_clock_metric__objective_index_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(doom_su_t24, doomsday_clock_metric__objective_index_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(doom_su_t32, doomsday_clock_metric__objective_index_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(doom_su_t40, doomsday_clock_metric__objective_index_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.1).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept of 'the Doomsday Clock setting' per the epsilon-invariance principle. The objective_index_reading (this file) authors ε=0.58 for the standing arrangement as an unreviewable expert-authority claim. The performative_tool_reading authors a distinct ε reflecting deliberate strategic framing for policy mobilization. The hybrid_legitimacy_reading authors a distinct ε reflecting entangled empirical/normative judgment with partial accountability mechanisms. Each carries its own stakeholder set, its own claimed_type, and its own metrics; they are linked, not merged, because measuring the constraint via 'is this objective science' versus 'is this a mobilization tool' versus 'is this irreducibly hybrid' yields different ε values — three constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
