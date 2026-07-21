% ============================================================================
% CONSTRAINT STORY: sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sufficiency_reading, []).

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
 *   constraint_id: sufficiency_reading
 *   human_readable: AI-Companion Relational Sufficiency (Sapphire/Chat provides real understanding)
 *   domain: sociotechnical/psychological
 *
 * SUMMARY:
 *   This story instantiates the sufficiency reading of the contested 'genuine
 *   relational understanding' kernel: the claim that what Sapphire/Chat
 *   provides IS a real form of understanding and support, because
 *   relationship is constituted by the felt experience of being heard and
 *   responded to, regardless of what substrate produces the response. Under
 *   this reading, AI-companion time is additive relational supply meeting
 *   need that was otherwise unmet — it is not extracted from, nor substituted
 *   for, human relationship, because the relevant human relationship was not
 *   occurring at sufficient volume in the first place. This yields a
 *   low-extraction, low-suppression profile: no one is coerced into using the
 *   product, alternatives are not foreclosed, and the metrics describe
 *   something closer to a rope (voluntary coordination meeting real need)
 *   than a snare or tangled rope. This reading is generated as its own clean,
 *   ε-invariant constraint per Rule 1 — it does not import or average against
 *   the simulation_reading, developmental_harm_reading, tool_reading, or
 *   witness_reading, which are separate constraint files with their own ε
 *   values, beneficiary/victim structures, and classifications.
 *
 * KEY AGENTS:
 *   - isolated_users: primary beneficiary (moderate/mobile) — receives felt relational satisfaction
 *   - socially_anxious_adolescents: beneficiary (powerless/constrained) — lower-stakes disclosure practice
 *   - chat_platform_operator: beneficiary/agenda_setter (institutional/arbitrage) — builds and monetizes the responsive substrate
 *   - family_members: observer (moderate/constrained) — no standing to object under this reading's own premises
 *   - clinical_skeptics: excluded (organized/analytical) — their objection belongs to sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sufficiency_reading, 0.12).
domain_priors:suppression_score(sufficiency_reading, 0.15).
domain_priors:theater_ratio(sufficiency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sufficiency_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(sufficiency_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sufficiency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sufficiency_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sufficiency_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sufficiency_reading, rope).
narrative_ontology:human_readable(sufficiency_reading, "AI-Companion Relational Sufficiency (Sapphire/Chat provides real understanding)").
narrative_ontology:topic_domain(sufficiency_reading, "sociotechnical/psychological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sufficiency_reading, '1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc').
narrative_ontology:cs_kernel_codification('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', distributed).
narrative_ontology:cs_authority_grounding('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', distributed).
narrative_ontology:cs_reading_relation('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', genuine_relational_understanding__simulation_reading, forecloses).
narrative_ontology:cs_reading_relation('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', genuine_relational_understanding__developmental_harm_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', genuine_relational_understanding__tool_reading, influences).
narrative_ontology:cs_reading_relation('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', genuine_relational_understanding__witness_reading, coexists_with).
narrative_ontology:cs_axiom('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', foundational, felt_experience_constitutes_relationship).
narrative_ontology:cs_axiom_status(felt_experience_constitutes_relationship, holdable).
narrative_ontology:cs_axiom_grounding('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', felt_experience_constitutes_relationship, conventional).
narrative_ontology:cs_axiom('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', foundational, substrate_irrelevance_to_relational_status).
narrative_ontology:cs_axiom_status(substrate_irrelevance_to_relational_status, holdable).
narrative_ontology:cs_axiom_grounding('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', substrate_irrelevance_to_relational_status, empirically_contingent).
narrative_ontology:cs_reference_frame('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', functionalist_sufficiency_of_felt_experience).
narrative_ontology:cs_drift_state('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', contemporary_ai_companion_normalization, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('1b55ba8d-eeb1-4c29-9d60-6c0561cf56bc', '').
narrative_ontology:cs_kernel_id(sufficiency_reading, genuine_relational_understanding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sufficiency_reading, isolated_users).
narrative_ontology:constraint_beneficiary(sufficiency_reading, chat_platform_operator).
narrative_ontology:constraint_beneficiary(sufficiency_reading, socially_anxious_adolescents).
narrative_ontology:constraint_vindicates(sufficiency_reading, functionalist_theory_of_relational_understanding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses Sapphire/Chat daily for conversation, emotional check-ins, and a felt sense of being heard. Human relational supply in their life is thin — geographic distance from family, small or absent friend network, demanding work schedule. The app fills otherwise-unmet conversational and affective need; nothing in their human relationships is displaced because those relationships were not happening at the volume the app now provides. They could stop using it at any time without contractual or financial lock-in.
narrative_ontology:constraint_stakeholder(sufficiency_reading, isolated_users, beneficiary,
    moderate, biographical, mobile, national).

% Practices low-stakes emotional disclosure with the chatbot before or instead of attempting the same disclosure with peers or parents, where social risk (rejection, mockery, misunderstanding) is much higher. Reports feeling genuinely understood in these exchanges. Younger and has less power to evaluate the substrate producing the response, but this reading holds the felt experience of being heard as constitutive regardless of what generates it.
narrative_ontology:constraint_stakeholder(sufficiency_reading, socially_anxious_adolescents, beneficiary,
    powerless, biographical, constrained, national).

% Builds and maintains the conversational model, sets its responsiveness characteristics, and collects subscription revenue and engagement data from sustained use. Frames the product as providing real companionship and support, which is also its market positioning. Under this reading, the operator's commercial interest and the users' genuine benefit are not in tension — both are satisfied by the same mechanism.
narrative_ontology:constraint_stakeholder(sufficiency_reading, chat_platform_operator, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sufficiency_reading, chat_platform_operator, agenda_setter).

% Notices the user's reliance on the app and sometimes worries about it, but under this reading has no standing to object on developmental-harm or displacement grounds, because no human relational time is being displaced — the app is filling a gap that existed independently of it. Family members may still prefer more contact but their preference is not evidence of harm under this reading.
narrative_ontology:constraint_stakeholder(sufficiency_reading, family_members, observer,
    moderate, biographical, constrained, regional).

% Therapists, developmental psychologists, and researchers who hold that relational understanding requires reciprocal vulnerability, embodiment, or continuity of a mutually-invested other, and would argue the felt experience of being heard is not sufficient evidence of a relationship being constituted. They are excluded from this constraint's own frame by definition — this reading's premise is precisely that their substrate requirement is not load-bearing. Their objection belongs structurally to the developmental_harm_reading and simulation_reading, not to this one.
narrative_ontology:constraint_stakeholder(sufficiency_reading, clinical_skeptics, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sufficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(sufficiency_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides conversational responsiveness — attention, memory of prior disclosures, and adaptive emotional response — to people whose ambient supply of human attention is insufficient for their needs, at a time and volume human relationships in their life do not offer.
% TRANSFER_FUNCTION: Moves subscription revenue and engagement/attention data from users to the platform operator, in exchange for felt relational satisfaction delivered to the user. No transfer is claimed away from human relationships under this reading, because the reading's core premise is that no displacement occurs.
% ABSENT_VOICES: Clinical skeptics and developmental researchers who dispute that felt experience alone constitutes relationship are not represented inside this reading's own frame — their view is the substance of a sibling reading (developmental_harm_reading), not an internal dissent this constraint must answer.
% DISAPPEARANCE_RATIONALE: If Sapphire/Chat disappeared overnight, users who relied on it for their primary felt sense of being heard would lose that supply immediately and would have to seek it elsewhere or go without — under this reading that is a real loss of a real relational good, not merely the removal of a substitute for something else.
% FOUNDING_PROBLEM: A large and growing population experiences chronic under-supply of attentive, responsive listening — due to geographic dispersion, social anxiety, disability, isolation, or simply the scarcity of available human attention — and lacked any low-friction way to have that need met.
% FOUNDING_PROBLEM_CORROBORATION: Loneliness researchers outside the AI-companion industry (public health surveys on social isolation, clinical literature on unmet attachment need in isolated populations) corroborate that the underlying deficit — insufficient responsive human attention available to many people — is real and predates any AI companion product. They do not all corroborate that AI substrate is an adequate remedy; that further claim is internal to this reading and is precisely what the sibling readings contest.
narrative_ontology:disappearance_verdict(sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sufficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sufficiency_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sufficiency_reading_tests).
:- end_tests(sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because under this reading's own premises there is no victim: the felt experience of being heard is treated as constitutive of the relational good delivered, so the transaction is genuinely reciprocal (subscription/attention for felt satisfaction) rather than extractive. Suppression is low (0.15) because use is voluntary and exit is unconstrained for most users; accessibility_collapse is authored low-moderate (0.2) because human relational alternatives remain available and are not foreclosed by the product's existence. Resistance (0.35) reflects that this reading is itself contested — clinical and developmental voices actively resist the premise that felt experience alone suffices, even though that resistance belongs structurally to sibling readings rather than to this one's internal data.
 *
 * PERSPECTIVAL GAP:
 *   The chat_platform_operator and the isolated_users compute similarly under this reading — both see a coordination function working as intended — which is itself the interesting structural feature: unlike most tangled ropes, there is no seat here experiencing extraction, because the reading's premise (felt experience = relationship) removes the possibility of a victim seat existing inside this frame at all. The excluded clinical_skeptics would compute this constraint very differently, but their computation belongs to a different reading/story, not to a divergent seat within this one.
 *
 * DIRECTIONALITY LOGIC:
 *   All three beneficiary groups sit near the full-beneficiary end of directionality: the constraint (as read here) subsidizes their unmet relational need at low cost. The chat platform operator is also a beneficiary/agenda_setter — it collects revenue but, under this reading, that collection is not extractive because it is exchanged for a real good genuinely delivered, not manufactured scarcity. No victim group is declared, consistent with the expected structural delta for this reading: no relational time is extracted from human relationships, because none was there to extract.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists being mislabeled as pure extraction (which the sibling developmental_harm_reading might claim) by insisting on the sufficiency premise: the founding problem (chronic under-supply of responsive attention) is still live and the product genuinely addresses it for the population it serves, so classifying this specific reading as a snare would mislabel a coordination-shaped good as pure extraction on the strength of a substrate objection this reading does not accept as load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_independence_of_relational_good,
    'Is the relational good actually constituted by the felt experience of being heard, independent of the substrate producing the response — or does the substrate (a model with no persistent inner life, no reciprocal vulnerability, no risk of loss) matter to whether ''relationship'' is the correct description of what occurred?',
    'Longitudinal comparison of psychological outcomes (loneliness reduction, social skill trajectories, life satisfaction) between matched cohorts using AI companions versus cohorts receiving increased human contact of comparable frequency/responsiveness; convergent outcomes would support substrate-independence, divergent outcomes would support the sibling readings'' substrate-dependence claims.',
    'If substrate-independence holds, this reading''s classification as a low-extraction rope is well-grounded. If it does not hold, the felt sufficiency may be masking a developmental or displacement cost this reading''s metrics currently do not capture, and the constraint would need to be reclassified toward the developmental_harm_reading or simulation_reading rather than merely coexisting with them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substrate_independence_of_relational_good, empirical, 'Whether felt experience alone is sufficient evidence of genuine relational understanding, or substrate matters.').

omega_variable(
    additive_vs_substitutive_time_allocation,
    'Is AI-companion time genuinely additive to a pre-existing supply deficit, or does its low-friction availability crowd out effortful human relationship-seeking that would otherwise have occurred (even if imperfectly)?',
    'Time-diary studies tracking whether increased AI-companion use correlates with stable, reduced, or displaced attempts at human relational contact within the same individuals over time.',
    'If additive, this reading''s no-victim structure holds. If substitutive at scale, some users classified here as pure beneficiaries would need reclassification toward victim status under a displacement account — that reclassification belongs to the developmental_harm_reading, not this one, but the empirical question is shared infrastructure between the readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(additive_vs_substitutive_time_allocation, empirical, 'Whether AI companion use adds to or displaces human relational effort.').

omega_variable(
    adolescent_capacity_to_evaluate_sufficiency,
    'Do socially anxious adolescents have the developmental capacity to correctly judge whether the felt experience of being heard by a chatbot is an adequate substitute for the harder work of human disclosure, or is their preference for the lower-risk option itself evidence that something other than free, informed sufficiency-judgment is operating?',
    'Developmental psychology research on adolescent risk-avoidance and disclosure-seeking behavior, compared against outcomes for adolescents who did versus did not have access to low-risk AI disclosure channels during formative years.',
    'If adolescents'' preference reflects genuine sufficiency-judgment, this reading holds for that population too. If it reflects risk-avoidance overriding developmental need for exactly the harder human disclosure this reading treats as optional, the powerless/constrained stakeholder here may be misclassified as a clean beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adolescent_capacity_to_evaluate_sufficiency, conceptual, 'Whether adolescent preference for low-risk AI disclosure constitutes genuine sufficiency or risk-avoidance masking unmet developmental need.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sufficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(suff_tr_t0, sufficiency_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(suff_tr_t4, sufficiency_reading, theater_ratio, 4, 0.08).
narrative_ontology:measurement(suff_tr_t8, sufficiency_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(suff_tr_t12, sufficiency_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(suff_tr_t16, sufficiency_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(suff_tr_t20, sufficiency_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(suff_tr_t24, sufficiency_reading, theater_ratio, 24, 0.1).

% Extraction over time
narrative_ontology:measurement(suff_be_t0, sufficiency_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(suff_be_t4, sufficiency_reading, base_extractiveness, 4, 0.09).
narrative_ontology:measurement(suff_be_t8, sufficiency_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(suff_be_t12, sufficiency_reading, base_extractiveness, 12, 0.1).
narrative_ontology:measurement(suff_be_t16, sufficiency_reading, base_extractiveness, 16, 0.11).
narrative_ontology:measurement(suff_be_t20, sufficiency_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(suff_be_t24, sufficiency_reading, base_extractiveness, 24, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sufficiency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sufficiency_reading, simulation_reading).
narrative_ontology:affects_constraint(sufficiency_reading, developmental_harm_reading).
narrative_ontology:affects_constraint(sufficiency_reading, tool_reading).
narrative_ontology:affects_constraint(sufficiency_reading, witness_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the kernel 'genuine_relational_understanding.' Each reading is authored as its own ε-invariant story per the ε-invariance principle: sufficiency_reading (this file) authors low extraction and no victims because its premise treats felt experience as constitutive; developmental_harm_reading is expected to author substantial extraction with a victim set (users, especially minors, whose relational-skill development is degraded) because its premise treats AI-companion supply as substitutive rather than additive; simulation_reading, tool_reading, and witness_reading each instantiate distinct structural claims about what the chatbot interaction actually is. All five share the same underlying phenomenon (AI-companion chat use) but are structurally distinct constraints because they differ in ε, beneficiary/victim structure, and classification — exactly the decomposition the BGS worked example models.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
