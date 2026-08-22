% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate (Composite Overdetermined Reading)
 *   domain: historical/sociological/cultural
 *
 * SUMMARY:
 *   This constraint story models the honor satisfaction substrate â the
 *   dueling institution and its associated code of honor â through the
 *   composite_overdetermined_reading of the honor_satisfaction_substrate
 *   kernel. The reading holds that dueling's decline was overdetermined:
 *   exogenous legal and institutional suppression operated simultaneously
 *   with endogenous honor-code transformation, and the two mechanisms were
 *   causally entangled rather than additive. The constraint was a tangled
 *   rope: it genuinely coordinated grievance resolution among status-equals,
 *   preventing feud and anarchic violence, while asymmetrically extracting
 *   bodily risk and compelled participation from individual gentlemen. Its
 *   decline exhibits both rope-breaking (coordination collapse under legal
 *   pressure) and mountain erosion (the honor substrate itself became
 *   unthinkable), but the composite reading treats these as non-independent
 *   processes.
 *
 * KEY AGENTS:
 *   - gentleman_participants: Primary targets (moderate power, identity_locked exit) â bore the extraction of bodily risk and compelled participation.
 *   - honor_community_enforcers: Agenda-setters and secondary beneficiaries (organized power, constrained exit) â administered the code and collected social authority.
 *   - state_legal_apparatus: Exogenous suppressor (institutional power, mobile exit) â supplied legal and administrative pressure that accelerated collapse.
 *   - gentleman_class_collective: Structural beneficiary (organized power, constrained exit) â collected class-boundary maintenance without individual exposure.
 *   - women_and_families: Excluded victims (powerless, trapped exit) â bore costs without voice or recognition.
 *   - religious_moral_authorities: Analytical observers (organized power, analytical exit) â critiqued without capacity to enforce alternative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.72).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate (Composite Overdetermined Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical/sociological/cultural").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '04ef43cd-8cef-4df2-abec-af1842c5a92c').
narrative_ontology:cs_kernel_codification('04ef43cd-8cef-4df2-abec-af1842c5a92c', distributed).
narrative_ontology:cs_authority_grounding('04ef43cd-8cef-4df2-abec-af1842c5a92c', practice).
narrative_ontology:cs_interpretation_layer_present('04ef43cd-8cef-4df2-abec-af1842c5a92c').
narrative_ontology:cs_reading_relation('04ef43cd-8cef-4df2-abec-af1842c5a92c', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('04ef43cd-8cef-4df2-abec-af1842c5a92c', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('04ef43cd-8cef-4df2-abec-af1842c5a92c', foundational, honor_law_non_independence).
narrative_ontology:cs_axiom_status(honor_law_non_independence, holdable).
narrative_ontology:cs_axiom_grounding('04ef43cd-8cef-4df2-abec-af1842c5a92c', honor_law_non_independence, empirically_contingent).
narrative_ontology:cs_axiom('04ef43cd-8cef-4df2-abec-af1842c5a92c', foundational, overdetermined_decline_necessity).
narrative_ontology:cs_axiom_status(overdetermined_decline_necessity, holdable).
narrative_ontology:cs_axiom_grounding('04ef43cd-8cef-4df2-abec-af1842c5a92c', overdetermined_decline_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('04ef43cd-8cef-4df2-abec-af1842c5a92c', integrated_honor_legal_substrate).
narrative_ontology:cs_drift_state('04ef43cd-8cef-4df2-abec-af1842c5a92c', modern_state_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('04ef43cd-8cef-4df2-abec-af1842c5a92c', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, gentleman_class_collective).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, honor_community_enforcers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, gentleman_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual gentlemen of honor compelled by social obligation to accept or deliver challenges, risking death, injury, or social annihilation. Refusal meant ostracism and dissolution of their social identity as gentlemen. Exit was blocked not merely by external barriers but by fusion of self-concept with the honor code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, gentleman_participants, payer,
    moderate, biographical, identity_locked, national).

% The network of seconds, peers, and social arbiters who administered the unwritten code of honor, arranged terms of satisfaction, and policed compliance through gossip, ostracism, and recognition. They derived social authority from their role as gatekeepers of honorable status.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_community_enforcers, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, honor_community_enforcers, beneficiary).

% Legal and military institutions that initially tolerated dueling as a private gentlemanly affair, then progressively criminalized and administratively suppressed it, imposing penalties from fines to imprisonment to professional disqualification, supplying the exogenous suppression mechanism in the overdetermined decline.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% The gentleman class as a collective status group, which benefited from the dueling institution as a boundary mechanism distinguishing honorable men from commoners, women, and dependents. The ritual reinforced class solidarity and hierarchical differentiation even as individual members bore its physical costs.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, gentleman_class_collective, beneficiary,
    organized, generational, constrained, national).

% Women, children, and family members who suffered the material and emotional consequences of dueling deaths and injuries but were structurally excluded from the honor code's operation, its decision-making, and its legitimation narratives.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, women_and_families, excluded,
    powerless, biographical, trapped, local).

% Religious and moral authorities who observed and periodically denounced dueling as sinful or immoral but lacked the social power to override the honor community's enforcement, functioning as external critics rather than participants.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, religious_moral_authorities, observer,
    organized, generational, analytical, national).

% Commoners, laborers, and others beneath the honor threshold who were excluded from the dueling code entirely, denied its recognition, and yet whose presence as non-participants helped define the gentlemanly status the duel preserved.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, lower_classes, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a structured, ritually governed mechanism for status-equals to resolve grievances without descending into feud or anarchic violence, while publicly validating the honorable standing of participants through shared risk.
% TRANSFER_FUNCTION: Transferred bodily risk and death from the collective honor of the gentleman class to individual gentlemen compelled to bear it; transferred dispute-resolution authority from formal legal institutions to informal social arbiters and seconds.
% ABSENT_VOICES: Women and family members who bore the costs of death and injury without voice in the code; religious moralists who condemned the violence but were ignored by the honor community; lower-class men excluded from recognition; modern legal theorists who would reject private violence as usurping state monopoly on legitimate coercion.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction substrate vanished overnight, gentleman identity would lose a core performative pillar, disputes would redirect to formal courts or non-violent resolution, and the class boundary maintained by the duel would require entirely different mechanisms of reproduction.
% FOUNDING_PROBLEM: In the absence of consolidated state institutions with monopoly on legitimate violence, how do status-equals resolve grievances without feud or social chaos while preserving their claim to superior standing?
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists attesting the consolidation of nation-state judicial systems; state legislative records criminalizing private violence; Weberian scholars documenting the shift to state monopoly on legitimate violence. No corroboration from within the benefiting gentleman class or honor community is accepted as independent.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint compelled individual gentlemen to stake their lives and health for collective class status, extracting bodily autonomy and security. Suppression is high (0.70) because refusal triggered social annihilation, and the code was policed by dense informal surveillance. Theater_ratio is moderate (0.40 baseline) because while duels had strong performative elements, they also produced genuine injury and death; the temporal series shows theater rising to 0.84 as the constraint atrophied and residual duels became increasingly symbolic or performative. Accessibility_collapse is very high (0.88) because for embedded gentlemen, refusing the code was nearly unthinkable without dissolving their social identity. Resistance is moderate (0.42) because religious, familial, and legal objections existed but were insufficient to break the constraint without the entangled exogenous and endogenous pressures.
 *
 * PERSPECTIVAL GAP:
 *   The gentleman_participant seat and the gentleman_class_collective seat diverge sharply: from the individual payer's perspective the constraint is pure compulsory endangerment, while from the collective beneficiary perspective it is necessary social infrastructure maintaining class boundaries. The honor_community_enforcers experience the constraint as coordination they maintain, while the state_legal_apparatus eventually experiences it as illegitimate private violence to be suppressed. The engine computes these divergences from the structural role and exit data rather than from any authored type claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the gentleman_class_collective and honor_community_enforcers: the former collects status boundary maintenance, the latter collects social authority. Both receive low directionality (near the beneficiary end). The gentleman_participants are declared victims/payers: they bear the bodily costs and identity compulsion, receiving high directionality (near the target end). The state_legal_apparatus is neither beneficiary nor victim of this constraint; it stands outside as exogenous suppressor and receives a canonical fallback directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misreading the dueling institution as either pure coordination (a rope) or pure extraction (a snare). It had a genuine coordination function â preventing feud and providing structured dispute resolution â which a snare classification would erase. Simultaneously, it had asymmetric extraction â individual gentlemen paid with life and limb for collective class benefit â which a rope classification would obscure. The mandatrophy is resolved: the founding problem (weak state, need for private dispute resolution) is dead, and the constraint persists only as historical residue. The temporal measurements show the characteristic atrophy profile: rising theater_ratio, falling base_extractiveness, and decaying suppression_requirement as the constraint loses operational substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_entanglement_severability,
    'Can exogenous legal suppression and endogenous honor transformation be analytically separated, or are they constitutively entangled in the historical record?',
    'Comparative historical analysis across jurisdictions where legal suppression preceded, followed, or operated simultaneously with cultural change; process-tracing of elite correspondence and legal archives to establish causal interdependence.',
    'If severable, the composite reading decomposes into practice_decline and cultural_contraction as independent mechanisms; if inseparable, the overdetermined reading is vindicated as the only coherent account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_entanglement_severability, conceptual, 'Whether legal and cultural causes of dueling''s decline are analytically separable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of the honor code structural (external social and legal penalties) or internalized (shame, identity fusion, self-policing)?',
    'Post-exit trajectory analysis: examining whether gentlemen who emigrated, converted, or left the class system continued to feel compelled by honor norms.',
    'If internalized, effective suppression exceeds the structural measure because targets carried the constraint with them after external barriers fell; this would raise extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in honor culture.').

omega_variable(
    honor_substrate_naturality,
    'Does the honor code function as a constructed social convention or as a natural-law-like constraint for those embedded in it?',
    'Cross-cultural comparative analysis assessing whether honor violence is historically universal or culturally contingent; examination of whether agents experienced the code as choice or necessity.',
    'If natural-law-like, endogenous transformation is anomalous and requires special explanation; if constructed, the composite reading''s emphasis on entanglement with legal structures is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_substrate_naturality, conceptual, 'Whether the honor substrate is a constructed convention or experienced as natural law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(hono_tr_t12, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(hono_tr_t24, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(hono_tr_t36, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 36, 0.6).
narrative_ontology:measurement(hono_tr_t48, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 48, 0.74).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 60, 0.84).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(hono_be_t12, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(hono_be_t24, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(hono_be_t36, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 36, 0.4).
narrative_ontology:measurement(hono_be_t48, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 48, 0.28).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 60, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(hono_su_t12, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(hono_su_t24, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(hono_su_t36, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 36, 0.38).
narrative_ontology:measurement(hono_su_t48, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 48, 0.24).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 60, 0.14).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the honor_satisfaction_substrate kernel into three structurally distinct constraints per the Îµ-invariance principle: composite_overdetermined_reading (entangled exogenous/endogenous causation), practice_decline_reading (exogenous suppression only), and cultural_contraction_reading (endogenous transformation only). Each reading carries its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
