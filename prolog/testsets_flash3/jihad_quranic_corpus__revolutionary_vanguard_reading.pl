% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Revolutionary Vanguard Reading of Jihad (Fard 'Ayn)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'revolutionary vanguard' reading of Jihad,
 *   which interprets it as an immediate individual obligation (fard 'ayn)
 *   against rulers deemed apostate and foreign occupiers. This reading
 *   bypasses traditional state authority through the doctrine of takfir
 *   (excommunication) and emergency jurisprudence, leading to a highly
 *   decentralized and often indiscriminate form of armed struggle. It expands
 *   the victim set to include civilians through concepts of collective guilt
 *   and rejects classical jurisprudential safeguards like non-combatant
 *   immunity. The high extractiveness and suppression reflect the coercive
 *   nature of this interpretation, which demands extreme sacrifice from its
 *   adherents and imposes severe costs on its targets.
 *
 * KEY AGENTS:
 *   - revolutionary_vanguard_leaders: Agenda-setter (organized/identity_locked) — mobilizes followers, declares targets.
 *   - radicalized_individuals: Beneficiary/Payer (powerless/identity_locked) — gains purpose, pays with life/freedom.
 *   - apostate_rulers: Payer (institutional/trapped) — targeted for overthrow.
 *   - occupying_forces: Payer (institutional/constrained) — targeted in asymmetric warfare.
 *   - civilians_in_target_areas: Payer (powerless/trapped) — suffer indiscriminate violence.
 *   - moderate_muslim_scholars: Excluded (institutional/constrained) — bypassed and delegitimized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Revolutionary Vanguard Reading of Jihad (Fard 'Ayn)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'ada46e49-e2f0-477c-ae79-cb2439c5edcb').
narrative_ontology:cs_kernel_codification('ada46e49-e2f0-477c-ae79-cb2439c5edcb', fixed_text).
narrative_ontology:cs_authority_grounding('ada46e49-e2f0-477c-ae79-cb2439c5edcb', extraction).
narrative_ontology:cs_interpretation_layer_present('ada46e49-e2f0-477c-ae79-cb2439c5edcb').
narrative_ontology:cs_reading_relation('ada46e49-e2f0-477c-ae79-cb2439c5edcb', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('ada46e49-e2f0-477c-ae79-cb2439c5edcb', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_axiom('ada46e49-e2f0-477c-ae79-cb2439c5edcb', foundational, takfir_justifies_rebellion).
narrative_ontology:cs_axiom_status(takfir_justifies_rebellion, holdable).
narrative_ontology:cs_axiom_grounding('ada46e49-e2f0-477c-ae79-cb2439c5edcb', takfir_justifies_rebellion, theological).
narrative_ontology:cs_axiom('ada46e49-e2f0-477c-ae79-cb2439c5edcb', foundational, jihad_fard_ayn_against_apostates_occupiers).
narrative_ontology:cs_axiom_status(jihad_fard_ayn_against_apostates_occupiers, holdable).
narrative_ontology:cs_axiom_grounding('ada46e49-e2f0-477c-ae79-cb2439c5edcb', jihad_fard_ayn_against_apostates_occupiers, theological).
narrative_ontology:cs_reference_frame('ada46e49-e2f0-477c-ae79-cb2439c5edcb', early_islamic_community_under_threat).
narrative_ontology:cs_drift_state('ada46e49-e2f0-477c-ae79-cb2439c5edcb', contemporary_global_jihad_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ada46e49-e2f0-477c-ae79-cb2439c5edcb', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_target_areas).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, moderate_muslim_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret religious texts to declare rulers apostate and jihad an individual obligation, bypassing traditional authorities. They gain legitimacy and power by mobilizing followers for direct action, often operating in clandestine networks.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders, agenda_setter,
    organized, generational, identity_locked, global).

% Are convinced of the immediate, individual obligation to wage jihad. They gain a sense of purpose, belonging, and divine reward, but bear extreme personal risk, including death or imprisonment. Their identity is fused with the cause, making exit unthinkable.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals, beneficiary,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals, payer).

% Are declared illegitimate and targeted for overthrow by the revolutionary vanguard. They face direct violence, destabilization of their regimes, and delegitimization among segments of the population. Their only 'exit' is to concede power or be overthrown.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    institutional, biographical, trapped, national).

% Are targeted as legitimate combatants, often without distinction from civilian populations due to the broad application of collective guilt. They face continuous asymmetric warfare and high casualties, with exit options limited by strategic objectives.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    institutional, biographical, constrained, regional).

% Are caught in the conflict, often deemed complicit or legitimate targets due to the expansive interpretation of 'enemy' and the rejection of non-combatant immunity. They suffer violence, displacement, and loss of life and property, with minimal exit options from conflict zones.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_target_areas, payer,
    powerless, immediate, trapped, local).

% Are bypassed and often condemned by the revolutionary vanguard for their adherence to classical jurisprudence and state authority. They lose influence and face threats for opposing the radical interpretation, but cannot easily abandon their scholarly tradition.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, moderate_muslim_scholars, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decentralized, immediate action against perceived enemies by providing a theological justification that bypasses traditional authority structures, enabling rapid mobilization of individuals and small groups.
% TRANSFER_FUNCTION: Transfers legitimacy from established religious and state authorities to self-appointed vanguard leaders, and transfers the burden of armed struggle from state armies to individual believers, often resulting in the transfer of lives and resources from target populations to the cause.
% ABSENT_VOICES: Classical Islamic jurists and mainstream religious institutions, who would emphasize state authority for declaring jihad, strict rules of engagement, and non-combatant immunity, are actively suppressed or ignored by the revolutionary vanguard.
% DISAPPEARANCE_RATIONALE: If this reading of jihad vanished, the decentralized, immediate, and individually obligatory nature of armed struggle against apostate rulers and occupiers would cease. This would significantly alter the operational landscape for radical groups, forcing a return to more traditional, state-centric, or defensive interpretations, fundamentally reorganizing the nature of religiously motivated conflict.
% FOUNDING_PROBLEM: The perceived apostasy of Muslim rulers and the occupation of Muslim lands by foreign powers, leading to a state of humiliation and injustice for the global Muslim community, which traditional authorities were seen as failing to address.
% FOUNDING_PROBLEM_CORROBORATION: While the revolutionary vanguard leaders themselves attest to the problem's live status, their claims are corroborated by segments of the population experiencing political oppression, foreign intervention, and economic hardship, who feel abandoned by traditional leadership. However, moderate scholars and state authorities dispute the 'apostasy' claim and the legitimacy of bypassing established channels.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because this reading demands total commitment and sacrifice from individuals, while imposing immense costs on target populations, including non-combatants. Suppression (0.88) is also very high, as dissent from this interpretation is often met with severe social pressure, theological condemnation, and even violence within the vanguard's sphere of influence; exit for radicalized individuals is identity-locked. Theater ratio is low (0.15) because the violence is real and directly functional to the stated goal of overthrowing regimes and expelling occupiers, not merely performative. Accessibility collapse is high (0.75) because the theological framing makes alternatives seem illegitimate or cowardly. Resistance is high (0.85) from both state actors and moderate religious authorities, reflecting the intense contestation this reading generates.
 *
 * PERSPECTIVAL GAP:
 *   The revolutionary vanguard leaders perceive this as a necessary, divinely sanctioned struggle for justice and liberation, where the costs are justified by the ultimate reward. Radicalized individuals experience it as a path to spiritual fulfillment and belonging. However, apostate rulers, occupying forces, and especially civilians experience it as pure, indiscriminate extraction and violence. Moderate scholars see it as a dangerous deviation from established Islamic law. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Revolutionary vanguard leaders are beneficiaries (d=0.0-0.1) as they gain power and legitimacy by directing the struggle. Radicalized individuals are both beneficiaries (purpose, belonging) and payers (life, freedom), placing their d closer to symmetric but skewed towards target due to extreme costs. Apostate rulers, occupying forces, and civilians are clear targets (d=0.9-1.0) as they bear the direct, violent costs. Moderate scholars are excluded, bearing costs of delegitimization and threats without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by explicitly identifying the beneficiaries (vanguard leaders, radicalized individuals seeking purpose) and victims (apostate rulers, occupiers, civilians). It highlights how the 'emergency' and 'individual obligation' framing serves to extract resources and sacrifice from a broad base, rather than genuinely coordinating a collective defense under legitimate authority. The high extractiveness and suppression, coupled with the decentralized authority, clearly mark it as a snare, despite its proponents' claims of divine mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    takfir_legitimacy,
    'Is the declaration of ''takfir'' (excommunication) against Muslim rulers legitimate under classical Islamic jurisprudence, or is it an innovation used to justify rebellion?',
    'Comparative textual analysis of classical Sunni and Shia legal opinions on takfir, focusing on conditions and authority for its declaration, and historical precedents for its application against rulers.',
    'If illegitimate, the entire basis for bypassing state authority and declaring individual obligation collapses, reclassifying the constraint towards a snare built on false premises. If legitimate, it strengthens the internal coherence of this reading, though not its ethical implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(takfir_legitimacy, conceptual, 'Legitimacy of takfir as a basis for revolutionary jihad.').

omega_variable(
    emergency_jurisprudence_scope,
    'Does classical Islamic emergency jurisprudence (darura) genuinely permit the suspension of rules regarding non-combatant immunity and state authority in the manner claimed by the revolutionary vanguard?',
    'Examination of the historical application and scholarly consensus on darura, particularly regarding its limits and the conditions under which it can override fundamental ethical principles of warfare.',
    'If the claimed scope of darura is an overreach, the indiscriminate targeting of civilians and the bypassing of state authority lose their jurisprudential cover, exposing the reading as pure extraction. If the scope is genuinely broad, it highlights a structural vulnerability in Islamic law that can be exploited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_jurisprudence_scope, conceptual, 'Scope of emergency jurisprudence in revolutionary jihad.').

omega_variable(
    identity_lock_mechanism,
    'What proportion of ''identity_locked'' exit for radicalized individuals is due to genuine ideological conviction versus social coercion and lack of viable alternatives?',
    'Longitudinal studies of individuals who have exited such groups, examining their post-exit psychological state, social reintegration challenges, and the persistence of ideological beliefs after physical separation from the group.',
    'If a high proportion is due to social coercion/lack of alternatives, the effective suppression and extractiveness are higher than measured, as the ''choice'' to participate is further diminished. If conviction is primary, it points to the power of the ideological framing itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized identity lock for radicalized individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 1979, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1979, 0.1).
narrative_ontology:measurement(jiha_tr_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(jiha_tr_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(jiha_tr_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jiha_be_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1979, 0.75).
narrative_ontology:measurement(jiha_be_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1990, 0.82).
narrative_ontology:measurement(jiha_be_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2001, 0.9).
narrative_ontology:measurement(jiha_be_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2010, 0.95).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1979, 0.7).
narrative_ontology:measurement(jiha_su_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(jiha_su_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2001, 0.85).
narrative_ontology:measurement(jiha_su_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
