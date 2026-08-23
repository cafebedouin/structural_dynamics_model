% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin at Conceptual Conceivability
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'became_thinkable_reading' of the
 *   digital_money_origin kernel: the claim that digital money emerged when
 *   the concept became technically and institutionally conceivable (1980s
 *   Chaumian ecash, central bank research, ISO standards work), prior to
 *   widespread implementation. The constraint is the definitional boundary
 *   that sets this conceptual moment as the origin, excluding earlier or
 *   parallel digital value-transfer practices (M-Pesa precursors, community
 *   electronic currencies, cryptographic timestamping). The frame coordinates
 *   institutional development but extracts definitional authority, rendering
 *   alternative monetary ontologies invisible or 'pre-digital.'
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.65).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.55).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin at Conceptual Conceivability").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '6cf733f7-e147-4f26-a632-212efcba692b').
narrative_ontology:cs_kernel_codification('6cf733f7-e147-4f26-a632-212efcba692b', distributed).
narrative_ontology:cs_authority_grounding('6cf733f7-e147-4f26-a632-212efcba692b', expertise).
narrative_ontology:cs_interpretation_layer_present('6cf733f7-e147-4f26-a632-212efcba692b').
narrative_ontology:cs_reading_relation('6cf733f7-e147-4f26-a632-212efcba692b', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cf733f7-e147-4f26-a632-212efcba692b', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('6cf733f7-e147-4f26-a632-212efcba692b', foundational, conceptual_priority_over_practice).
narrative_ontology:cs_axiom_status(conceptual_priority_over_practice, holdable).
narrative_ontology:cs_axiom_grounding('6cf733f7-e147-4f26-a632-212efcba692b', conceptual_priority_over_practice, conventional).
narrative_ontology:cs_axiom('6cf733f7-e147-4f26-a632-212efcba692b', foundational, institutional_conceivability_as_origin).
narrative_ontology:cs_axiom_status(institutional_conceivability_as_origin, holdable).
narrative_ontology:cs_axiom_grounding('6cf733f7-e147-4f26-a632-212efcba692b', institutional_conceivability_as_origin, conventional).
narrative_ontology:cs_reference_frame('6cf733f7-e147-4f26-a632-212efcba692b', technical_institutional_conceivability_framework).
narrative_ontology:cs_drift_state('6cf733f7-e147-4f26-a632-212efcba692b', contemporary_crypto_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6cf733f7-e147-4f26-a632-212efcba692b', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_bank_researchers).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, standards_bodies).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_digital_cash_theorists).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, excluded_monetary_ontologies).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, community_currency_practitioners).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, non_western_monetary_traditions).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, crypto_anarchist_framings).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, informal_economy_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, mainstream_economists).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, digital_money_users).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, technical_conceivability_precedes_implementation).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, institutional_frameworks_define_monetary_possibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central bank researchers, BIS/ISO standards bodies, and early digital cash theorists (Chaum, etc.) who defined the technical and institutional parameters of what digital money could be. They authored the conceptual framework that later implementers had to inhabit, capturing epistemic authority and setting the agenda for regulatory recognition.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, early_institutional_architects, beneficiary).

% Research divisions at major central banks (Fed, ECB, BoE, BoJ) that funded and directed early digital money research. They benefit from the 'thinkable' frame because it positions central banks as the natural stewards of digital money's evolution, legitimizing CBDC research agendas decades later.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_bank_researchers, beneficiary,
    institutional, biographical, mobile, national).

% ISO TC68, W3C, and other standards bodies that codified the technical specifications for digital money. Their standards become the de facto definition, creating path dependency that rewards their institutional position and makes alternative technical framings costly to pursue.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, standards_bodies, beneficiary,
    organized, generational, constrained, global).

% Academic cryptographers and computer scientists (Chaum, Ferguson, Brands, etc.) who developed the foundational cryptographic primitives. They benefit from the frame because it canonizes their work as the 'origin' of digital money, securing citation legacy and intellectual property recognition.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_digital_cash_theorists, beneficiary,
    organized, biographical, mobile, global).

% Practitioners and theorists of alternative monetary forms — community currencies, mutual credit systems, indigenous gift economies, Islamic finance models — whose frameworks don't map onto the 'technically and institutionally conceivable' criteria. They pay by having their monetary practices rendered invisible or 'pre-digital' by the dominant frame, losing legitimacy and policy access.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, excluded_monetary_ontologies, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, excluded_monetary_ontologies, excluded).

% Operators of LETS, time banks, and local currencies who find their digital implementations forced into the dominant technical frame (blockchain, CBDC, or commercial platform models) rather than being recognized as digital money on their own terms. They bear compliance costs and epistemic marginalization.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, community_currency_practitioners, payer,
    moderate, biographical, constrained, local).

% Monetary systems grounded in non-Western ontologies (e.g., African mobile money as social infrastructure, Latin American comunidad savings circles, Pacific Island gift economies) that the 'technical/institutional conceivability' frame categorizes as 'not yet digital' or 'informal,' excluding them from the origin narrative and its attendant resources.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, non_western_monetary_traditions, excluded,
    powerless, civilizational, identity_locked, continental).

% Cypherpunk and crypto-anarchist communities who developed alternative origin narratives (digital money as cryptographic liberation from institutions). They are excluded from the institutional 'thinkable' frame and must constantly translate their work into institutional terms to gain recognition, paying a permanent epistemic tax.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, crypto_anarchist_framings, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, crypto_anarchist_framings, excluded).

% Billions of people using mobile money, hawala, rotating savings circles, and other digital-adjacent value transfer systems who are rendered 'unbanked' or 'pre-digital' by the frame. They pay through financial exclusion, higher transaction costs, and policy neglect because their practices don't count as 'digital money' under the institutional definition.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, informal_economy_participants, payer,
    powerless, immediate, trapped, local).

% Academic economists who adopt the 'thinkable' frame as the neutral baseline for modeling digital money. They benefit from a stabilized object of study, clear publication venues, and policy relevance — but only within the frame's boundaries.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, mainstream_economists, beneficiary,
    organized, biographical, mobile, global).

% End users of digital payment systems (Apple Pay, Alipay, CBDC pilots, stablecoins) who inherit the conceptual frame's constraints: limited interoperability, surveillance architecture, and no voice in the definitional boundaries that shape their financial lives.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, digital_money_users, payer,
    moderate, biographical, constrained, global).

% Financial regulators (FATF, national FSAs, treasury departments) who later adopt the 'thinkable' frame as the basis for regulatory perimeters. They enforce the boundary by licensing only entities that fit the institutional conception, making the frame legally binding.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, regulatory_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Developers of complementary currencies, crypto-localism projects, and post-capitalist monetary experiments who must either conform to the dominant technical frame (blockchain, token standards) or operate in legal grey zones. Their exclusion is active — the frame defines them as 'not real digital money.'
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, alternative_currency_practitioners, excluded,
    moderate, biographical, constrained, regional).

% Scholars (Maurer, Nelms, Swartz, etc.) who study money as a social technology across cultures. They observe the 'thinkable' frame as a historically specific construction, documenting what it includes and excludes, but lack institutional power to shift the boundary.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared conceptual and technical framework that allows diverse institutions (central banks, standards bodies, commercial firms) to coordinate on digital money development without negotiating fundamental ontological disagreements at every step.
% TRANSFER_FUNCTION: Moves definitional authority, epistemic legitimacy, research funding, regulatory recognition, and path-dependent technical standards from a pluralistic field of monetary practices to a narrow technical/institutional frame controlled by early architects.
% ABSENT_VOICES: Community currency practitioners, non-Western monetary traditions, crypto-anarchist framings, informal economy participants, and alternative currency developers — all of whom would contest the claim that digital money's origin lies in institutional conceivability rather than in diverse practices of digital value transfer. They are structurally excluded because the frame defines them as 'not yet digital' or 'outside the perimeter.'
% DISAPPEARANCE_RATIONALE: If the 'thinkable' constraint vanished, the definitional boundary of digital money would shift to include mobile money (M-Pesa), community digital currencies, crypto protocols, and indigenous value-transfer systems as co-equal origins. This would redistribute epistemic authority, regulatory recognition, and research funding away from the current institutional architects toward a pluralistic field.
% FOUNDING_PROBLEM: The need for a shared conceptual framework to coordinate digital money development across institutions in the 1980s-1990s, when cryptographic primitives existed but no interoperable institutional architecture did.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians outside the central banking establishment (e.g., anthropologists of money like Bill Maurer and Lana Swartz, heterodox economists like Geoffrey Ingham, and historians of technology like Finn Brunton) corroborate that the coordination need was real but argue the specific frame that emerged was one contested outcome among many, not a natural necessity. The central banking establishment attests the problem remains live.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the real material consequences of definitional capture: regulatory perimeters, research funding, standards compliance costs, and financial inclusion/exclusion all track this boundary. Suppression (0.55) is moderate — alternatives aren't banned but are structurally marginalized (no regulatory sandbox access, no ISO working group seats, 'informal' labeling). Theater ratio (0.35) acknowledges genuine coordination work (cryptographic standards, interoperability specs) while noting growing performative maintenance (CBDC pilot frameworks that replicate the frame without solving new problems). Accessibility collapse (0.58) and resistance (0.45) reflect that alternatives persist but require constant translation effort.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional architect seat, the constraint is a genuine coordination achievement (rope-like) that solved the 'interoperability of meaning' problem. From the excluded practitioner seat, it's an epistemic enclosure (snare-like) that defined their practices out of existence. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) captures the structural reality that both coordination and extraction are real and simultaneous.
 *
 * DIRECTIONALITY LOGIC:
 *   Early institutional architects (central bank researchers, standards bodies, digital cash theorists) are structural beneficiaries (d ~ 0.15) — they set the frame and collect epistemic rents. Excluded monetary ontologies (community currencies, non-Western traditions, crypto-anarchists, informal economy users) are structural targets (d ~ 0.85) — they bear translation costs, exclusion effects, and epistemic marginalization. Mainstream economists and digital money users sit near symmetric (d ~ 0.45-0.55) — they get coordination benefits but inherit frame constraints. Regulatory bodies are late-stage agenda_setters (d ~ 0.2) who enforce the frame after it's established.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inter-institutional coordination on digital money architecture) was real in the 1980s-90s but is now contested: the coordination infrastructure exists, yet the frame persists and expands (CBDC frameworks, FATF guidance) capturing new domains. This is classic mandatrophy — the arrangement outlives its founding coordination function and becomes a vehicle for institutional self-perpetuation. The constraint prevents mislabeling this as pure coordination (rope) by documenting the asymmetric extraction that grew alongside the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_naturalness_vs_construction,
    'Is the ''technically and institutionally conceivable'' boundary a natural epistemic limit (digital money literally could not exist before this conceptual breakthrough) or a constructed frame that selected one lineage from a plural field?',
    'Counterfactual historical analysis: were there viable digital money implementations (e.g., M-Pesa 2007, but also earlier stored-value cards, community electronic currencies, cryptographic timestamping services) that functioned as digital money before or outside the ''thinkable'' frame? If yes, the boundary is constructed.',
    'If natural limit, the constraint is closer to mountain (epistemic necessity). If constructed, it is a tangled_rope or snare that actively suppresses alternative origin narratives to maintain institutional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_naturalness_vs_construction, conceptual, 'Whether the ''thinkable'' boundary reflects epistemic necessity or institutional construction.').

omega_variable(
    definitional_capture_extent,
    'How much of the measured extraction stems from active definitional capture (early architects shaping standards/regulation to favor their lineage) versus emergent path dependency (the frame won because it worked better)?',
    'Process tracing of standards body minutes (ISO TC68, W3C), central bank research agendas, and regulatory sandboxes 1985-2020: identify moments where alternative framings were actively excluded vs. passively not adopted.',
    'High active capture -> snare/tangled_rope with strong enforcement. Low active capture -> rope/scaffold where extraction is emergent. Affects mandatrophy assessment: active capture implies intentional perpetuation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definitional_capture_extent, empirical, 'Degree of intentional vs. emergent definitional capture by early architects.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''digital_money_origin'' admit a single coherent framing, or are there multiple defensible framings (technical, social, legal, anthropological) that produce different constraint boundaries?',
    'Compare the three declared readings'' structural implications: if each reading produces a different beneficiary/victim structure and different ε, the kernel is underdetermined and the frame choice is political, not analytical.',
    'If underdetermined, the ''thinkable'' frame''s claim to neutrality is false — it is one political choice among others. This supports the tangled_rope classification (coordination + extraction) over mountain (neutral fact).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is framable in multiple structurally distinct ways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dm_origin_thinkable_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dm_origin_thinkable_tr_t8, digital_money_origin__became_thinkable_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(dm_origin_thinkable_tr_t16, digital_money_origin__became_thinkable_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(dm_origin_thinkable_tr_t24, digital_money_origin__became_thinkable_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(dm_origin_thinkable_tr_t32, digital_money_origin__became_thinkable_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(dm_origin_thinkable_tr_t40, digital_money_origin__became_thinkable_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(dm_origin_thinkable_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dm_origin_thinkable_be_t8, digital_money_origin__became_thinkable_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(dm_origin_thinkable_be_t16, digital_money_origin__became_thinkable_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(dm_origin_thinkable_be_t24, digital_money_origin__became_thinkable_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(dm_origin_thinkable_be_t32, digital_money_origin__became_thinkable_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(dm_origin_thinkable_be_t40, digital_money_origin__became_thinkable_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(dm_origin_thinkable_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dm_origin_thinkable_su_t8, digital_money_origin__became_thinkable_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(dm_origin_thinkable_su_t16, digital_money_origin__became_thinkable_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(dm_origin_thinkable_su_t24, digital_money_origin__became_thinkable_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(dm_origin_thinkable_su_t32, digital_money_origin__became_thinkable_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(dm_origin_thinkable_su_t40, digital_money_origin__became_thinkable_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_origin__became_thinkable_reading, 0.03).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, cbdc_regulatory_perimeter).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, stablecoin_definition_frame).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, financial_inclusion_metrics).

% DUAL FORMULATION NOTE:
% This constraint decomposes the 'digital money origin' concept from the 'first_held_reading' (practical use) and 'regulatory_recognition_reading' (formal incorporation). The became_thinkable_reading has the earliest origin date (conceptual priority) and highest extractiveness (definitional capture precedes implementation). The first_held_reading shifts beneficiaries to early adopters/communities and victims to late institutionalizers. The regulatory_recognition_reading shifts beneficiaries to regulators and victims to unregulated innovators. All three form a constraint family linked by shared kernel but distinct ε and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__became_thinkable_reading, institutional, 0.15).
constraint_indexing:directionality_override(digital_money_origin__became_thinkable_reading, powerless, 0.85).
constraint_indexing:directionality_override(digital_money_origin__became_thinkable_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
