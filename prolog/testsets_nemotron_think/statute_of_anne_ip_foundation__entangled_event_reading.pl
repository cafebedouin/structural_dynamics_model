% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne as Entangled Conceptual-Institutional Event (1710)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The 1710 Statute of Anne is conventionally read either as the birth of
 *   modern copyright (conceptual emergence) or as a transfer of monopoly from
 *   Stationers to authors (institutional reallocation). The entangled-event
 *   reading holds these are inseparable: the statute's single legislative act
 *   simultaneously made 'literary property' thinkable as a limited state
 *   grant AND instantiated the institutional machinery (registration,
 *   deposit, term limits, assigns clause) that gave it force. The conceptual
 *   category 'copyright' and the institutional form 'statutory monopoly'
 *   co-constituted each other in 1710. This fusion benefited London
 *   publishers practically (they held the assigns) while naming authors
 *   nominally; the victim was conceptual clarity — the possibility of
 *   discussing literary property apart from state monopoly was foreclosed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.68).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.55).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne as Entangled Conceptual-Institutional Event (1710)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, 'baf273d2-5b77-4356-a293-c59f81a9d7f6').
narrative_ontology:cs_kernel_codification('baf273d2-5b77-4356-a293-c59f81a9d7f6', formalized).
narrative_ontology:cs_authority_grounding('baf273d2-5b77-4356-a293-c59f81a9d7f6', lineage).
narrative_ontology:cs_interpretation_layer_present('baf273d2-5b77-4356-a293-c59f81a9d7f6').
narrative_ontology:cs_reading_relation('baf273d2-5b77-4356-a293-c59f81a9d7f6', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('baf273d2-5b77-4356-a293-c59f81a9d7f6', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('baf273d2-5b77-4356-a293-c59f81a9d7f6', foundational, conceptual_institutional_entanglement_necessary).
narrative_ontology:cs_axiom_status(conceptual_institutional_entanglement_necessary, holdable).
narrative_ontology:cs_axiom_grounding('baf273d2-5b77-4356-a293-c59f81a9d7f6', conceptual_institutional_entanglement_necessary, conventional).
narrative_ontology:cs_axiom('baf273d2-5b77-4356-a293-c59f81a9d7f6', secondary, assigns_clause_enables_publisher_capture).
narrative_ontology:cs_axiom_status(assigns_clause_enables_publisher_capture, holdable).
narrative_ontology:cs_axiom_grounding('baf273d2-5b77-4356-a293-c59f81a9d7f6', assigns_clause_enables_publisher_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('baf273d2-5b77-4356-a293-c59f81a9d7f6', statute_of_anne_1710_entangled_event).
narrative_ontology:cs_drift_state('baf273d2-5b77-4356-a293-c59f81a9d7f6', donaldson_v_beckett_1774, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('baf273d2-5b77-4356-a293-c59f81a9d7f6', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, london_publishers_stationers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, parliament_crown).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal_rights_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, reading_public_conceptual_clarity).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, competing_philosophical_frameworks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal_rights_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, london_publishers_stationers).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, copyright_as_limited_statutory_grant).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, learning_promotion_as_legitimate_state_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1710 statute to break the Stationers' perpetual monopoly while creating a new statutory copyright. The Crown gained a regulatory lever over print; Parliament claimed to serve 'the encouragement of learning.' The statute's dual character — conceptual innovation and institutional reallocation — served state interests by legitimizing a new property form that could be calibrated.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament_crown, agenda_setter,
    institutional, generational, analytical, national).

% The Stationers' Company lost its perpetual common-law monopoly but gained a statutory 14+14 year term they could enforce through courts they knew. Practically, they continued to control the trade by holding authors' assigned copyrights. They paid compliance costs (registration, deposit) but captured the lion's share of the new property's economic value. Their exit from the old perpetual system was blocked by the statute itself.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, london_publishers_stationers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, london_publishers_stationers, payer).

% Formally granted copyright for the first time, but in practice had to assign it to publishers to reach markets. The statute named them as beneficiaries ('authors and their assigns') but the institutional economics funneled value to assignees. Their exit option — self-publishing — was theoretically open but practically blocked by the Stationers' distribution network and the capital requirements of print.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal_rights_holders, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal_rights_holders, beneficiary).

% Lost the conceptual clarity of the pre-statute world where 'copy' meant a physical manuscript copy held by the Stationers. The statute fused 'property in ideas' with 'regulation of printing' in a single gesture, making it impossible to discuss literary property without invoking the state's monopoly grant. The public's access to knowledge became contingent on a property form that claimed to serve learning but operated as trade regulation.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, reading_public_conceptual_clarity, payer,
    powerless, generational, trapped, national).

% Natural-law property theorists (like later Hargrave arguing for perpetual common-law copyright) and utilitarian reformers (like later Bentham) were both excluded from the statute's founding moment. The statute's entangled character — simultaneously creating a new concept and a new institution — meant no pure philosophical position could be cleanly mapped onto it. Later disputes (Donaldson v Beckett, 1774) replayed this exclusion.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, competing_philosophical_frameworks, excluded,
    moderate, civilizational, identity_locked, universal).

% From Blackstone to modern IP historians, observers have tried to disentangle the statute's conceptual and institutional dimensions. The entangled-event reading holds that this disentanglement is impossible — the statute's force comes precisely from their fusion. This seat sees the full structure but cannot change it.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, legal_scholars_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a standardized, state-backed property right in literary works that replaced the Stationers' private perpetual registry, enabling a national market in books with defined terms and public domain reversion.
% TRANSFER_FUNCTION: Moves control over literary reproduction from the Stationers' Company's perpetual common-law claim to a time-limited statutory grant vested initially in authors but practically assigned to publishers; moves the 'public domain' from a non-concept (perpetual monopoly) to a positive legal status after term expiry.
% ABSENT_VOICES: Scottish and Irish publishers (outside the Stationers' London monopoly) who saw the statute as English overreach; colonial printers who faced metropolitan copyright extension without representation; working-class readers for whom book prices remained high despite the statute's 'learning' rhetoric.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne vanished in 1710, the Stationers' perpetual common-law claim would have persisted (as it did in Scotland until 1774), the concept of 'limited statutory copyright' would not have entered Anglo-American law, and the entire genealogy of modern IP — including the 1790 US Copyright Act and the Berne Convention — would lack its foundational precedent. The world of literary property would be unrecognizable.
% FOUNDING_PROBLEM: The Stationers' Company held a perpetual, extra-legal monopoly over printing enforced by royal charter and Star Chamber decrees. This monopoly blocked competition, kept prices high, and served Crown censorship. The 'encouragement of learning' required breaking this monopoly without simply nationalizing it.
% FOUNDING_PROBLEM_CORROBORATION: The statute's preamble and parliamentary debates (recorded in the Journal of the House of Commons, 1709-1710) attest the 'learning' justification. The Stationers' own petitions against the bill (British Library, Add MS 4472) attest their perception of lost privilege. Modern historians (Rose, 'Authors and Owners'; Deazley, 'On the Origin of the Right to Copy') corroborate that the statute simultaneously created a new concept and reallocated institutional power — the entanglement is structural, not rhetorical.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at 1774) reflects the growing gap between the statute's 'learning' rhetoric and its operation as a publisher-controlled trade regulation. Suppression (0.55) is moderate — the constraint didn't need total coercion because the assigns clause and distribution economics naturally funneled rights to publishers. Theater ratio (0.42) rises over the interval as the 'encouragement of learning' justification becomes increasingly performative relative to the commercial reality. Accessibility collapse (0.62) is substantial: once the statute fused concept and institution, alternative framings (perpetual natural right, pure regulatory license, commons-based models) became structurally harder to articulate within English law. Resistance (0.48) reflects ongoing but fragmented challenges: Scottish litigation, pamphlet wars, parliamentary reform attempts.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher seat, the statute is a rope (coordination of a chaotic trade). From the author seat, it's a tangled rope (nominal right, practical extraction). From the public seat, it's a snare (monopoly disguised as learning). The engine computes these divergences from the structural data; the entangled-event reading claims the constraint IS the entanglement — no single seat's perception captures it.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament/Crown (agenda_setter, d ~ 0.15) benefits from a new regulatory lever. London publishers (beneficiary, d ~ 0.25) capture economic value despite nominal compliance costs. Authors (payer/beneficiary, d ~ 0.55) are structurally centered — they hold the formal right but must assign it. The reading public (payer, d ~ 0.85) is trapped: the fused concept/institution makes 'access to knowledge' depend on monopoly pricing. Competing frameworks (excluded, d ~ 0.75) are identity-locked into their philosophical positions and cannot exit the statute's framing. Observers (analytical, d = 0.5) see the structure symmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Stationers' perpetual monopoly) was live in 1710. By 1774 (Donaldson v Beckett), the House of Lords confirmed the statute's limited term, declaring the common-law perpetual right extinguished. The founding problem was 'solved' — but the statute's entangled structure persisted, becoming the template for all subsequent IP expansion. The mandatrophy is unresolved: the constraint outlived its founding justification and became the vehicle for expanding the very monopoly form it was meant to replace.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_institutional_separability,
    'Could the Statute of Anne have created the institutional machinery (registration, term limits, assigns) WITHOUT creating the conceptual category ''copyright as limited statutory property'', or vice versa?',
    'Counterfactual legal history: examine whether the Stationers'' 1710 petition for a ''bill for the better regulation of the press'' (which sought only institutional renewal of their charter) could have passed without the conceptual innovation. If the bill that passed required BOTH the new concept AND the new institution to secure a parliamentary majority, the entanglement is structurally necessary.',
    'If separable, the entangled-event reading overstates the fusion — one dimension could have occurred without the other. If inseparable, the entanglement is the constraint''s defining structural feature, and the sibling readings are necessarily partial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_institutional_separability, conceptual, 'Whether the statute''s conceptual and institutional dimensions are structurally separable or necessarily fused.').

omega_variable(
    beneficiary_ambiguity_resolution,
    'Does the statute''s ''authors and their assigns'' language reflect genuine ambiguity about who benefits, or a deliberate drafting strategy to name authors while enabling publisher capture?',
    'Legislative history analysis: compare the 1710 bill''s drafting stages (House of Commons committee amendments, Stationers'' lobbying records, author petitions). If ''assigns'' was inserted by publisher allies after author testimony, the ambiguity is strategic. If authors themselves demanded assignability, the ambiguity reflects author intent.',
    'If strategic, the beneficiary structure is a snare-like feature (deliberate misdirection). If author-driven, the tangled rope''s coordination function includes voluntary alienability — authors chose the publisher path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_ambiguity_resolution, empirical, 'Whether the nominal/practical beneficiary split is deliberate obfuscation or emergent structure.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the entangled-event reading''s core premise (conceptual and institutional change are inseparable in the statute) logically foreclose the sibling readings, or do all three readings coexist as live interpretive positions?',
    'Analyze whether a single legal framework could simultaneously hold: (a) the statute created a genuinely new concept, (b) the statute merely reallocated existing rights, and (c) the statute''s force comes from fusing (a) and (b). If (c) entails that (a) and (b) are each incomplete but not false, the readings coexist. If (c) entails that (a) and (b) are category errors, it forecloses them.',
    'If forecloses, the kernel has a dominant reading that structurally displaces alternatives. If coexists_with, the kernel sustains irreducible interpretive pluralism — the constraint family persists without resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the entangled-event reading logically eliminates the sibling readings or coexists with them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statute_anne_entangled_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.25).
narrative_ontology:measurement(statute_anne_entangled_tr_t1720, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1720, 0.3).
narrative_ontology:measurement(statute_anne_entangled_tr_t1735, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1735, 0.35).
narrative_ontology:measurement(statute_anne_entangled_tr_t1750, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1750, 0.38).
narrative_ontology:measurement(statute_anne_entangled_tr_t1760, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1760, 0.4).
narrative_ontology:measurement(statute_anne_entangled_tr_t1774, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1774, 0.42).

% Extraction over time
narrative_ontology:measurement(statute_anne_entangled_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.45).
narrative_ontology:measurement(statute_anne_entangled_be_t1720, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1720, 0.52).
narrative_ontology:measurement(statute_anne_entangled_be_t1735, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1735, 0.58).
narrative_ontology:measurement(statute_anne_entangled_be_t1750, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement(statute_anne_entangled_be_t1760, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1760, 0.65).
narrative_ontology:measurement(statute_anne_entangled_be_t1774, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1774, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(statute_anne_entangled_su_t1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1710, 0.4).
narrative_ontology:measurement(statute_anne_entangled_su_t1720, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1720, 0.45).
narrative_ontology:measurement(statute_anne_entangled_su_t1735, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1735, 0.5).
narrative_ontology:measurement(statute_anne_entangled_su_t1750, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1750, 0.52).
narrative_ontology:measurement(statute_anne_entangled_su_t1760, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1760, 0.54).
narrative_ontology:measurement(statute_anne_entangled_su_t1774, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1774, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__entangled_event_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, donaldson_v_beckett_1774).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, us_copyright_act_1790).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, berne_convention_1886).

% DUAL FORMULATION NOTE:
% This constraint is one member of the statute_of_anne_ip_foundation kernel family. The conceptual_emergence_reading (low extraction, mountain-like) and institutional_reallocation_reading (moderate extraction, rope-like) are sibling constraints. This entangled_event_reading (high extraction, tangled_rope) captures the structural fusion that the siblings separate. All three share the same historical referent (the 1710 Act) but author different ε values because they isolate different structural claims — per ε-invariance, these are distinct constraints linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, organized, 0.25).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, moderate, 0.55).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
