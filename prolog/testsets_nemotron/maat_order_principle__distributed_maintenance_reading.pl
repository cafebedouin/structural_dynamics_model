% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Distributed Ma'at Maintenance Responsibility
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint models the distributed_maintenance_reading of the
 *   maat_order_principle kernel — the position that Ma'at (cosmic order,
 *   truth, justice) is sustained through the proper conduct of every actor in
 *   their station, from Pharaoh to peasant, with no single privileged
 *   interpreter. Authority to interpret Ma'at's requirements rests with those
 *   who demonstrably maintain it: priesthoods who perform rituals correctly,
 *   nomarchs who administer justly, household heads who fulfill familial
 *   duties. This reading draws on Middle Kingdom wisdom literature (Satire of
 *   the Trades, Teaching of Amenemope) that presents Ma'at as a distributed
 *   practice rather than a royal monopoly. The claimed type is rope: a
 *   genuine coordination mechanism solving the problem of social cohesion in
 *   a pre-bureaucratic state, with minimal coercive overhead and broad
 *   participation as net beneficiaries.
 *
 * KEY AGENTS:
 *   - pharaoh_as_first_servant: Primary coordinator (institutional/identity_locked) — embodies Ma'at but is accountable to it; cannot violate it without cosmic consequence
 *   - priesthood_interpreters: Ritual maintainers (organized/constrained) — mediate divine will through correct performance; derive status from demonstrated ritual competence
 *   - regional_nomarchs: Administrative maintainers (powerful/constrained) — implement Ma'at as justice and resource distribution in their nomes; legitimacy depends on visible maintenance outcomes
 *   - commoner_households: Distributed maintainers (moderate/mobile) — sustain Ma'at through proper familial, economic, and ritual conduct in their station; exit exists through migration or station change
 *   - foreign_powers: Excluded observers (powerful/arbitrage) — outside the Ma'at framework entirely; their order (isfet) is the constitutive outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.12).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.25).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Distributed Ma'at Maintenance Responsibility").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '66a0be21-e360-42f9-8598-8f57292e6235').
narrative_ontology:cs_kernel_codification('66a0be21-e360-42f9-8598-8f57292e6235', distributed).
narrative_ontology:cs_authority_grounding('66a0be21-e360-42f9-8598-8f57292e6235', practice).
narrative_ontology:cs_interpretation_layer_present('66a0be21-e360-42f9-8598-8f57292e6235').
narrative_ontology:cs_reading_relation('66a0be21-e360-42f9-8598-8f57292e6235', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('66a0be21-e360-42f9-8598-8f57292e6235', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('66a0be21-e360-42f9-8598-8f57292e6235', foundational, maat_authority_from_demonstrated_maintenance).
narrative_ontology:cs_axiom_status(maat_authority_from_demonstrated_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('66a0be21-e360-42f9-8598-8f57292e6235', maat_authority_from_demonstrated_maintenance, conventional).
narrative_ontology:cs_axiom('66a0be21-e360-42f9-8598-8f57292e6235', foundational, station_proportional_obligation_universal).
narrative_ontology:cs_axiom_status(station_proportional_obligation_universal, holdable).
narrative_ontology:cs_axiom_grounding('66a0be21-e360-42f9-8598-8f57292e6235', station_proportional_obligation_universal, deontological).
narrative_ontology:cs_reference_frame('66a0be21-e360-42f9-8598-8f57292e6235', middle_kingdom_wisdom_practice).
narrative_ontology:cs_drift_state('66a0be21-e360-42f9-8598-8f57292e6235', new_kingdom_imperial_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('66a0be21-e360-42f9-8598-8f57292e6235', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, priesthood_interpreters).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, regional_nomarchs).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, commoner_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, pharaoh_as_first_servant).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, regional_nomarchs).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, distributed_cosmic_accountability).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, station_proportional_maat_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pharaoh is the supreme ritual and administrative actor whose every action must exemplify Ma'at. He commissions temples, leads festivals, appoints officials, and personifies the state's cosmic alignment. His station-proportional obligation is total — failure brings cosmic disorder (isfet) and loss of legitimacy. He cannot exit the role; his identity is fused with the office. He bears the heaviest maintenance burden (payer) while also setting the agenda for how Ma'at is enacted at scale (agenda_setter).
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh_as_first_servant, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh_as_first_servant, payer).

% Temple priesthoods maintain the ritual dimension of Ma'at through daily offerings, festivals, and oracular consultation. Their authority derives from demonstrated ritual competence — correct performance maintains cosmic order; error risks isfet. They receive material support (land, offerings, tax exemption) and social prestige as beneficiaries of the coordination function. Exit is constrained: priesthood is hereditary and requires specialized training; leaving means losing ritual authority and temple income.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priesthood_interpreters, beneficiary,
    organized, generational, constrained, regional).

% Nomarchs govern nomes (provinces) as the Pharaoh's delegates. Their Ma'at-obligation is to administer justice, maintain irrigation, collect taxes fairly, and suppress disorder. Legitimacy depends on visible maintenance outcomes — a nomarch whose nome prospers demonstrates Ma'at; one whose nome suffers famine or revolt has failed. They benefit from delegated authority and local prestige (beneficiary) but bear heavy administrative burdens and personal risk (payer). Exit is constrained: office is often hereditary, and failure means replacement or death.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, regional_nomarchs, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, regional_nomarchs, payer).

% Household heads (mostly male, some female) maintain Ma'at through proper familial conduct: honoring ancestors, educating children, fair dealing in markets, fulfilling corvée labor, and observing ritual purity. Their station-proper conduct is cosmically efficacious — a well-ordered household contributes to cosmic order. They benefit from social stability, cosmic alignment, and community standing (beneficiary). Exit is relatively mobile: migration to another nome, station change through apprenticeship or marriage, or (in extremis) flight to desert/foreign lands — though this abandons the Ma'at framework entirely.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, commoner_households, beneficiary,
    moderate, biographical, mobile, local).

% Neighboring states (Nubia, Libya, Levant, Hatti) operate outside the Ma'at framework entirely. Their ordering principle is isfet (disorder) from the Egyptian perspective. They would object to Egyptian claims of universal cosmic order but are structurally excluded from the Ma'at conversation. Their exit option is arbitrage: they can engage diplomatically, trade, or conquer without recognizing Ma'at's authority. Their exclusion is constitutive — Ma'at defines itself against the foreign/chaotic.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, foreign_powers, excluded,
    powerful, biographical, arbitrage, global).

% Modern scholar analyzing the Ma'at system across its three readings. Sees the full structural contestation: how each reading serves different power configurations, how the distributed reading minimizes extraction but requires high social trust, how the divine_mandate reading centralizes authority at extraction cost, how the reciprocity reading balances mutual obligation with Pharaonic primacy. Neither collects nor pays; observes the constraint family's dynamics.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a stratified, pre-bureaucratic society around a shared cosmic order without centralized enforcement: each actor's station-proper conduct (ritual, administrative, familial) is both locally meaningful and cosmically efficacious, creating alignment from Pharaoh to peasant through distributed accountability rather than top-down command.
% TRANSFER_FUNCTION: Moves ritual performance, administrative justice, and familial duty from every station upward as Ma'at-maintenance; moves cosmic alignment, social legitimacy, and material stability downward as the returns of participation. No single extraction vector — the flow is circular and station-proportional.
% ABSENT_VOICES: The isfet-constituted: those whose station is defined by exclusion from Ma'at (criminals, rebels, the chaotic dead, foreign enemies). They would object to the framework that defines them as disorder, but their exclusion is what makes Ma'at coherent. Also absent: women's voices in the wisdom literature — the distributed model's 'commoner' is gendered male in the textual record, though women maintained household Ma'at in practice.
% DISAPPEARANCE_RATIONALE: If distributed Ma'at maintenance vanished overnight, the coordination mechanism binding Pharaoh, priesthood, nomarchs, and commoners into a single cosmic order would collapse. The Old Kingdom divine_mandate model might reassert (Pharaoh as sole embodiment), or the system might fragment into warlordism (isfet). Egyptian civilization's specific form of cohesion — station-proportional cosmic accountability — would be lost; something else would replace it.
% FOUNDING_PROBLEM: How to coordinate a geographically extended, socially stratified society with minimal bureaucratic infrastructure, using cosmic order as the alignment mechanism instead of coercive enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Middle Kingdom wisdom texts (Teaching of Amenemope, Satire of the Trades) attest the problem as live — they address a society where station-proper conduct is still the primary coordination mechanism. The problem's persistence is corroborated by the distributed reading's operational longevity (Middle Kingdom through Late Period) and its reappearance in post-New Kingdom restoration theology. No external non-beneficiary source attests the problem is dead; the divine_mandate and reciprocity readings contest the SOLUTION, not the problem's existence.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'no_scope_rebuild_nemotron+seed_rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint operates through distributed participation rather than centralized extraction — each station's Ma'at-maintenance is its own reward (cosmic alignment, social legitimacy). Suppression is low (0.25) because alternatives are not actively crushed; isfet (disorder) is the natural consequence of non-participation, not a state punishment. Theater ratio is low (0.15) — ritual and administrative performance are the substance of maintenance, not theater. Accessibility collapse is moderate (0.45) — the Ma'at framework is cognitively pervasive in Egyptian thought, but the distributed model explicitly permits multiple legitimate interpreters. Resistance is moderate (0.35) — the system faces contestation from the divine_mandate_reading (royal monopoly) and from periods of centralization where Pharaoh reasserts sole interpretive authority.
 *
 * PERSPECTIVAL GAP:
 *   The priesthood seat experiences this as coordination (they maintain rituals that structure society); the nomarch seat experiences it as accountability (they must demonstrate just administration to retain legitimacy); the commoner seat experiences it as participation (their proper conduct is cosmically efficacious). The engine computes these divergent seat types from the structural data: priesthood and nomarchs as beneficiaries of the coordination function, commoners as symmetric participants. The pharaoh seat is unique — structurally the highest-power actor but directionality d ≈ 0.5 (symmetric) because the distributed reading makes Pharaoh the 'first servant' of Ma'at, not its owner.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: priesthood_interpreters (ritual authority grounded in demonstrated maintenance), regional_nomarchs (administrative legitimacy from Ma'at-performance), commoner_households (cosmic alignment through station-proper conduct). No victims declared — the distributed reading has no structural extraction target; non-participation yields isfet (disorder) as natural consequence, not imposed penalty. Pharaoh is neither beneficiary nor victim in this reading — the first servant bears the heaviest maintenance burden with the least exit (identity_locked), but this is station-proportional obligation, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The distributed reading avoids mandatrophy by making the coordination function (cosmic/social order maintenance) identical to the constraint's operation — there is no separate 'mandate' that could atrophy. The founding problem (how to coordinate a stratified society without bureaucratic enforcement) remains live as long as the society exists. The constraint persists because it solves a genuine coordination problem at minimal overhead; it does not persist by inertia or theater. If the coordination problem dissolves (state collapse, foreign conquest), the constraint dissolves with it — no zombie maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the distributed_maintenance_reading of the maat_order_principle kernel. How does this reading''s structural claim of distributed interpretive authority differ from the divine_mandate_reading (Pharaoh as sole Ma''at embodiment) and reciprocity_reading (mutual obligations with Pharaoh as primary guarantor)?',
    'Comparative analysis of Middle Kingdom textual corpora: the Satire of the Trades (distributed station duty), the Prophecy of Neferti (divine mandate collapse/restoration), and the Loyalist Teaching (reciprocal loyalty). Each reading weights different texts as normative.',
    'If distributed_maintenance_reading is the empirically dominant operational model of Ma''at in Middle Kingdom administration, its lower extraction profile is descriptive. If divine_mandate_reading reflects actual state ideology, the distributed reading is an analytical idealization. The classification divergence between readings measures the kernel''s contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Structural relationship between this reading and its sibling readings of the maat_order_principle kernel').

omega_variable(
    maat_extraction_measurement,
    'Does the distributed maintenance reading genuinely produce lower extraction, or does the diffusion of interpretive authority create a different extraction vector (e.g., priesthood rent-seeking through ritual gatekeeping)?',
    'Archaeological analysis of temple economic records (Wilbour Papyrus, temple day-books) for evidence of priesthood extraction vs. state extraction under each reading''s operational period.',
    'If priesthood intermediation extracts comparably to royal extraction, the distributed reading''s low ε is an artifact of attribution displacement, not genuine coordination efficiency. Classification would shift toward tangled_rope (coordination + asymmetric extraction via ritual monopoly).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maat_extraction_measurement, empirical, 'Whether distributed interpretive authority actually reduces extraction or displaces it to religious intermediaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(maat_tr_t0, observed).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__distributed_maintenance_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(maat_tr_t100, observed).
narrative_ontology:measurement(maat_tr_t200, maat_order_principle__distributed_maintenance_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement_basis(maat_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(maat_be_t0, observed).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement_basis(maat_be_t100, observed).
narrative_ontology:measurement(maat_be_t200, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 200, 0.12).
narrative_ontology:measurement_basis(maat_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(maat_su_t0, observed).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 100, 0.28).
narrative_ontology:measurement_basis(maat_su_t100, observed).
narrative_ontology:measurement(maat_su_t200, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 200, 0.25).
narrative_ontology:measurement_basis(maat_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__distributed_maintenance_reading, 0.08).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the maat_order_principle constraint family. Each reading instantiates a different structural claim about how Ma'at operates: distributed_maintenance (this reading, rope, ε≈0.12), divine_mandate (mountain-claim with false_summit risk, ε≈0.35), reciprocity (tangled_rope, ε≈0.45). The distributed reading is the operational model of Middle Kingdom wisdom literature; the divine_mandate reading is the Old Kingdom royal ideology; the reciprocity reading is the New Kingdom restoration theology. They contest the same kernel with different extraction profiles and authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, institutional, 0.55).
constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, organized, 0.3).
constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, powerful, 0.35).
constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
