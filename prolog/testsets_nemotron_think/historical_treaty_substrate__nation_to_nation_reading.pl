% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historical Treaties as Nation-to-Nation Agreements Requiring Ongoing Consent
 *   domain: legal/indigenous/constitutional
 *
 * SUMMARY:
 *   This constraint story captures the nation-to-nation reading of historical
 *   treaties between Indigenous nations and settler states (primarily Canada,
 *   USA, New Zealand, and Scandinavian states). The reading asserts that
 *   treaties are international agreements between sovereign equals, requiring
 *   ongoing consent for any territorial or jurisdictional changes, and
 *   governed by modern treaty law principles (pacta sunt servanda, good
 *   faith, prohibition on unilateral modification). The claimed type is
 *   'rope' — genuine coordination between sovereigns — but the authored
 *   metrics reflect the reading's own assessment of the standing arrangement:
 *   a framework that promises coordination while enabling asymmetric
 *   extraction through systematic non-compliance by the settler state. The
 *   settler state acts as agenda_setter controlling interpretation, while
 *   Indigenous nations are identity-locked beneficiaries who become payers
 *   when the framework fails. The constraint has persisted for 300+ years
 *   with rising extractiveness and theater as the consent principle is
 *   ritually invoked but substantively ignored.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.45).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.55).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Treaties as Nation-to-Nation Agreements Requiring Ongoing Consent").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal/indigenous/constitutional").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, 'cbe3d3a5-c9b2-45ca-b848-8007f4d47d12').
narrative_ontology:cs_kernel_codification('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', formalized).
narrative_ontology:cs_authority_grounding('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', lineage).
narrative_ontology:cs_interpretation_layer_present('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12').
narrative_ontology:cs_reading_relation('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', foundational, nation_to_nation_sovereignty_preserved).
narrative_ontology:cs_axiom_status(nation_to_nation_sovereignty_preserved, holdable).
narrative_ontology:cs_axiom_grounding('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', nation_to_nation_sovereignty_preserved, deontological).
narrative_ontology:cs_axiom('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', foundational, ongoing_consent_required).
narrative_ontology:cs_axiom_status(ongoing_consent_required, holdable).
narrative_ontology:cs_axiom_grounding('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', ongoing_consent_required, conventional).
narrative_ontology:cs_reference_frame('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', treaty_as_international_agreement).
narrative_ontology:cs_drift_state('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', contemporary_settler_state_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cbe3d3a5-c9b2-45ca-b848-8007f4d47d12', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_corporations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, treaty_as_international_agreement).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, ongoing_consent_principle).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, indigenous_sovereignty_unextinguished).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold treaty rights as co-equal sovereigns; the framework recognizes their consent authority over territorial changes and resource decisions. In practice, their consent is routinely bypassed by the settler state for resource extraction, and they bear the ecological, cultural, and economic costs of violation. Exit from the treaty relationship is identity-locked — sovereignty and nationhood are constituted through the treaty relationship itself, making withdrawal inconceivable without dissolution of the nation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, payer).

% Inherits treaty obligations that legally constrain unilateral action over Indigenous territories; the framework requires ongoing Indigenous consent for territorial changes and resource development. The state administers the treaty interpretation machinery (courts, commissions, negotiation tables) and routinely exercises arbitrage-grade exit by ignoring consent requirements, treating treaties as extinguished, and using domestic law to override treaty terms. The state gains legitimate title and resource access through the treaty framework while evading its consent obligations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Resource extraction companies (mining, forestry, energy) are not treaty parties but benefit structurally from the settler state's unilateral action on treaty lands. They gain access to resources at below-market cost because the state does not obtain Indigenous consent or pay fair compensation. Their exit is mobile — they can shift operations globally — but they lobby to maintain the state's non-compliance because it lowers their costs.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_corporations, beneficiary,
    powerful, biographical, mobile, national).

% UN treaty bodies, the ICJ, the Inter-American Court, and international legal scholars monitor compliance with the nation-to-nation framework. They consistently affirm that treaties are international agreements requiring ongoing consent (e.g., UNDRIP, ICERD jurisprudence), but they lack enforcement power against the settler state. Their role is declaratory and normative, not coercive.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% Future generations who will inherit either intact treaty rights or their erosion. They have no voice in current treaty interpretation, litigation, or negotiation processes, yet they bear the intergenerational consequences of consent violations — lost lands, degraded waters, and diminished cultural continuity. Their exclusion is structural: the treaty framework's current operation does not formally represent future generations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_youth_future_generations, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for peaceful coexistence between sovereign nations, establishing mutual recognition and consent-based territorial governance instead of conquest. The treaty framework solves the coordination problem of how distinct political orders share territory without one subjugating the other.
% TRANSFER_FUNCTION: Moves territorial authority and resource rights from Indigenous nations to settler state conditional on ongoing consent and treaty compliance; when consent is bypassed, moves resources, land, and jurisdiction without compensation or legitimate authority transfer.
% ABSENT_VOICES: Indigenous nations not party to specific treaties (unceded territories), future generations, and non-human entities (land, water, animal nations) that are treaty parties in Indigenous legal orders but excluded from state legal processes. Also absent: Indigenous legal scholars and elders whose interpretations are marginalized in state courts.
% DISAPPEARANCE_RATIONALE: The treaty framework is the legal basis for settler state title in many jurisdictions; its disappearance would destabilize land tenure systems but also remove the primary mechanism that channels Indigenous sovereignty claims into a constrained, state-controlled legal process. Indigenous nations would lose their strongest legal tool for asserting consent rights, but would also be freed from a framework that legitimizes settler occupation while denying its own obligations.
% FOUNDING_PROBLEM: How to establish peaceful relations and legitimate land sharing between Indigenous sovereigns and arriving settler populations without conquest — how to make coexistence lawful rather than imposed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Indigenous oral histories and legal traditions (e.g., Two Row Wampum, Treaty of Niagara, Numbered Treaties oral understandings) and by international law scholars (e.g., S. James Anaya, John Borrows, Karen Engle) from outside the settler state beneficiary set. Settler state courts and governments often endorse the extinguishment reading instead, treating the founding problem as resolved by cession.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).
:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because the treaty framework itself does not directly extract — it establishes a consent gate. The extraction occurs when the settler state bypasses that gate, which the framework fails to prevent. Suppression (0.55) is moderate-high because the treaty framework, as administered by the settler state, suppresses Indigenous alternatives (full sovereignty, Indigenous legal orders, land back) by channeling claims into a domestic legal process that presumes state sovereignty. Theater ratio (0.5) is high because the nation-to-nation rhetoric and consultation processes are increasingly performative — the state goes through motions of consent while the outcome is predetermined. Accessibility collapse (0.6) reflects that alternatives to the treaty framework (e.g., Indigenous self-determination outside state law) are substantially collapsed once the treaty framework is understood as the only legitimate path. Resistance (0.75) is high — Indigenous nations continuously resist through litigation, direct action, international advocacy, and revitalization of Indigenous legal orders.
 *
 * PERSPECTIVAL GAP:
 *   From the Indigenous nation seat (beneficiary/payer, identity_locked), the constraint is experienced as a broken promise — a coordination mechanism that has become a snare because the other party refuses to honor its core term (ongoing consent). From the settler state seat (agenda_setter, arbitrage), the constraint is a manageable obligation that can be interpreted away — the state experiences low effective extraction because it controls enforcement. From the settler corporation seat (beneficiary, mobile), the constraint is a minor friction to be lobbied around. The engine will compute divergent per-seat types from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are declared beneficiaries because the treaty framework's core promise — ongoing consent — structurally benefits them by recognizing their decision-making authority. They are also secondary payers because they bear the costs when that promise is violated (ecological damage, cultural loss, economic marginalization). The settler state is the agenda_setter because it controls the interpretation and enforcement machinery (courts, negotiation mandates, legislative override). The state has arbitrage-grade exit: it can ignore treaty obligations with minimal consequence. Settler corporations are beneficiaries of the state's non-compliance — they extract resource value without paying the consent price. International observers are analytical seats with no stake in the extraction. Future generations are excluded and trapped — they inherit the consequences without voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The treaty framework was built to solve the founding problem of just coexistence. That problem remains live (contested status) — Indigenous nations assert it is unresolved; settler states often claim it was solved by cession. The framework has not been formally sunsetted; instead, its core term (ongoing consent) has been hollowed out while the form persists. This is not mandatrophy in the classic sense (a completed mission whose structure remains) but a zombie constraint: the coordination function is dead for the settler state but live for Indigenous nations, and the structure persists because it legitimizes the state's title. The mandatrophy_resolved flag is false — the mandate has not been acknowledged as obsolete by the authority structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does the nation-to-nation reading''s structural analysis of the treaty substrate differ from the extinguishment and stewardship readings, and what classification consequences follow?',
    'Comparative constraint story generation for all three readings; cross-reading analysis of beneficiary/victim sets, extractiveness referents, and claimed types.',
    'If the extinguishment reading computes as mountain (completed transaction) or snare (extraction via law), and this reading computes as rope/tangled_rope, the kernel''s true structure is fragmented — no single constraint story captures the treaty substrate. The engine''s multi-reading comparison would reveal whether the contest is about classification or about which constraint actually operates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural divergence across readings of the same kernel.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the nation-to-nation principle (treaties as ongoing international agreements) a genuine natural law of international relations, or a constructed legal argument that benefits identifiable agents?',
    'Test whether the principle operates without active enforcement (mountain) or requires continuous Indigenous resistance and international advocacy to maintain (constructed). If the principle would persist without any party defending it, it approaches mountain; if it collapses without active maintenance, it is constructed.',
    'If mountain, the constraint would require emerges_naturally=true and would trigger false_summit_mountain detection (since beneficiaries are declared). If constructed, the rope/tangled_rope classification stands and the beneficiary declaration is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the nation-to-nation principle is a natural law or a maintained legal construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.55) primarily structural (state courts, police, legislation overriding treaties) or internalized (Indigenous nations accepting state legal process as the only legitimate path)?',
    'Post-resistance suppression trajectory: if suppression persists after Indigenous nations assert non-state legal orders (e.g., Indigenous courts, land back actions), reclassify as partially internalized. Track whether resistance creates new alternatives or merely contests within the suppressed frame.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them into resistance. This would increase effective extraction for the Indigenous nation seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the treaty framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1700, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1700, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement(hist_tr_t1763, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1763, 0.2).
narrative_ontology:measurement(hist_tr_t1800, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(hist_tr_t1867, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1867, 0.35).
narrative_ontology:measurement(hist_tr_t1900, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(hist_tr_t1950, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(hist_tr_t1982, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1982, 0.48).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2000, 0.49).
narrative_ontology:measurement(hist_tr_t2025, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2025, 0.5).

% Extraction over time
narrative_ontology:measurement(hist_be_t1700, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1700, 0.2).
narrative_ontology:measurement(hist_be_t1763, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1763, 0.25).
narrative_ontology:measurement(hist_be_t1800, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(hist_be_t1867, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1867, 0.35).
narrative_ontology:measurement(hist_be_t1900, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(hist_be_t1950, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(hist_be_t1982, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1982, 0.43).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(hist_be_t2025, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1700, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(hist_su_t1763, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1763, 0.35).
narrative_ontology:measurement(hist_su_t1800, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(hist_su_t1867, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1867, 0.5).
narrative_ontology:measurement(hist_su_t1900, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(hist_su_t1950, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(hist_su_t1982, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1982, 0.53).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(hist_su_t2025, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__nation_to_nation_reading, 0.12).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint (nation_to_nation_reading) and its two siblings (extinguishment_reading, stewardship_reading) form a constraint family decomposing the historical_treaty_substrate kernel. Each reading instantiates a different constraint with different ε, beneficiaries, and types. The extinguishment reading likely computes as mountain or snare (completed transaction, extraction via law); the stewardship reading likely computes as rope or scaffold (relational coordination). This reading computes as rope (claimed) with tangled_rope metrics. The family linkage enables contamination analysis: if one reading's purity degrades, the kernel's overall coherence is affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, institutional, 0.3).
constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
