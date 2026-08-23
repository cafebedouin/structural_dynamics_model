% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity: Pharaonic Obligation to Justice and Resource Distribution
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint story represents the reciprocity reading of the Ma'at
 *   kernel: Ma'at as a mutual obligation structure where Pharaoh's legitimacy
 *   and the stability of cosmic order depend on the ruler fulfilling concrete
 *   obligations — justice, stability, proper resource distribution — in
 *   exchange for the populace's support and the gods' favor. The reading
 *   positions Pharaoh as subject to Ma'at rather than its embodiment,
 *   creating a structural ceiling on extractiveness: failed obligations
 *   justify resistance, withdrawal of support, or loss of legitimacy. The
 *   constraint functions as a coordination mechanism (solving the problem of
 *   legitimate authority in a cosmologically ordered society) with a built-in
 *   reciprocity brake that limits pure extraction. Over three millennia, the
 *   reciprocity norm persisted as a cultural invariant even as specific
 *   Pharaohs violated it; the norm's persistence created periodic correction
 *   cycles (Middle Kingdom reforms, New Kingdom restoration rhetoric) rather
 *   than monotonic extraction accumulation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.35).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.45).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity: Pharaonic Obligation to Justice and Resource Distribution").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'f66f5779-8fed-4993-9b1c-f5f0d25d54ed').
narrative_ontology:cs_kernel_codification('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', distributed).
narrative_ontology:cs_authority_grounding('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', lineage).
narrative_ontology:cs_interpretation_layer_present('f66f5779-8fed-4993-9b1c-f5f0d25d54ed').
narrative_ontology:cs_reading_relation('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', foundational, pharaoh_subject_to_maat).
narrative_ontology:cs_axiom_status(pharaoh_subject_to_maat, holdable).
narrative_ontology:cs_axiom_grounding('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', pharaoh_subject_to_maat, deontological).
narrative_ontology:cs_axiom('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', foundational, failed_obligations_justify_resistance).
narrative_ontology:cs_axiom_status(failed_obligations_justify_resistance, holdable).
narrative_ontology:cs_axiom_grounding('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', failed_obligations_justify_resistance, deontological).
narrative_ontology:cs_axiom('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', secondary, reciprocity_ceiling_on_extraction).
narrative_ontology:cs_axiom_status(reciprocity_ceiling_on_extraction, holdable).
narrative_ontology:cs_axiom_grounding('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', reciprocity_ceiling_on_extraction, instrumental).
narrative_ontology:cs_reference_frame('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', old_kingdom_maat_reciprocity).
narrative_ontology:cs_drift_state('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', new_kingdom_imperial_maat, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f66f5779-8fed-4993-9b1c-f5f0d25d54ed', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, egyptian_populace).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, temple_institutions).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, nomarchs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, nomarchs).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, reciprocity_norm).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, cosmic_balance_through_justice).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, pharaonic_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds supreme authority and mediates Ma'at for the realm. Must enact justice, maintain stability (Nile management, border defense, internal order), and oversee resource distribution (granaries, labor allocation, temple endowments). Legitimacy depends on visible reciprocity: sed festivals, ma'at offerings, wisdom literature patronage. Cannot exit the role without cosmic disorder — identity is fused to the office. When reciprocity fails (famine, injustice, military defeat), the norm provides no exit for Pharaoh, only correction (reformation, replacement, or collapse).
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).

% Farmers, laborers, artisans, and their families who receive Pharaonic justice (kenbet courts), stability (corvée labor organized for flood control, not pure extraction), and redistribution (granary access in famine, temple distributions). Their cosmological identity as 'people of Ma'at' is identity_locked — exit means ceasing to be Egyptian in the meaningful sense. They have a structural claim on Pharaoh's reciprocity but limited enforcement power beyond withdrawal of labor or localized resistance. Wisdom literature (Eloquent Peasant, Neferti) articulates their claim.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, egyptian_populace, beneficiary,
    powerless, biographical, identity_locked, local).

% Major temples (Amun at Karnak, Ptah at Memphis, Ra at Heliopolis) receive land grants, labor allocations, and ritual monopoly from Pharaoh in exchange for maintaining the cosmic order through daily ritual. They mediate the reciprocity norm: they certify Pharaoh's ma'at-compliance and distribute resources to the populace. Their exit option is arbitrage — they can shift allegiance between Pharaohs (as in the Amarna period) or between Pharaoh and foreign rulers (Persian, Ptolemaic periods) while maintaining institutional continuity. They are the primary institutional guardians of the reciprocity reading.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, temple_institutions, beneficiary,
    organized, generational, arbitrage, regional).

% Provincial governors who benefit from Pharaonic stability (trade routes, border defense, Nile coordination) and legitimating ideology, but pay through resource transfers to the center and military obligations. Their exit is constrained — they control regional power bases but depend on the national framework for legitimacy. In periods of reciprocity failure (First Intermediate Period, Late Period fragmentation), nomarchs become de facto independent rulers, withdrawing support from Pharaoh. They are the pivot actors: their defection signals reciprocity collapse.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, nomarchs, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, nomarchs, payer).

% Nubian, Libyan, Asiatic, Persian, Greek, and Roman powers who interact with Egypt but are structurally excluded from the Ma'at reciprocity framework. They are not bound by Ma'at obligations nor entitled to its benefits. Their presence creates external pressure: conquest forces either Ma'at adaptation (Ptolemies adopting Pharaonic reciprocity) or framework replacement. They would object to Ma'at as a barrier to extraction but are not in the conversation.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, foreign_powers, excluded,
    powerful, generational, mobile, continental).

% Egyptologists, historians of political thought, and scholars of ancient religion who analyze the Ma'at reciprocity structure from outside. They see the full structural pattern: the reciprocity ceiling on extraction, the identity-lock dynamics, the periodic correction cycles. Their analytical seat computes the constraint type without being subject to it.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of legitimate authority in a cosmologically ordered society: Pharaoh's rule is legitimate iff Pharaoh maintains Ma'at (justice, stability, resource distribution), creating a coordination equilibrium where elite and populace support the center because the center delivers cosmologically-required order.
% TRANSFER_FUNCTION: Moves labor (corvée, military service), grain (taxation, temple offerings), and loyalty (ritual participation, ideological assent) from populace and nomarchs to Pharaoh and temple institutions, in exchange for justice (court access), stability (Nile management, defense), and redistribution (famine relief, public works). The transfer is bounded by the reciprocity norm — if Pharaoh takes without giving, the transfer claim lapses.
% ABSENT_VOICES: Women, enslaved persons, and peripheral populations (oasis dwellers, Sinai miners) are structurally absent from the reciprocity conversation. Wisdom literature and legal texts show their claims on justice were recognized in principle but mediated through male household heads or institutional channels. They would object to the patriarchal and centralizing framing of Ma'at reciprocity but are not seated at the institutional table.
% DISAPPEARANCE_RATIONALE: If the Ma'at reciprocity norm vanished overnight, Pharaonic legitimacy would lose its primary ideological foundation. The center would either collapse into pure coercion (snare transformation), fragment into warlordism (nomarch independence), or be replaced by a foreign legitimacy framework (as occurred in the Late Period). The Egyptian state as a Ma'at-coordinated entity would cease to exist; the world would rearrange.
% FOUNDING_PROBLEM: The founding problem was establishing legitimate, stable authority over a unified Nile Valley society in a cosmological framework where order (Ma'at) is constantly threatened by chaos (isfet). The reciprocity norm solved this by binding the ruler's authority to concrete, observable obligations — justice, stability, resource distribution — that the ruled could evaluate and the gods would enforce.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by Egyptian textual tradition itself: the Pyramid Texts, Coffin Texts, and wisdom literature consistently frame Pharaonic authority as conditional on Ma'at-maintenance. Outside the beneficiary set (Pharaoh and temples), the Middle Kingdom 'Prophecy of Neferti' and 'Admonitions of Ipuwer' — texts produced in periods of reciprocity collapse — attest that the problem of legitimate authority under Ma'at remained live and contested. Modern scholarship (Assmann, Hornung, Allen) corroborates from an analytical seat.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate because the reciprocity norm imposes a genuine ceiling: Pharaoh extracts labor, grain, and loyalty but must deliver ma'at-conform outcomes or face legitimacy collapse. Suppression (0.45) is present but not total — the constraint relies more on normative enforcement (divine retribution, loss of mandate, elite defection) than physical coercion. Theater ratio (0.25) reflects that ritual performance (sed festivals, ma'at offerings) is real but not purely performative; it signals commitment to the reciprocity norm. Accessibility collapse (0.35) is moderate because alternative legitimacy frameworks (local cults, foreign rule, merchant power) remained thinkable. Resistance (0.55) is significant: textual record shows repeated elite and popular pushback when reciprocity was violated (First Intermediate Period collapse, Middle Kingdom reclamation, Amarna period backlash). The claimed type 'rope' reflects the reading's structural judgment: Ma'at-as-reciprocity is a genuine coordination solution with a built-in extraction brake, not a snare or tangled rope.
 *
 * PERSPECTIVAL GAP:
 *   From the populace seat, the constraint feels like a rope — genuine coordination with a claim on Pharaoh. From Pharaoh's seat, it feels like a high-stakes rope with identity-locked binding — the norm cannot be exited without losing the office itself. From nomarch seat, it reads as a conditional coordination: support Pharaoh when reciprocity holds, defect when it fails. The engine computes these per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Egyptian populace (powerless/constrained) are primary beneficiaries — they receive justice, stability, and redistribution; their exit is constrained (identity_locked to Egyptian cosmological identity) but the norm gives them a structural claim. Temple institutions (organized/arbitrage) are beneficiaries — they mediate the reciprocity, gain resources and authority, but can pivot between Pharaohs. Nomarchs (powerful/constrained) are dual-positioned: they benefit from Pharaonic stability but can withdraw support when reciprocity fails (First Intermediate Period). Pharaoh (institutional/identity_locked) is the agenda_setter but also the primary constrained actor — the reciprocity norm binds Pharaoh most tightly; exit is identity_locked (Pharaoh cannot cease being Pharaoh without cosmic disorder). The directionality derivation from beneficiary/victim declarations and exit options captures this: populace and temples get low d (beneficiaries), Pharaoh gets d near 0.5 (symmetric — constrained by the very norm that legitimates rule), nomarchs get moderate d (constrained exit, dual position). No victims declared because this reading sees no structural victim class; failed reciprocity triggers correction, not permanent extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The reciprocity reading avoids mandatrophy by making the founding problem (legitimate authority in a cosmological order) permanently live — cosmic balance is never 'solved.' The constraint persists because the problem persists. No sunset clause is needed because the reciprocity norm is the operating principle, not a temporary measure. The theater ratio rise over time reflects ritual elaboration, not function loss — the coordination function (legitimacy through reciprocity) remains active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_divine_embodiment,
    'Does the reciprocity reading''s claim that Pharaoh is subject to Ma''at logically foreclose the divine_mandate_reading''s claim that Pharaoh embodies Ma''at and cannot violate it, or do they coexist as competing legitimacy framings?',
    'Examine textual evidence for periods where both framings operate simultaneously (e.g., coronation rhetoric vs. wisdom literature admonitions). If coronation texts assert divine embodiment while wisdom texts assert reciprocal obligation, they coexist as complementary legitimacy layers rather than contradictory claims.',
    'If forecloses, the kernel has mutually exclusive readings; if coexists_with, the kernel supports a layered legitimacy structure where divine embodiment and reciprocal obligation operate at different registers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_divine_embodiment, conceptual, 'Structural relationship between reciprocity and divine embodiment readings of Ma''at.').

omega_variable(
    reciprocity_enforcement_mechanism,
    'What is the actual enforcement mechanism of the reciprocity norm — divine retribution, elite defection, popular uprising, or cosmic disorder — and does its effectiveness vary by historical period?',
    'Correlate textual records of Pharaonic failures (famine, military defeat, injustice) with documented outcomes: loss of elite support, succession crises, foreign invasion, or explicit repudiation. Map mechanism effectiveness across Old, Middle, and New Kingdoms.',
    'If enforcement is primarily elite defection, the reciprocity norm operates as an intra-elite coordination mechanism; if popular uprising or cosmic disorder, it has broader structural force. Affects whether the constraint classifies as rope (elite coordination) or scaffold (broader social contract).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_enforcement_mechanism, empirical, 'Mechanism and period-variance of Ma''at reciprocity enforcement.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the maat_order_principle kernel best framed as a single cosmological principle with three interpretive readings, or as three structurally distinct constraints that share vocabulary?',
    'Test epsilon-invariance: if measuring ''Ma''at'' via reciprocity metrics yields ε≈0.35, via divine mandate metrics yields ε≈0.15, and via distributed maintenance yields ε≈0.25, the kernel decomposes into three constraints. If all readings converge on similar ε when measuring the same referent, it is one constraint with interpretive variance.',
    'If three constraints, they form a constraint family linked by network.affects_constraints; if one constraint, the readings are perspectival variants on a single structural object.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the Ma''at kernel decomposes into multiple ε-invariant constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(maat_tr_t500, maat_order_principle__reciprocity_reading, theater_ratio, 500, 0.18).
narrative_ontology:measurement(maat_tr_t1000, maat_order_principle__reciprocity_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(maat_tr_t1500, maat_order_principle__reciprocity_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(maat_tr_t2000, maat_order_principle__reciprocity_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(maat_tr_t2500, maat_order_principle__reciprocity_reading, theater_ratio, 2500, 0.24).
narrative_ontology:measurement(maat_tr_t3000, maat_order_principle__reciprocity_reading, theater_ratio, 3000, 0.25).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(maat_be_t500, maat_order_principle__reciprocity_reading, base_extractiveness, 500, 0.28).
narrative_ontology:measurement(maat_be_t1000, maat_order_principle__reciprocity_reading, base_extractiveness, 1000, 0.32).
narrative_ontology:measurement(maat_be_t1500, maat_order_principle__reciprocity_reading, base_extractiveness, 1500, 0.3).
narrative_ontology:measurement(maat_be_t2000, maat_order_principle__reciprocity_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(maat_be_t2500, maat_order_principle__reciprocity_reading, base_extractiveness, 2500, 0.34).
narrative_ontology:measurement(maat_be_t3000, maat_order_principle__reciprocity_reading, base_extractiveness, 3000, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(maat_su_t500, maat_order_principle__reciprocity_reading, suppression_requirement, 500, 0.38).
narrative_ontology:measurement(maat_su_t1000, maat_order_principle__reciprocity_reading, suppression_requirement, 1000, 0.42).
narrative_ontology:measurement(maat_su_t1500, maat_order_principle__reciprocity_reading, suppression_requirement, 1500, 0.4).
narrative_ontology:measurement(maat_su_t2000, maat_order_principle__reciprocity_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement(maat_su_t2500, maat_order_principle__reciprocity_reading, suppression_requirement, 2500, 0.44).
narrative_ontology:measurement(maat_su_t3000, maat_order_principle__reciprocity_reading, suppression_requirement, 3000, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__reciprocity_reading, 0.08).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% The maat_order_principle kernel decomposes into three readings with distinct ε values and beneficiary structures. This reciprocity_reading (ε=0.35, rope) imposes mutual obligations on Pharaoh; divine_mandate_reading (ε≈0.15, mountain-claimed) makes Pharaoh the embodiment of Ma'at; distributed_maintenance_reading (ε≈0.25, rope) distributes obligation across all stations. They form a constraint family linked by mutual network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__reciprocity_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
