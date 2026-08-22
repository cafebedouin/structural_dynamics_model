% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Restrictive Sovereignty Reading of the 1951 Refugee Convention
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint story models the *restrictive sovereignty reading* of the
 *   1951 Refugee Convention (as amended by the 1967 Protocol) — one of three
 *   contested readings of the same treaty kernel. This reading treats the
 *   Convention as a minimum floor that permits states maximum discretion in
 *   defining who qualifies for protection. It requires individualized proof
 *   of persecution on a Convention ground, limits 'particular social group'
 *   to immutable characteristics formally recognized by the state, excludes
 *   generalized violence and non-state persecution unless state complicity is
 *   proven, and treats offshore processing and extraterritorial interception
 *   as permissible. The reading is instantiated in domestic legislation
 *   (e.g., US credible fear/expedited removal, EU safe country concepts, UK
 *   Rwanda policy, Australian offshore processing), judicial precedent (e.g.,
 *   Matter of A-B-, EU Qualification Directive restrictive transpositions),
 *   and executive practice. It is not the treaty text itself — it is one
 *   structurally coherent, institutionally powerful reading of that text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.74).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Restrictive Sovereignty Reading of the 1951 Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '8e14ca95-83be-4b85-8662-0136c1c98f0d').
narrative_ontology:cs_kernel_codification('8e14ca95-83be-4b85-8662-0136c1c98f0d', formalized).
narrative_ontology:cs_authority_grounding('8e14ca95-83be-4b85-8662-0136c1c98f0d', lineage).
narrative_ontology:cs_interpretation_layer_present('8e14ca95-83be-4b85-8662-0136c1c98f0d').
narrative_ontology:cs_reading_relation('8e14ca95-83be-4b85-8662-0136c1c98f0d', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e14ca95-83be-4b85-8662-0136c1c98f0d', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('8e14ca95-83be-4b85-8662-0136c1c98f0d', foundational, convention_as_minimum_floor_only).
narrative_ontology:cs_axiom_status(convention_as_minimum_floor_only, holdable).
narrative_ontology:cs_axiom_grounding('8e14ca95-83be-4b85-8662-0136c1c98f0d', convention_as_minimum_floor_only, conventional).
narrative_ontology:cs_axiom('8e14ca95-83be-4b85-8662-0136c1c98f0d', foundational, persecution_requires_state_awareness_or_acquiescence).
narrative_ontology:cs_axiom_status(persecution_requires_state_awareness_or_acquiescence, holdable).
narrative_ontology:cs_axiom_grounding('8e14ca95-83be-4b85-8662-0136c1c98f0d', persecution_requires_state_awareness_or_acquiescence, conventional).
narrative_ontology:cs_axiom('8e14ca95-83be-4b85-8662-0136c1c98f0d', foundational, psg_limited_to_immutable_characteristics_with_state_recognition).
narrative_ontology:cs_axiom_status(psg_limited_to_immutable_characteristics_with_state_recognition, holdable).
narrative_ontology:cs_axiom_grounding('8e14ca95-83be-4b85-8662-0136c1c98f0d', psg_limited_to_immutable_characteristics_with_state_recognition, conventional).
narrative_ontology:cs_reference_frame('8e14ca95-83be-4b85-8662-0136c1c98f0d', postwar_individualized_persecution_framework).
narrative_ontology:cs_drift_state('8e14ca95-83be-4b85-8662-0136c1c98f0d', contemporary_mixed_migration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8e14ca95-83be-4b85-8662-0136c1c98f0d', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, immigration_control_bureaucracies).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_nonstate_persecution).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_identity_groups_without_state_awareness).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, refugee_status_determination_adjudicators_constrained).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, state_sovereignty_over_admission).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, individualized_persecution_requirement).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, immutable_characteristic_definition_of_psg).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and implement the Convention through domestic legislation, asylum procedures, and border controls. They set admissibility standards, define 'particular social group' narrowly, authorize offshore processing, and control the resource allocation for refugee status determination. They benefit from maximum discretion to manage migration flows and avoid protection obligations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Operationalize restrictive interpretation through expedited removal, credible fear screening, detention, and interdiction. Their institutional mission and resource base expand under restrictive readings; personnel identify with the enforcement function. Exit requires abandoning professional identity and institutional affiliation.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies, beneficiary).

% Administer the asylum system under restrictive standards: narrower eligibility means fewer grants, lower costs, simpler workflows. They collect budgetary and organizational benefits from streamlined processing. Can transfer to other administrative roles if the regime changes.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, immigration_control_bureaucracies, beneficiary,
    institutional, generational, mobile, national).

% Fleeing war, state collapse, or generalized violence but lacking individualized persecution proof. Under this reading they are excluded from protection — no well-founded fear of *persecution* on a Convention ground. No legal pathway, no alternative protection, returned to danger.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_generalized_violence, payer,
    powerless, immediate, trapped, global).

% Persecuted by non-state actors (gangs, militias, families, traffickers) where the state is unable or unwilling to protect. This reading requires state awareness/acquiescence for 'persecution' — private actors don't count. No Convention protection, no effective remedy.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_nonstate_persecution, payer,
    powerless, immediate, trapped, global).

% Members of groups defined by gender, sexuality, clan, or other social visibility where persecution is structural but the state does not formally recognize or target the group. 'Particular social group' limited to immutable characteristics *with state awareness* excludes them. No protection pathway.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_identity_groups_without_state_awareness, payer,
    powerless, immediate, trapped, global).

% Must apply restrictive legal standards that contradict their professional judgment and humanitarian training. Face disciplinary pressure for grant rates deemed too high. Cannot easily leave — specialized legal expertise, institutional role, professional identity bound to the system.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, refugee_status_determination_adjudicators_constrained, payer,
    moderate, biographical, constrained, national).

% Advocate for broader interpretation, monitor compliance, provide protection where states fail. Structurally excluded from adjudicative authority — their submissions are consultative, not binding. Would argue for expansive reading but have no vote on the legal standard.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr_and_protection_ngos, excluded,
    organized, biographical, mobile, global).

% Observe destination states' restrictive practices; some benefit from reduced outflow pressure, others face non-refoulement pressure. Their interest is in the geopolitical management of displacement, not individual protection outcomes.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, origin_state_governments, observer,
    institutional, generational, analytical, national).

% Analyze the interpretive contest, document state practice, track the divergence between treaty text and operational reality. No enforcement power; their analysis feeds the long-term interpretive trajectory.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, treaty-based floor for refugee protection that prevents a race to the bottom among states — a minimum common standard that all signatories accept, enabling predictable burden-sharing and non-refoulement norms.
% TRANSFER_FUNCTION: Transfers the cost of protection from destination states (who would bear full responsibility under a broader reading) to excluded asylum seekers (who bear the harm of denial) and origin/transit states (who absorb the displaced). The restrictive reading maximizes the transfer to the vulnerable by minimizing the obligation.
% ABSENT_VOICES: Asylum seekers from generalized violence, non-state persecution, and identity-based persecution without state awareness — the very groups excluded by the restrictive reading. They are physically absent from the interpretive fora (courts, legislatures, UNHCR executive committee) where the reading is crafted and legitimated. Their exclusion is structural: they have no standing, no representation, no vote.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished overnight, destination states would face immediate pressure to grant protection to currently excluded categories — generalized violence fleeers, non-state persecution victims, gender/LGBTQ+/clan-based claimants. Asylum systems would confront expanded caseloads, higher grant rates, and resource demands. Non-refoulement obligations would extend to new populations. The migration governance architecture would reorganize around broader protection.
% FOUNDING_PROBLEM: Post-WWII displacement in Europe required a predictable, limited framework for protecting specifically *persecuted* individuals — not all displaced persons — while preserving state control over admission. The Convention was a compromise: a defined class (individualized persecution on five grounds) in exchange for universal non-refoulement.
% FOUNDING_PROBLEM_CORROBORATION: The drafting history (travaux préparatoires) shows states explicitly limited the definition to individualized persecution and rejected broader 'refugee' concepts — corroborated by diplomatic records outside the beneficiary states. However, UNHCR's supervisory role and subsequent state practice (expanded PSG jurisprudence, gender guidelines, non-state actor recognition) attest the founding problem is contested: the 'limited class' reading is one authoritative strand, not the only one.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the systematic transfer of protection costs from states to excluded populations: the narrower the eligible class, the more displacement harm is externalized. Suppression (0.74) is high because the reading's persistence depends on active enforcement — expedited removal, detention, interdiction, carrier sanctions, safe third country agreements, offshore processing — that physically and legally blocks access to adjudication. Theater ratio (0.42) captures the gap between the Convention's humanitarian framing and the restrictive reading's operational reality: the language of protection is maintained while the substantive scope is narrowed. Accessibility collapse (0.62) is substantial: for excluded categories, the legal pathway does not exist — alternatives (complementary protection, humanitarian parole) are discretionary, fragile, and geographically patchy. Resistance (0.55) is significant but fragmented: litigation, UNHCR advocacy, NGO monitoring, and some judicial pushback exist but have not shifted the dominant institutional trajectory in major destination states.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this reading is legitimate sovereign discretion implementing a treaty floor. From the payer seats (excluded asylum seekers), it is a snare: the Convention's language of protection operates as cover for exclusion. From the constrained adjudicator seat, it is a tangled rope: they must apply standards they view as legally erroneous and morally indefensible. The engine computes this divergence from the structural data — the declared beneficiaries/victims, power atoms, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Destination state governments and their enforcement bureaucracies are structural beneficiaries (d ≈ 0.1–0.2): they collect discretion, control resources, and avoid protection costs. Asylum seekers in excluded categories are structural targets (d ≈ 0.85–0.95): they bear the full harm of denial with no exit. RSD adjudicators are constrained payers (d ≈ 0.6): they bear professional/ethical costs but cannot easily leave. UNHCR/NGOs are excluded observers (d ≈ 0.5 analytically): they see the structure but lack power to change it. Origin states are analytical observers. The exit options reflect real structural conditions: trapped for excluded claimants (no legal pathway, physical barriers), identity_locked for enforcement personnel (professional self-concept bound to the mission), constrained for adjudicators (specialized role, institutional pressure), mobile/arbitrage for state actors (can shift policy, negotiate bilateral deals).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (individualized persecution in post-WWII Europe) is contested: states say it remains live (persecution persists); UNHCR and scholars say the *nature* of displacement has shifted (generalized violence, climate, non-state actors) and the restrictive reading fails the new reality. The mandate has not atrophied — it has been *reinterpreted* to serve a different function: migration control rather than protection. This is not mandatrophy (function lost, form persists) but functional capture (form repurposed). The reading persists because it serves the powerful, not because it solves the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restrictive_vs_expansive_victim_set,
    'Does the restrictive reading''s narrow victim set reflect the Convention''s genuine object and purpose, or is it a sovereign reinterpretation that extracts protection from those the treaty was meant to cover?',
    'ICJ advisory opinion or authoritative treaty body interpretation on the scope of ''persecution'' and ''particular social group'' under Articles 1A(2) and 33, assessed against the travaux préparatoires and subsequent state practice.',
    'If the restrictive reading is found inconsistent with the treaty''s object and purpose, it loses legal legitimacy and the extraction becomes unlawful — shifting classification toward snare. If upheld as a permissible interpretation, the tangled rope classification holds (coordination floor + asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restrictive_vs_expansive_victim_set, conceptual, 'Whether the restrictive reading''s narrow victim set is a faithful or extractive interpretation of the Convention.').

omega_variable(
    coordination_function_vs_extraction_boundary,
    'Is the Convention''s minimum floor (non-refoulement + individualized persecution definition) a genuine coordination mechanism that prevents a race to the bottom, or has the restrictive reading hollowed out the coordination function leaving only extraction?',
    'Counterfactual analysis: if all states adopted the restrictive reading, would the protection regime collapse (no floor) or stabilize at a lower but real equilibrium? Empirical study of state compliance with non-refoulement under restrictive vs. expansive regimes.',
    'If the floor is real and the restrictive reading preserves it, tangled rope (coordination + extraction). If the floor is illusory under this reading, snare (extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_vs_extraction_boundary, empirical, 'Whether the restrictive reading preserves a genuine coordination function or hollows it out.').

omega_variable(
    kernel_reading_identity,
    'Is this reading a structurally distinct constraint from its siblings, or merely a difference in application of the same constraint?',
    'Test ε-invariance: do the sibling readings produce materially different beneficiary/victim sets, extractiveness scores, and enforcement requirements? If yes, they are distinct constraints linked by the kernel; if no, they are observational variants.',
    'Confirms the ε-invariance principle decomposition: three constraint stories for one kernel, each with its own ε, stakeholders, and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s contested readings instantiate distinct constraints per the ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t1951, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1951, 0.15).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t1967, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t1980, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t1990, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t2001, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t2015, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t2025, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t1951, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1951, 0.35).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t1967, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1967, 0.38).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t1980, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t1990, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t2001, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t2015, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t2025, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t1951, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1951, 0.4).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t1967, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t1980, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t1990, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t2001, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t2015, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t2025, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2025, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__restrictive_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, eu_asylum_acquis_implementation).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, us_expedited_removal_regime).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, australian_offshore_processing_system).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, uk_rwanda_asylum_partnership).

% DUAL FORMULATION NOTE:
% The refugee_convention_text kernel decomposes into three constraint stories: restrictive_sovereignty_reading (this file), expansive_humanitarian_reading, and procedural_integrity_reading. Each has distinct ε, beneficiary/victim structures, and operational logics. The restrictive reading maximizes sovereign discretion and narrows protection; the expansive reading maximizes protection scope; the procedural reading fixes process integrity as the non-negotiable core. They compete for institutional uptake and legal authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
