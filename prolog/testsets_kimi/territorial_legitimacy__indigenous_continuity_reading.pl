% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Indigenous Continuity Reading of Territorial Legitimacy (1948 as Nakba)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the indigenous_continuity_reading of the
 *   territorial_legitimacy kernel. It holds that sovereign legitimacy over
 *   historic Palestine derives from continuous indigenous habitation and
 *   anti-colonial self-determination, framing the 1948 war as the Nakbaâa
 *   catastrophic dispossession rather than a legitimate partition. Under this
 *   reading, the Israeli state is structurally illegitimate as a
 *   settler-colonial entity, the right of return for 1948 refugees is
 *   non-negotiable and central to justice, and partition frameworks including
 *   UN Resolution 181 are rejected as colonial impositions. The constraint
 *   coordinates Palestinian national identity and transnational solidarity
 *   while delegitimizing Israeli statehood and suppressing two-state
 *   pragmatism.
 *
 * KEY AGENTS:
 *   - Palestinian refugee community (powerless/identity_locked/global) â primary beneficiary of the right-of-return claim, identity fused with historic land
 *   - Israeli state and citizenry (institutional/constrained/national) â primary payer, denied legitimacy and self-determination under this reading
 *   - Palestinian liberation factions (organized/identity_locked/global) â agenda-setters enforcing the anti-colonial indigenous framework
 *   - Anti-colonial solidarity movements (organized/constrained/global) â beneficiaries collecting moral coherence and purpose from the framework
 *   - Two-state pragmatists (moderate/constrained/regional) â excluded and paying, rendered illegitimate and unthinkable within this reading
 *   - International legal community (institutional/analytical/global) â observer asked to adjudicate competing legitimacy claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.85).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.82).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Indigenous Continuity Reading of Territorial Legitimacy (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '5a607f97-3b53-4625-bd0b-e5fb619136a6').
narrative_ontology:cs_kernel_codification('5a607f97-3b53-4625-bd0b-e5fb619136a6', fixed_text).
narrative_ontology:cs_authority_grounding('5a607f97-3b53-4625-bd0b-e5fb619136a6', lineage).
narrative_ontology:cs_interpretation_layer_present('5a607f97-3b53-4625-bd0b-e5fb619136a6').
narrative_ontology:cs_reading_relation('5a607f97-3b53-4625-bd0b-e5fb619136a6', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('5a607f97-3b53-4625-bd0b-e5fb619136a6', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('5a607f97-3b53-4625-bd0b-e5fb619136a6', foundational, continuous_habitation_grounds_exclusive_sovereignty).
narrative_ontology:cs_axiom_status(continuous_habitation_grounds_exclusive_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5a607f97-3b53-4625-bd0b-e5fb619136a6', continuous_habitation_grounds_exclusive_sovereignty, deontological).
narrative_ontology:cs_axiom('5a607f97-3b53-4625-bd0b-e5fb619136a6', foundational, settler_colonial_presence_per_se_void).
narrative_ontology:cs_axiom_status(settler_colonial_presence_per_se_void, holdable).
narrative_ontology:cs_axiom_grounding('5a607f97-3b53-4625-bd0b-e5fb619136a6', settler_colonial_presence_per_se_void, deontological).
narrative_ontology:cs_reference_frame('5a607f97-3b53-4625-bd0b-e5fb619136a6', pre_colonial_indigenous_sovereignty).
narrative_ontology:cs_drift_state('5a607f97-3b53-4625-bd0b-e5fb619136a6', contemporary_geopolitical_reality, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('5a607f97-3b53-4625-bd0b-e5fb619136a6', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugee_community).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_solidarity_movements).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_liberation_factions).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_state_citizenry).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, two_state_pragmatists).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, indigenous_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispossessed in 1948 and their descendants; this reading affirms an absolute collective right of return to homes and lands in historic Palestine. Their identity is fused with the territorial claim, making exit from the framework equivalent to abandoning the moral and historical basis of their peoplehood.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugee_community, beneficiary,
    powerless, generational, identity_locked, global).

% Constituted as a settler-colonial entity under this reading; their state is denied legitimacy regardless of internal democratic or legal structures, and their territorial control is framed as an ongoing Nakba. They cannot exit this classification except by dismantling the state or leaving the territory.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_citizenry, payer,
    institutional, generational, constrained, national).

% Derive moral coherence, funding streams, and collective purpose from the anti-colonial framing. The reading provides a clear binary of indigenous legitimacy versus colonial illegitimacy that structures advocacy campaigns, alliance-building, and movement boundaries.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_solidarity_movements, beneficiary,
    organized, biographical, constrained, global).

% Palestinian, Israeli, and international actors who advocate for partition-based compromise. Their proposals are rendered illegitimate and unthinkable within this reading, and they are expelled from solidarity spaces that enforce the indigenous-continuity boundary, bearing the cost of political homelessness.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, two_state_pragmatists, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, two_state_pragmatists, excluded).

% Political and resistance factions that propagate the anti-colonial indigenous framework as the non-negotiable basis of Palestinian identity. They set discursive boundaries, enforce adherence through institutional and social means, and derive legitimacy from unwavering commitment to full territorial restoration.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_liberation_factions, agenda_setter,
    organized, generational, identity_locked, global).

% UN bodies, international courts, and legal scholars asked to adjudicate between competing legitimacy claims. This reading contests the UN's own partition resolution as a colonial imposition, placing the legal community in tension with its foundational post-war framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Palestinian national identity, diaspora cohesion, and transnational anti-colonial solidarity around a unified moral-territorial claim to historic Palestine, providing a non-negotiable boundary against fragmentation or compromise.
% TRANSFER_FUNCTION: Transfers legitimacy and territorial entitlement from Israeli state structures and partition advocates to Palestinian indigenous claimants and refugees, while structurally delegitimizing any territorial arrangement that falls short of full historic restoration.
% ABSENT_VOICES: Two-state pragmatists within Palestinian society, Israeli citizens who do not identify as settlers but are categorized as such, and international legal actors who view UN partition as legitimate are structurally excluded; their presence would dissolve the binary indigenous-colonial boundary.
% DISAPPEARANCE_RATIONALE: If this legitimacy framework vanished, Palestinian national identity would lose its primary territorial anchor, the right of return would become negotiable rather than structural, anti-colonial solidarity movements would lose their organizing binary, and Israeli state legitimacy claims would face a fundamentally different discursive environment.
% FOUNDING_PROBLEM: Colonial dispossession and the fragmentation of Palestinian society in 1948, requiring a moral and legal framework that preserves indigenous entitlement against settler-state normalization.
% FOUNDING_PROBLEM_CORROBORATION: Independent anti-colonial historians and UNRWA demographic records corroborate the 1948 dispossession as a foundational trauma; Israeli state archives and Zionist historiography contest the framing as ongoing Nakba, providing an external check from outside the beneficiary set.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint denies legitimacy to an entire state and population regardless of their current democratic or legal structures, and centralizes all territorial entitlement in one national group defined by pre-1948 habitation. Suppression is high (0.82) because the constraint's persistence requires actively excluding partition discourse, delegitimizing compromise actors, and maintaining a binary indigenous-colonial boundary against complex realities. Theater ratio is moderate (0.45): the anti-colonial framing performs genuine identity-coordination work, but a growing share of discourse enforces boundary maintenance rather than advancing practical return or sovereignty. Accessibility collapse is high (0.78) because once this framework is adopted, partition and liberal-Zionist alternatives become morally unthinkable. Resistance is high (0.88) because the constraint meets sustained opposition from the Israeli state, Western powers, and international legal institutions that recognize partition. The temporal series show extraction and suppression intensifying as partition frameworks failed and the framework consolidated its identity-lock function.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (refugees, liberation factions, solidarity movements) experience this constraint as identity-affirming coordination that resists erasure. The target seats (Israeli citizenry, two-state pragmatists) experience it as an extractive denial of their own legitimacy and political agency. The engine computes this divergence from the structural data; the authored tangled_rope claim reflects the presence of both genuine coordination and asymmetric extraction in the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian refugee community and liberation factions sit at the beneficiary end of directionality: the constraint subsidizes their territorial claim and moral authority. Anti-colonial solidarity movements also benefit from the low-cost moral clarity the framework provides. Israeli state and citizenry are full targets: the constraint structurally extracts legitimacy and self-determination from them. Two-state pragmatists are also targets, bearing the cost of exclusion and delegitimization within Palestinian and solidarity spaces. The international legal community sits near symmetric, as it is pulled between its partition-based charter and anti-colonial normative evolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy mislabeling because its coordination function (preserving Palestinian identity and justice claims against erasure) is structurally inseparable from its extraction function (denying Israeli legitimacy and suppressing partition). It is not a pure snare because the coordination is genuine and historically grounded; it is not a pure rope because the denial of alternatives and the delegitimization of an existing population are actively enforced and extractive. The Tangled Rope classification captures this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_continuity_empirical_or_normative,
    'Is continuous indigenous habitation a discoverable empirical fact that independently generates sovereignty, or a normative commitment that selects and interprets historical evidence?',
    'Independent historiography and archaeology that bracket national frameworks; assessment of whether evidence is treated as constitutive or illustrative.',
    'If purely normative, the constraint''s authority is conventional or deontological; if empirical, it routes through empirically_contingent grounding and could face axiom-overriding drift from counter-evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_continuity_empirical_or_normative, conceptual, 'Whether the indigenous continuity claim is empirical fact or normative frame').

omega_variable(
    refugee_identity_lock_welfare,
    'Does the structural centrality of the right of return benefit refugees by preserving their claim, or victimize them by blocking attainable alternative settlements and prolonging statelessness?',
    'Comparative welfare and integration outcomes for Palestinian refugees versus refugee populations without absolute return frameworks.',
    'Would reclassify the refugee community from pure beneficiary to dual-positioned or payer, altering the directionality map.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refugee_identity_lock_welfare, empirical, 'Welfare effect of absolute return framing on refugees').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of partition alternatives achieved through structural coercion (institutional control, funding gatekeeping, platform denial) or internalized identity-lock (social death for dissenters, self-censorship)?',
    'Ethnographic study of dissent within Palestinian and solidarity institutions; post-exit suppression trajectory of dissenters.',
    'Internalized suppression raises effective extraction beyond the structural measure; purely structural suppression would leave dissenters unharmed after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    partition_reading_foreclosure,
    'Does this reading logically foreclose the partition reading in all possible frameworks, or can a single actor hold indigenous continuity as a moral aspiration while accepting partition as a temporary political arrangement?',
    'Analysis of whether any extant political party or movement simultaneously holds both premises without contradiction.',
    'If coexistent holding is possible, the forecloses relation should be coexists_with or influences; this would alter the kernel''s contamination network and coupling analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_reading_foreclosure, conceptual, 'Logical relation to partition reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(terr_tr_t15, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(terr_tr_t45, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(terr_tr_t76, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 76, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(terr_be_t15, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(terr_be_t45, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 45, 0.75).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 60, 0.8).
narrative_ontology:measurement(terr_be_t76, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 76, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(terr_su_t15, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(terr_su_t45, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(terr_su_t76, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 76, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, security_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the indigenous_continuity_reading of the territorial_legitimacy kernel, decomposed from the colloquial label 'territorial legitimacy' which conflates three structurally distinct claims: partition via international law, security necessity, and indigenous continuity. Each reading has distinct beneficiaries, victim sets, and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
