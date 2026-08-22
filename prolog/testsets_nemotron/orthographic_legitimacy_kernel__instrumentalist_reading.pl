% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Instrumentalist Script Reform — Legitimacy via Literacy and Administrative Efficiency
 *   domain: political/linguistic/institutional
 *
 * SUMMARY:
 *   The instrumentalist reading of orthographic legitimacy justifies script
 *   reform (exemplified by the 1928 Turkish alphabet reform) as a pragmatic
 *   response to a literacy crisis and administrative bottleneck. The
 *   constraint is the state's claim that legitimacy derives from measurable
 *   outcomes — literacy rates, administrative efficiency — not from
 *   civilizational continuity or Western alignment. The beneficiaries are the
 *   newly literate population and the state bureaucracy; the primary victims
 *   are the Arabic-literate elite whose specialized human capital is
 *   devalued. The reading presents script choice as a technical decision, not
 *   an identity choice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.28).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Instrumentalist Script Reform — Legitimacy via Literacy and Administrative Efficiency").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political/linguistic/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, 'b16a9764-398b-4c0e-8d4b-aef5f11538fc').
narrative_ontology:cs_kernel_codification('b16a9764-398b-4c0e-8d4b-aef5f11538fc', formalized).
narrative_ontology:cs_authority_grounding('b16a9764-398b-4c0e-8d4b-aef5f11538fc', lineage).
narrative_ontology:cs_interpretation_layer_present('b16a9764-398b-4c0e-8d4b-aef5f11538fc').
narrative_ontology:cs_reading_relation('b16a9764-398b-4c0e-8d4b-aef5f11538fc', orthographic_legitimacy_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b16a9764-398b-4c0e-8d4b-aef5f11538fc', orthographic_legitimacy_kernel__modernist_reading, influences).
narrative_ontology:cs_axiom('b16a9764-398b-4c0e-8d4b-aef5f11538fc', foundational, legitimacy_derives_from_measurable_outcomes).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_measurable_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('b16a9764-398b-4c0e-8d4b-aef5f11538fc', legitimacy_derives_from_measurable_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('b16a9764-398b-4c0e-8d4b-aef5f11538fc', foundational, script_is_pragmatic_tool_not_identity_marker).
narrative_ontology:cs_axiom_status(script_is_pragmatic_tool_not_identity_marker, holdable).
narrative_ontology:cs_axiom_grounding('b16a9764-398b-4c0e-8d4b-aef5f11538fc', script_is_pragmatic_tool_not_identity_marker, conventional).
narrative_ontology:cs_reference_frame('b16a9764-398b-4c0e-8d4b-aef5f11538fc', pre_reform_script_crisis).
narrative_ontology:cs_drift_state('b16a9764-398b-4c0e-8d4b-aef5f11538fc', post_reform_consolidation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b16a9764-398b-4c0e-8d4b-aef5f11538fc', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_bureaucracy).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__instrumentalist_reading, literacy_rate_as_legitimacy_metric).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__instrumentalist_reading, administrative_efficiency_as_legitimacy_metric).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain access to literacy and state services through a phonologically transparent script that matches spoken language. Their children enter school with lower barriers; literacy rates rise measurably within a generation. Exit is mobile — they can adopt the new script without losing identity, and the old script was already inaccessible to them.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    organized, generational, mobile, national).

% Designs and implements the reform. Gains administrative efficiency: unified orthography reduces transcription costs, printing standardization lowers publishing overhead, and a literate population simplifies tax collection, conscription, and legal notification. The state could reverse course but has no incentive — the reform serves its capacity to govern.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold specialized literacy in the Perso-Arabic script (religious scholars, scribes, traditional administrators). Their human capital is devalued by the reform — they lose professional monopoly on reading/writing official documents and religious texts. Exit is constrained: retraining is possible but costly, and their authority rests on the old script's prestige. They are excluded from the reform's justification, which cites statistics not tradition.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, excluded).

% Argue that legitimacy derives from unbroken access to the literary and religious corpus. They are not the primary losers — the arabic_literate_elite bear the material cost — but they provide the intellectual framework for opposition. Their exit is trapped: identity is fused to the script; adopting the new orthography feels like civilizational rupture. They have no institutional seat in the reform process.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, historical_continuity_advocates, excluded,
    moderate, civilizational, trapped, national).

% External actors (European advisors, missionaries, diplomatic missions) who advocated Latin-script adoption as alignment with Western modernity. They are not the agenda-setters here — the state adopts instrumentalist justifications, not civilizational ones — but their prior pressure created the policy window. They observe outcomes from outside the national commitment framework.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, western_modernist_reformers, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of mass literacy and state legibility: a single, phonologically consistent script enables universal schooling, standardized administration, and a shared public sphere without requiring each community to maintain its own scribal tradition.
% TRANSFER_FUNCTION: Transfers the cost of literacy acquisition from the learner (lowered by script transparency) and the state (lowered by administrative standardization) onto the arabic_literate_elite, whose specialized skills are rendered obsolete. The elite lose professional rents; the population and bureaucracy gain reduced friction.
% ABSENT_VOICES: The arabic_literate_elite and historical_continuity_advocates are structurally excluded from the reform's legitimating discourse. The reform justifies itself through literacy statistics and administrative metrics — not through consultation with those whose authority it displaces. They would object that legitimacy cannot be reduced to efficiency, but the reform's epistemology treats that objection as irrelevant.
% DISAPPEARANCE_RATIONALE: If the instrumentalist justification vanished, the state would lose its primary legitimating account for the script reform. The arabic_literate_elite would not automatically regain their position — the material infrastructure (printing, schooling, bureaucracy) has been rebuilt around the new script — but the political settlement would become contested. Competing legitimating narratives (continuity, modernist) would rush the vacuum.
% FOUNDING_PROBLEM: Pre-reform society faced a literacy crisis: the Perso-Arabic script was poorly suited to the spoken language's phonology, required years of specialized training, and confined literacy to a small elite. The state could not efficiently administer, tax, conscript, or communicate with a largely illiterate population. The founding problem was the mismatch between the script's cognitive/administrative load and the needs of a modernizing state.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary literacy statistics and administrative records from the reform period (e.g., Ottoman/Turkish census data, printing records, school enrollment figures) corroborate the literacy crisis and the reform's measurable impact. The arabic_literate_elite's own writings acknowledge the script's difficulty for learners but dispute whether efficiency justifies rupture. Independent linguistic analyses of script-phonology fit (e.g., Zimmer & Orgun 1999 on Turkish orthography) support the instrumentalist claim from outside the benefiting parties.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because the constraint transfers real costs onto a specific group (arabic_literate_elite) while delivering diffuse coordination benefits (literacy, administrative legibility). Suppression is low-moderate (0.28) — the reform was imposed by decree but did not require sustained coercion once the new infrastructure was in place; resistance was intellectual and cultural, not physical. Theater is low (0.15) — the stated justification (literacy statistics) matches the operational driver. Accessibility collapse is moderate (0.35) — the old script remains readable with effort, and religious texts persist in Arabic script; alternatives are not fully foreclosed. Resistance is moderate (0.55) — the elite mounted cultural and religious opposition, but it did not threaten the reform's survival.
 *
 * PERSPECTIVAL GAP:
 *   From the state_bureaucracy seat, the constraint is a rope — genuine coordination solving a real collective-action problem (mass literacy, administrative legibility). From the arabic_literate_elite seat, it is a tangled_rope — they concede the coordination function (literacy did rise) but experience asymmetric extraction (their professional monopoly destroyed). From the historical_continuity_advocates seat, it reads as a snare — the coordination story is cover for civilizational rupture. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_bureaucracy (agenda_setter, institutional, arbitrage exit) sits at the beneficiary end — it designed the constraint and captures the administrative gains. The newly_literate_population (beneficiary, organized, mobile exit) also benefits with low extraction. The arabic_literate_elite (payer, organized, constrained exit) bears the concentrated cost — their exit is constrained because retraining is possible but their authority is script-bound. The historical_continuity_advocates (excluded, moderate, trapped exit) are identity-locked to the old script; they are not the primary extractive target but bear cultural suppression. The western_modernist_reformers (observer, institutional, analytical exit) are external to the commitment framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (literacy crisis, administrative bottleneck) remains live — literacy rates are still a development metric, administrative efficiency still a state goal. The constraint has not become a piton; its coordination function is active and its justification matches its operation. No mandatrophy declaration is needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentalist_vs_modernist_conflation,
    'To what extent was the instrumentalist justification a sincere framing versus a cover for the modernist reading''s civilizational rupture agenda?',
    'Internal deliberation records (cabinet minutes, parliamentary debates, correspondence of reform architects) showing whether efficiency arguments preceded or followed the political decision to adopt Latin script.',
    'If instrumentalist framing was post-hoc, the constraint''s claimed_type (rope) misrepresents its actual structure — it would be a modernist snare wearing an instrumentalist mask. If sincere, the moderate extraction is the genuine price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalist_vs_modernist_conflation, empirical, 'Whether the instrumentalist justification was the genuine motive or a rationalization for modernist rupture.').

omega_variable(
    elite_devaluation_vs_displacement,
    'Were the arabic_literate_elite''s skills devalued (lowered market value) or actively displaced (barred from practice)?',
    'Professional licensing records, employment data for religious scribes and traditional administrators 1928-1950, and fatwa/legal rulings on script use in religious vs. state domains.',
    'If displaced by prohibition, suppression is higher and the constraint edges toward snare. If merely devalued by competition, the coordination function is more genuine and the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_devaluation_vs_displacement, empirical, 'Whether the victim group faced active exclusion or market competition.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the orthographic_legitimacy_kernel admit a single coherent framing, or do the three readings represent fundamentally different kernels masquerading as one?',
    'Comparative analysis of the structural commitments each reading makes: if continuity_reading and modernist_reading share no common referent for ''legitimacy'' beyond the word, the kernel is a linguistic illusion.',
    'If the kernel is incoherent, each reading should be authored as a standalone constraint without kernel linkage. The current family structure would be a category error.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three declared readings share a genuine kernel or merely a label.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_leg_inst_tr_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(orth_leg_inst_tr_t1932, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1932, 0.12).
narrative_ontology:measurement(orth_leg_inst_tr_t1936, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1936, 0.13).
narrative_ontology:measurement(orth_leg_inst_tr_t1940, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1940, 0.14).
narrative_ontology:measurement(orth_leg_inst_tr_t1945, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(orth_leg_inst_tr_t1950, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1950, 0.15).

% Extraction over time
narrative_ontology:measurement(orth_leg_inst_be_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1928, 0.55).
narrative_ontology:measurement(orth_leg_inst_be_t1932, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1932, 0.48).
narrative_ontology:measurement(orth_leg_inst_be_t1936, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1936, 0.44).
narrative_ontology:measurement(orth_leg_inst_be_t1940, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1940, 0.43).
narrative_ontology:measurement(orth_leg_inst_be_t1945, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(orth_leg_inst_be_t1950, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1950, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(orth_leg_inst_su_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1928, 0.45).
narrative_ontology:measurement(orth_leg_inst_su_t1932, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1932, 0.35).
narrative_ontology:measurement(orth_leg_inst_su_t1936, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1936, 0.3).
narrative_ontology:measurement(orth_leg_inst_su_t1940, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1940, 0.28).
narrative_ontology:measurement(orth_leg_inst_su_t1945, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1945, 0.28).
narrative_ontology:measurement(orth_leg_inst_su_t1950, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1950, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% The orthographic_legitimacy_kernel decomposes into three constraint stories: this instrumentalist_reading (rope — genuine coordination with moderate asymmetric extraction), continuity_reading (tangled_rope or snare — coordinates tradition-bound communities but extracts from reform-oriented populations), and modernist_reading (snare — civilizational rupture as extraction cover). The instrumentalist reading is upstream: its literacy/efficiency metrics are cited by modernist reformers as evidence, and its existence creates the policy window continuity_reading must contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__instrumentalist_reading, organized, 0.35).
constraint_indexing:directionality_override(orthographic_legitimacy_kernel__instrumentalist_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
