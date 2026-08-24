% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems — Autonomy Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story captures the autonomy primacy reading of the One
 *   Country, Two Systems framework: the 1984 Sino-British Joint Declaration
 *   and the 1990 Hong Kong Basic Law create a binding treaty commitment that
 *   Hong Kong retains substantive autonomy (executive, legislative, judicial)
 *   for 50 years, with civil liberties and judicial independence guaranteed
 *   and internationally enforceable. The reading treats mainland interference
 *   as treaty violation, not legitimate sovereignty exercise. The claimed
 *   type is 'rope' — a genuine coordination mechanism solving a credible
 *   commitment problem across a sovereignty transfer. The authored metrics
 *   reflect this reading's assessment: low base extractiveness (the framework
 *   itself restrains central power), low suppression (meaningful checks
 *   exist), but rising theater ratio and suppression requirement post-2020 as
 *   the framework's operation diverges from its text. The claim/metric gap is
 *   deliberate: the reading claims the constraint IS a rope; the metrics
 *   describe its observed degradation under pressure from a competing
 *   reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.22).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.18).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems — Autonomy Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '3c06e428-c3f4-4153-ba2d-2dad94e0e1c6').
narrative_ontology:cs_kernel_codification('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', formalized).
narrative_ontology:cs_authority_grounding('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', lineage).
narrative_ontology:cs_interpretation_layer_present('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6').
narrative_ontology:cs_reading_relation('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', foundational, substantive_autonomy_treaty_guaranteed).
narrative_ontology:cs_axiom_status(substantive_autonomy_treaty_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', substantive_autonomy_treaty_guaranteed, conventional).
narrative_ontology:cs_axiom('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', foundational, judicial_independence_constrains_executive).
narrative_ontology:cs_axiom_status(judicial_independence_constrains_executive, holdable).
narrative_ontology:cs_axiom_grounding('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', judicial_independence_constrains_executive, conventional).
narrative_ontology:cs_axiom('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', secondary, democratic_reform_pathway_live).
narrative_ontology:cs_axiom_status(democratic_reform_pathway_live, holdable).
narrative_ontology:cs_axiom_grounding('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', democratic_reform_pathway_live, conventional).
narrative_ontology:cs_reference_frame('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', joint_declaration_basic_law_framework).
narrative_ontology:cs_drift_state('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', post_2020_national_security_law, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3c06e428-c3f4-4153-ba2d-2dad94e0e1c6', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_actors).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_executive).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, treaty_supremacy_over_domestic_law).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, judicial_independence_as_constitutional_bedrock).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, substantive_autonomy_as_coordination_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold civil liberties and political rights guaranteed by the Basic Law and the Sino-British Joint Declaration. Experience those rights as enforceable against both the Hong Kong executive and mainland encroachment. Exit is constrained — emigration is possible but costly; political exit (renouncing the framework) means losing the protections it affords.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    organized, biographical, constrained, local).

% Exercise judicial review over executive and legislative acts under the Basic Law; interpret 'one country, two systems' as a justiciable constitutional principle. Their institutional identity is fused with the autonomy framework — professional legitimacy depends on maintaining independence from mainland political direction. Exit would mean accepting political subordination or leaving the judiciary.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, beneficiary).

% Claims sovereign authority over Hong Kong under the Basic Law; interprets 'one country' as paramount. From this reading's perspective, its interventions (National Security Law, electoral reform, interpretation of Basic Law Article 158) are treaty violations that extract autonomy from Hong Kong. It holds arbitrage-grade exit — it can reinterpret or override the framework at will.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Administers Hong Kong under dual accountability: to the Basic Law (and thus the judiciary) and to the central government. Bears the cost of navigating contradictory mandates — judicial rulings that constrain policy versus central directives that demand compliance. Exit is constrained: resignation triggers central appointment of a successor; resistance risks removal.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_executive, payer,
    institutional, biographical, constrained, local).

% Advocate for the democratic reform pathway promised in the Basic Law (universal suffrage for Chief Executive and LegCo). Benefit from the autonomy framework's guarantee of political participation. Since 2020, face prosecution, disqualification, or exile — effectively trapped within a framework that no longer delivers its promised coordination function.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_actors, beneficiary,
    moderate, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_actors, excluded).

% The United Kingdom (co-signatory of the Joint Declaration) and the broader international legal order that treats the Joint Declaration as a binding treaty registered at the UN. Monitors compliance; issues statements; lacks enforcement mechanism beyond diplomatic pressure. Analytical seat — does not collect from or pay into the constraint.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_system, observer,
    institutional, generational, analytical, global).

% Operates in Hong Kong under the National Security Law and other central directives. From this reading's perspective, its presence is the primary suppression mechanism — it enforces the sovereignty primacy reading's vision. Would object to being characterized as an extractive imposition but is structurally excluded from the autonomy framework's coordination logic.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, mainland_security_apparatus, excluded,
    powerful, biographical, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates two distinct legal-political systems under one sovereignty: provides a credible commitment that Hong Kong's common law system, civil liberties, and capitalist economy will persist for 50 years (1997–2047), enabling long-term planning by residents, businesses, and foreign governments without fear of abrupt assimilation.
% TRANSFER_FUNCTION: Transfers legislative and interpretive authority over Hong Kong's internal affairs from the central government to Hong Kong institutions (judiciary, legislature, executive) — a negative transfer (restraint on central power) rather than a positive resource flow. The central government foregoes direct rule; Hong Kong receives autonomy.
% ABSENT_VOICES: Hong Kong residents who would have voted in universal suffrage elections (promised but not delivered); mainland Chinese citizens who might prefer a different model of national integration; the UK government as co-signatory, which has no formal role in adjudicating disputes. The pro-democracy camp is partially excluded (disqualified, imprisoned, exiled) — they would object to the framework's current operation but are not in the room.
% DISAPPEARANCE_RATIONALE: If the autonomy framework vanished overnight, Hong Kong would be directly administered under mainland law: common law replaced by civil law, judicial independence eliminated, civil liberties curtailed to mainland standards, foreign capital flight triggered, and the 'two systems' distinction erased. The world rearranges because the constraint is the only thing maintaining the distinction.
% FOUNDING_PROBLEM: How to return Hong Kong to Chinese sovereignty after 155 years of British rule without destroying the economic, legal, and social systems that made it a global financial center and a distinct society — a problem of credible commitment across a sovereignty transfer.
% FOUNDING_PROBLEM_CORROBORATION: The Joint Declaration and Basic Law texts (negotiated 1982–1990) attest the founding problem. The UK government (co-signatory) and the UN treaty registry corroborate the international legal character. Hong Kong's legal profession and business community (outside the central government's beneficiaries) corroborate that the problem was credible commitment, not mere administrative convenience. The central government now contests whether the founding problem ever required substantive autonomy rather than administrative delegation.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).
:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the framework's core function is negative restraint on central power, not positive extraction from Hong Kong. Suppression is low (0.18) at the framework level — the Basic Law provides justiciable rights — but the suppression_requirement series shows the enforcement machinery needed to maintain the autonomy reading against central encroachment spiked after 2020 (National Security Law). Theater ratio rises as Hong Kong institutions perform autonomy (elections, judicial review) while substantive decision-making shifts to central agencies. Accessibility collapse is moderate (0.45): alternatives (full integration, independence) exist conceptually but are politically foreclosed. Resistance is high (0.55): legal challenges, protests, international advocacy, and institutional non-cooperation persist.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the resident/judiciary seats, the constraint computes as rope (genuine coordination, low χ); from the central government seat, it computes as snare (high χ, the constraint extracts sovereign authority); from the executive seat, tangled_rope (coordinates administration but extracts policy autonomy); from the pro-democracy seat, the computed type shifts over the interval from rope toward snare as the coordination function collapses while extraction (prosecution, disqualification) rises. This perspectival divergence IS the kernel contest structuralized.
 *
 * DIRECTIONALITY LOGIC:
 *   Hong Kong residents and judiciary are structural beneficiaries (d near 0.0) — the framework subsidizes their rights and institutional independence. The PRC central government is the structural target (d near 1.0) — the constraint extracts its sovereign prerogative to govern Hong Kong directly. The Hong Kong executive sits near symmetric (d ~0.5) — it both benefits from administrative autonomy and bears the cost of dual accountability. Pro-democracy actors are identity-locked beneficiaries (d low but exit trapped) — their political identity is constituted through the framework's democratic promise. The international treaty system is an analytical observer (d = 0.5 by default). Mainland security apparatus is excluded from the coordination logic entirely — its operations are the suppression mechanism, not a coordinated party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible commitment across sovereignty transfer) remains live — Hong Kong's distinct systems still require protection from assimilation. But the framework's mandate has atrophied: the central government no longer treats the autonomy guarantee as binding, and the international co-signatory (UK) lacks enforcement leverage. The constraint persists as a coordination shell (rope claim) while its extraction profile shifts toward the central government's benefit. This is not mandatrophy in the classical sense (problem solved, structure remains) but mandate capture: the structure remains because the problem is denied, not because it's solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_violation_vs_sovereign_act,
    'Are mainland interventions (National Security Law, Article 158 interpretations, electoral reform) treaty violations under the Joint Declaration, or lawful exercises of PRC sovereignty under the Basic Law''s ''one country'' premise?',
    'International legal adjudication (ICJ, UN treaty bodies) or a mutually agreed dispute resolution mechanism under the Joint Declaration — neither currently accessible. Absent that, the question resolves politically: if Hong Kong''s common law system remains functionally independent, the autonomy reading holds; if not, the sovereignty reading prevails de facto.',
    'If treaty violation, the constraint''s extraction is external (imposed on the framework); if sovereign act, the framework itself contains the extraction mechanism. Determines whether ε belongs to the constraint or to a separate violating constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_violation_vs_sovereign_act, conceptual, 'Whether the framework''s observed extraction is internal or imposed from outside the coordination logic.').

omega_variable(
    civil_liberties_distributional_incidence,
    'Do civil liberties remain low-epsilon for most residents, or only for a narrowing stratum (professionals, non-political actors) while political dissidents face high extraction?',
    'Empirical survey of rights enforcement across demographic and political segments; case-law analysis of judicial review outcomes for political vs. non-political claims.',
    'If extraction is concentrated on a political minority, the constraint may be a snare for that group while remaining a rope for others — requiring decomposition into constraint family. If uniformly low, the rope claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_liberties_distributional_incidence, empirical, 'Whether the reading''s low-epsilon claim holds across the population or masks targeted extraction.').

omega_variable(
    democratic_pathway_credibility,
    'Is the democratic reform pathway (universal suffrage for CE and LegCo) still a live coordination function of the framework, or has it become a performative promise with no implementation mechanism?',
    'Track NPCSC decisions on electoral reform; measure the gap between Basic Law text (Articles 45, 68) and implemented rules; assess whether any institutional pathway remains for democratic activists to achieve reform without central approval.',
    'If the pathway is dead, the constraint loses its forward-looking coordination function (transition to piton or snare). If live, the scaffold/rope character persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_pathway_credibility, preference, 'Whether the framework''s promised democratic transition is a genuine coordination horizon or a depleted promise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(one__tr_t20, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(one__tr_t22, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 22, 0.3).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(one__tr_t30, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(one__be_t20, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(one__be_t22, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 22, 0.25).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(one__be_t30, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 30, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement(one__su_t20, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(one__su_t22, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 22, 0.35).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(one__su_t30, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__autonomy_primacy_reading, 0.08).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'One Country, Two Systems' label into three structurally distinct readings with different ε values, beneficiary/victim structures, and coordination functions. The autonomy primacy reading (this story) claims low extraction and genuine coordination; the sovereignty primacy reading claims the framework extracts central sovereign authority; the balanced coexistence reading claims a hybrid coordination-extraction equilibrium. They are linked by network.affects_constraints and share the kernel_id in commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__autonomy_primacy_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
