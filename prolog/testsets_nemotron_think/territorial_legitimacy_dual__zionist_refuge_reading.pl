% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Territorial Legitimacy
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story captures the Zionist Refuge Reading of territorial
 *   legitimacy in Israel/Palestine — one of three contested readings of the
 *   same kernel. This reading grounds Israeli legitimacy in three pillars:
 *   (1) historical persecution culminating in the Holocaust, establishing
 *   moral claim to refuge; (2) divine promise / historical connection to the
 *   land, establishing metaphysical claim; (3) UN General Assembly Resolution
 *   181 (partition), establishing international legal claim. The reading
 *   holds 1948 boundaries as uncontested legitimacy (defensive war after
 *   partition acceptance), treats 1967 territories as negotiable but
 *   security-justified, frames Palestinian displacement as consequence of
 *   Arab rejection of partition rather than Zionist expulsion, and views
 *   ongoing territorial control as security necessity. The constraint
 *   operates as a tangled rope: it genuinely coordinates Jewish collective
 *   survival and self-determination (coordination function) while extracting
 *   territory, autonomy, and rights from Palestinians through active military
 *   and legal enforcement (extraction function). The theater ratio reflects
 *   growing performative democracy maintenance alongside substantive
 *   occupation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'd078ac21-3edc-487c-80d2-0cf009c53afe').
narrative_ontology:cs_kernel_codification('d078ac21-3edc-487c-80d2-0cf009c53afe', formalized).
narrative_ontology:cs_authority_grounding('d078ac21-3edc-487c-80d2-0cf009c53afe', lineage).
narrative_ontology:cs_interpretation_layer_present('d078ac21-3edc-487c-80d2-0cf009c53afe').
narrative_ontology:cs_reading_relation('d078ac21-3edc-487c-80d2-0cf009c53afe', territorial_legitimacy_dual__palestinian_autochthony_reading, forecloses).
narrative_ontology:cs_reading_relation('d078ac21-3edc-487c-80d2-0cf009c53afe', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('d078ac21-3edc-487c-80d2-0cf009c53afe', foundational, jewish_historical_right_to_sovereignty).
narrative_ontology:cs_axiom_status(jewish_historical_right_to_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d078ac21-3edc-487c-80d2-0cf009c53afe', jewish_historical_right_to_sovereignty, theological).
narrative_ontology:cs_axiom('d078ac21-3edc-487c-80d2-0cf009c53afe', secondary, un_partition_resolution_as_legal_basis).
narrative_ontology:cs_axiom_status(un_partition_resolution_as_legal_basis, holdable).
narrative_ontology:cs_axiom_grounding('d078ac21-3edc-487c-80d2-0cf009c53afe', un_partition_resolution_as_legal_basis, empirically_contingent).
narrative_ontology:cs_axiom('d078ac21-3edc-487c-80d2-0cf009c53afe', secondary, arab_rejection_caused_palestinian_displacement).
narrative_ontology:cs_axiom_status(arab_rejection_caused_palestinian_displacement, holdable).
narrative_ontology:cs_axiom_grounding('d078ac21-3edc-487c-80d2-0cf009c53afe', arab_rejection_caused_palestinian_displacement, empirically_contingent).
narrative_ontology:cs_axiom('d078ac21-3edc-487c-80d2-0cf009c53afe', secondary, security_justifies_territorial_control).
narrative_ontology:cs_axiom_status(security_justifies_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('d078ac21-3edc-487c-80d2-0cf009c53afe', security_justifies_territorial_control, instrumental).
narrative_ontology:cs_reference_frame('d078ac21-3edc-487c-80d2-0cf009c53afe', zionist_refuge_legitimacy_framework).
narrative_ontology:cs_drift_state('d078ac21-3edc-487c-80d2-0cf009c53afe', post_oslo_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d078ac21-3edc-487c-80d2-0cf009c53afe', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jews).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, zionist_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinians_under_occupation).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, jewish_self_determination_right).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, refugee_solution_through_sovereignty).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territorial control regime, sets security policy, manages settlement enterprise, and negotiates diplomatic frameworks. Collects legitimacy capital from the reading's narrative. Could alter the constraint but bears high political cost for doing so.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Experience the constraint as realized self-determination and security guarantee after historical persecution. Benefit from sovereign rights, law of return, state services. Exit options constrained by identity attachment and limited global refuge alternatives. Bear costs of military service and regional conflict.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jews, beneficiary,
    organized, biographical, constrained, national).

% Ideological and institutional engine that produced the reading. Collects legitimacy, funding, and political capital from the constraint's operation. Maintains global advocacy networks. Could pivot framing but has invested institutional identity in this reading.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, zionist_movement, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, zionist_movement, agenda_setter).

% Derives existential security guarantee and identity anchor from the sovereign state. Not directly subject to territorial enforcement but affects and is affected by its legitimacy. Exit is mobile (can disengage politically) but identity-locked for many.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora, beneficiary,
    moderate, biographical, mobile, global).

% Displaced in 1948 and 1967, denied return, stateless or host-country dependent. Bear the displacement cost framed by this reading as consequence of Arab rejection. No effective exit from refugee status; right of return blocked by the constraint's enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under military occupation in West Bank/Gaza with movement restrictions, land expropriation, dual legal systems. Bear daily extraction (land, water, autonomy) justified by security narrative. Exit options severely constrained by permit regime and geography.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinians_under_occupation, payer,
    powerless, biographical, trapped, local).

% Represent Palestinian national claims but structurally excluded from legitimacy adjudication in this reading. Their rejection of partition is cited as cause of displacement; their current demands (right of return, 1967 boundaries) are framed as rejectionist. Would object to the reading's framing but lack standing in its framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_leadership, excluded,
    moderate, biographical, constrained, regional).

% Holds UN partition resolution 181 as legal basis but increasingly recognizes Palestinian rights. Provides diplomatic framework (two-state parameters) but lacks enforcement will. Observes constraint operation through human rights monitoring, ICJ opinions, Security Council resolutions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_community, observer,
    institutional, generational, analytical, global).

% Formal custodians of the partition resolution and subsequent legal framework (Resolutions 242, 338, 2334). Produce legal opinions on occupation, settlements, apartheid allegations. Authority is interpretive not enforcement; constraint persists despite adverse findings.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, un_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a sovereign refuge for a historically persecuted people, converting existential vulnerability into collective self-defense through statehood. Solves the coordination problem of Jewish survival by concentrating agency in a territorial sovereign recognized by UN partition.
% TRANSFER_FUNCTION: Moves territorial control, demographic majority, and security architecture from Palestinian inhabitants to Jewish sovereign authority. Transfers the cost of refugee absorption and ongoing security maintenance to the international system and Palestinian population. Transfers legitimacy capital from historical persecution narrative to contemporary sovereign rights.
% ABSENT_VOICES: Palestinian refugees and occupied Palestinians are structurally excluded from the legitimacy adjudication — their objection is framed as the cause of their own displacement. Pre-1948 Palestinian majority population has no voice in the partition acceptance narrative. Mizrahi Jewish refugees from Arab lands are invoked as symmetry but not consulted on Palestinian displacement.
% DISAPPEARANCE_RATIONALE: If this legitimacy reading vanished overnight, the legal-moral foundation for Israeli sovereignty in 1948 lines would collapse, the law of return would lose its normative ground, the occupation's security justification would evaporate, and the entire international diplomatic framework (two-state, land-for-peace) would require reconstruction. The physical state would persist but its legitimacy architecture would need replacement.
% FOUNDING_PROBLEM: The founding problem was the existential vulnerability of a stateless people subjected to recurring persecution culminating in genocide, with no territory where they could exercise collective self-defense. The UN partition offered a legal pathway; Arab rejection forced military victory as the only realization.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state education attest the problem remains live (rising antisemitism, Iran threat). Palestinian historiography and international human rights bodies attest the founding problem was real but the solution imposed disproportionate cost on Palestinians — the refugee problem persists because the solution displaced it rather than solved it. Post-Zionist historians (Morris, Pappé, Shlaim) corroborate from within the Israeli academy that the founding problem's solution entailed deliberate displacement.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the constraint transfers territorial control and demographic outcomes from Palestinians to Jewish sovereign authority, with costs borne disproportionally by the displaced and occupied. Suppression (0.72) reflects active enforcement: military occupation, permit regimes, settlement expansion, legal dual systems, blockade. Theater (0.38) reflects that democratic institutions, courts, and peace process rituals perform legitimacy while substantive control deepens. Accessibility collapse (0.45) is moderate — alternatives (binational state, full withdrawal, right of return) remain conceptually available but politically collapsed. Resistance (0.78) is high — Palestinians maintain national movement, international legal challenges, BDS, uprisings. The claimed type is tangled_rope because genuine coordination (Jewish survival/self-determination) coexists with asymmetric extraction (Palestinian displacement/occupation) under active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats (Israeli government, Israeli Jews, Zionist movement), the constraint appears as genuine coordination — a refuge built from persecution, defended against existential threats, offering negotiable compromise on 1967 lines. From the payer seats (Palestinian refugees, occupied Palestinians), the same structure operates as enforced extraction — displacement framed as their leaders' fault, occupation justified by security needs that expand settlements, no negotiating partner that recognizes their core claims. The engine computes this divergence from the structural data: the same territorial control regime produces sovereignty for one people and statelessness for another.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli government (agenda_setter, institutional, arbitrage exit) sits near beneficiary end — controls the constraint, collects legitimacy, could change it but at high political cost. Israeli Jews (beneficiary, organized, constrained exit) are net beneficiaries — gain sovereignty/security, bear military/service costs, exit constrained by identity. Zionist movement (beneficiary/agenda_setter, organized, mobile) is institutional beneficiary with ideological investment. Jewish diaspora (beneficiary, moderate, mobile) gains existential security guarantee, identity anchor. Palestinian refugees (payer, powerless, trapped) bear foundational displacement cost with no exit. Palestinians under occupation (payer, powerless, trapped) bear daily extraction. Palestinian leadership (excluded, moderate, constrained) structurally barred from legitimacy adjudication. International community and UN (observers, institutional, analytical) witness but lack enforcement will.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish existential vulnerability) was live in 1948 and remains contested today. The reading claims the problem persists (rising antisemitism, Iran) justifying continued constraint. But the constraint has expanded beyond the founding solution: 1967 occupation, settlement enterprise, and nation-state law extend extraction beyond the refugee solution. The mandate has atrophied into territorial maximalism — the refugee problem is solved for Jews but reproduced for Palestinians. This is not pure mandatrophy (the founding problem isn't dead) but mandate expansion: the coordination function has grown extractive appendages that the founding problem doesn't justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the territorial_legitimacy_dual kernel, or does it blend with the two_state_coexistence_reading in practice?',
    'Analyze whether Israeli governments since 1993 have formally adopted the two-state framework (Oslo, Camp David, Annapolis) while substantively maintaining the zionist_refuge_reading''s territorial logic. Track divergence between diplomatic rhetoric and settlement/occupation policy.',
    'If blended, the constraint''s extractiveness is higher than this reading alone admits — the two-state rhetoric becomes theater masking continued zionist_refuge territorial logic. If distinct, the two_state_reading is a genuine alternative framework that has been foreclosed by power asymmetries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the zionist_refuge_reading and two_state_coexistence_reading are structurally distinct or merged in practice.').

omega_variable(
    divine_promise_vs_legal_partition_tension,
    'Does the reading''s dual grounding in divine promise (theological, non-negotiable) and UN partition (legal, negotiated) create an internal contradiction that drives extraction?',
    'Trace how theological claim to whole land (Eretz Israel) interacts with legal claim to partition lines. Measure settlement expansion in areas beyond partition lines but within divine promise. Assess whether security justification is cover for theological expansion.',
    'If theological claim drives expansion beyond legal claim, the constraint''s coordination function (UN partition legitimacy) is cover for theological extraction. The tangled rope becomes more snare-like. If legal claim genuinely constrains, 1967 negotiability holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_promise_vs_legal_partition_tension, conceptual, 'Tension between theological and legal grounding of legitimacy and its effect on territorial extraction.').

omega_variable(
    palestinian_displacement_causality,
    'Is Palestinian displacement primarily caused by Arab rejection of partition (as this reading claims) or by Zionist military planning and expulsion (as Palestinian reading claims)?',
    'Historical analysis of 1948 war archives (Israeli, British, Arab), demographic studies of depopulated villages, Plan Dalet documentation, and comparative refugee studies. New historiography since 1980s (Morris, Pappé, Karsh, Shlaim) provides contested evidence.',
    'If Zionist planning/expulsion was primary driver, the reading''s core framing (displacement as Arab-caused) is a cover story — the extraction is foundational, not reactive. This would shift classification toward snare. If Arab rejection was primary driver, the reading''s coordination claim (defensive war) is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_displacement_causality, empirical, 'Causal attribution of 1948 Palestinian displacement and its classification consequence.').

omega_variable(
    security_justification_authenticity,
    'Are security concerns the genuine driver of territorial control in 1967 territories, or is security the legitimating narrative for ideological/religious settlement?',
    'Correlate settlement locations with genuine security needs (high ground, approaches to population centers) vs. ideological/religious sites. Analyze IDF security assessments vs. political settlement decisions. Track whether disengagement (Gaza 2005, Sinai 1982) improved or degraded security.',
    'If security is genuine driver, the extraction is coordination-adjacent (tangled rope). If security is cover for settlement, the extraction is primary (snare). The theater ratio would rise significantly in the latter case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_justification_authenticity, empirical, 'Whether security justification for 1967 territorial control is authentic or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_tr_t1973, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_tr_t1993, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_tr_t2000, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_be_t1973, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1973, 0.65).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_be_t1993, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_be_t2000, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_su_t1973, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1973, 0.72).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_su_t1993, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_su_t2000, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(territorial_legitimacy_dual__zionist_refuge_reading_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__zionist_refuge_reading, 0.08).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_right_of_return_claim).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_resolution_181_implementation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the territorial_legitimacy_dual kernel. The kernel is the contested territorial legitimacy between Jordan River and Mediterranean Sea. This reading (zionist_refuge) grounds legitimacy in Jewish historical persecution, divine promise, and UN partition acceptance. The palestinian_autochthony_reading grounds it in continuous habitation, displacement trauma, and right of return. The two_state_coexistence_reading grounds it in mutual recognition with 1967 lines as compromise. The three readings have different ε values, beneficiary/victim structures, and claimed types. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, institutional, 0.15).
constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, organized, 0.2).
constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, moderate, 0.25).
constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
