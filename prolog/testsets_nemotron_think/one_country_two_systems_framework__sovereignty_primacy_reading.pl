% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country Two Systems: Sovereignty Primacy Reading
 *   domain: constitutional/political/sovereignty
 *
 * SUMMARY:
 *   This constraint story captures the sovereignty primacy reading of the One
 *   Country Two Systems framework: Hong Kong's autonomy is delegated by and
 *   revocable through PRC sovereign authority; national security and
 *   territorial integrity override local autonomy when they conflict. The
 *   reading instantiated here is the one operationalized since the 2020
 *   National Security Law — NPCSC interpretations bypass HK legislature,
 *   mainland security agents operate in HK, political opposition is
 *   disqualified/arrested, and judicial independence on national security
 *   matters is functionally suspended. The claimed type is tangled_rope
 *   because the framework genuinely coordinates HK's distinct systems within
 *   Chinese sovereignty (coordination function) while extracting political
 *   autonomy and civil liberties from HK residents to consolidate central
 *   control (asymmetric extraction). The measurement series (1997-2024) shows
 *   accelerating extraction and suppression after 2019, with theater rising
 *   as 'two systems' rhetoric persists while 'one country' dominance hardens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.82).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.88).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country Two Systems: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional/political/sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'a6fc1705-87e4-4395-af14-14e7ac7c435a').
narrative_ontology:cs_kernel_codification('a6fc1705-87e4-4395-af14-14e7ac7c435a', formalized).
narrative_ontology:cs_authority_grounding('a6fc1705-87e4-4395-af14-14e7ac7c435a', extraction).
narrative_ontology:cs_interpretation_layer_present('a6fc1705-87e4-4395-af14-14e7ac7c435a').
narrative_ontology:cs_reading_relation('a6fc1705-87e4-4395-af14-14e7ac7c435a', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a6fc1705-87e4-4395-af14-14e7ac7c435a', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('a6fc1705-87e4-4395-af14-14e7ac7c435a', foundational, prc_sovereignty_absolute_over_hk).
narrative_ontology:cs_axiom_status(prc_sovereignty_absolute_over_hk, holdable).
narrative_ontology:cs_axiom_grounding('a6fc1705-87e4-4395-af14-14e7ac7c435a', prc_sovereignty_absolute_over_hk, conventional).
narrative_ontology:cs_axiom('a6fc1705-87e4-4395-af14-14e7ac7c435a', foundational, national_security_override_autonomy).
narrative_ontology:cs_axiom_status(national_security_override_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a6fc1705-87e4-4395-af14-14e7ac7c435a', national_security_override_autonomy, conventional).
narrative_ontology:cs_reference_frame('a6fc1705-87e4-4395-af14-14e7ac7c435a', basic_law_original_framework).
narrative_ontology:cs_drift_state('a6fc1705-87e4-4395-af14-14e7ac7c435a', post_national_security_law_2020, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a6fc1705-87e4-4395-af14-14e7ac7c435a', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, pro_beijing_hk_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_camp).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary_independence).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_residents_political_rights).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, prc_sovereign_authority_supremacy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, national_security_primacy_over_local_autonomy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, basic_law_as_delegated_not_entrenched).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims ultimate sovereign authority over Hong Kong; interprets Basic Law through NPCSC interpretations; enacted National Security Law bypassing HK legislature; deploys mainland security agents in HK; controls appointment of Chief Executive and principal officials. Exits are not relevant — it sets the rules.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Business tycoons, political parties, and professional groups aligned with Beijing. Receive preferential access to mainland markets, political appointments, and policy influence. Their position depends on the sovereignty primacy framework; exit would mean losing mainland patronage but they retain wealth and international mobility.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, pro_beijing_hk_establishment, beneficiary,
    powerful, biographical, constrained, regional).

% Political parties, activists, and legislators advocating for genuine autonomy. Subject to disqualification, arrest, exile, and organizational bans under National Security Law. Exit options: self-censorship, exile (UK BNO pathway, Taiwan, elsewhere), or imprisonment. Some continue resistance from abroad.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_camp, payer,
    organized, biographical, constrained, regional).

% NGOs, unions, student groups, professional associations, media outlets. Face registration requirements, funding restrictions, leadership arrests, and self-censorship. Many have disbanded or relocated. Exit: dissolve, relocate operations overseas, or operate in severely constrained space.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society, payer,
    moderate, biographical, constrained, regional).

% Judges and legal professionals bound by common law tradition and judicial oath. NPCSC interpretations bind courts on national security matters; NSL Article 44 allows Chief Executive to designate judges for NSL cases; mainland law applies in limited circumstances. Identity-locked: judicial identity fused with rule-of-law role; exit means resigning or compromising professional self-concept.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary_independence, payer,
    institutional, generational, identity_locked, regional).

% Ordinary residents facing restricted assembly, speech, voting rights (electoral reform 2021), and academic freedom. Exit: internal migration to mainland, emigration (BNO, skilled migration), or political disengagement. Cost of exit is high — family, career, home.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_residents_political_rights, payer,
    powerless, biographical, constrained, regional).

% Foreign governments (UK, US, EU, G7), UN human rights bodies, international legal scholars, NGOs. Monitor compliance with Sino-British Joint Declaration and ICCPR. Issue reports, impose sanctions, offer migration pathways. No direct enforcement power over PRC.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_observers, observer,
    institutional, generational, analytical, global).

% Co-signatory of Sino-British Joint Declaration; claims standing to monitor implementation. Publishes six-monthly reports; offers BNO visa pathway to HK residents. PRC rejects UK standing post-1997. Excluded from any formal role in HK governance despite treaty obligation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, uk_government, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Hong Kong's return to Chinese sovereignty while maintaining distinct legal, economic, and administrative systems for a 50-year transition period (1997-2047); solves the problem of integrating a capitalist enclave into a socialist state without immediate systemic rupture.
% TRANSFER_FUNCTION: Moves ultimate interpretive authority over the Basic Law from Hong Kong courts to NPCSC; moves national security enforcement from HK police to mainland state security agents; moves political candidacy vetting from electoral process to Beijing-controlled vetting committee; moves legislative agenda-setting from LegCo to NPCSC/Beijing liaison system.
% ABSENT_VOICES: Hong Kong residents who would vote for genuine autonomy in a fair referendum (never offered); Taiwan population for whom the framework is a unification template; UK government as treaty co-signatory, formally excluded from governance role post-1997; HK localist/independence advocates, banned from political participation.
% DISAPPEARANCE_RATIONALE: The framework structures the entire HK-PRC relationship: legal system, currency, border, trade status, international representation. If it vanished overnight, either full integration (mainland laws apply directly) or crisis (loss of distinct status, capital flight, international status collapse) would follow. No stable 'status quo ante' exists.
% FOUNDING_PROBLEM: How to return Hong Kong to Chinese sovereignty after 155 years of British rule while preserving its capitalist system, common law judiciary, civil liberties, and way of life for 50 years (1997-2047) as guaranteed by the Sino-British Joint Declaration and Basic Law.
% FOUNDING_PROBLEM_CORROBORATION: Sino-British Joint Declaration (UN-registered treaty), Basic Law Articles 1-8 and 158-159, Deng Xiaoping's 1984-87 speeches on 'high degree of autonomy' and 'Hong Kong people ruling Hong Kong'. Corroborated by UK government (treaty party), international legal scholars (treaty interpretation), and HK Bar Association — not solely by PRC beneficiaries.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint transfers substantive political authority — legislative agenda, judicial interpretation, security enforcement, electoral candidacy — from HK institutions to Beijing, with no reciprocal transfer. Suppression (0.88) is very high because alternatives (genuine autonomy, democratic development, judicial independence) are actively foreclosed through NSL, electoral reform, and NPCSC interpretations; exit is constrained (emigration possible but costly). Theater (0.48) reflects the maintenance of 'two systems' branding (common law, currency, customs territory) while the sovereignty primacy operation hollows out autonomy. Accessibility collapse (0.78) is high because the constitutional structure makes autonomy contingent on central approval — no internal mechanism can enforce the guarantee. Resistance (0.65) is significant: 2019 protests, 2020-21 primaries, ongoing diaspora advocacy, international pressure — but has not reversed the trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC agenda-setter seat, the constraint is coordination: a stable framework for HK's return, with central authority as the necessary anchor. From the HK pro-democracy and civil society payer seats, the same structure is extraction: autonomy promised, autonomy revoked, resistance criminalized. The judiciary seat experiences identity-locked capture: professional duty requires applying law, but the law now includes NPCSC interpretations that override common law protections. The engine computes this divergence from the structural data — the sovereignty primacy reading declares the coordination function as real but subordinate, while the autonomy primacy reading would declare it as the core guarantee.
 *
 * DIRECTIONALITY LOGIC:
 *   PRC central government is the structural beneficiary (d ≈ 0.05): it collects sovereign authority, controls interpretation, faces no enforcement cost. Pro-Beijing HK establishment is beneficiary (d ≈ 0.2): gains mainland market access and political patronage, but depends on Beijing's favor. HK pro-democracy camp and civil society are targets (d ≈ 0.9): bear arrests, bans, exile, organizational death; exit is constrained (BNO helps but uproots life). HK judiciary is identity-locked target (d ≈ 0.85): institutional role fused with rule-of-law identity; NPCSC interpretations bind them, NSL Article 44 designates judges — they cannot exit the role without professional self-negation. HK residents are targets (d ≈ 0.75): political rights extracted, exit costly. UK government is excluded (d ≈ 0.5): treaty co-signatory but denied standing; would object if effective. International observers are analytical (d = 0.5): no stake, pure assessment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peaceful return with autonomy guarantee) was live in 1997. By 2024, PRC claims the problem is solved (sovereignty restored, prosperity maintained); HK democrats and UK say the autonomy half of the bargain is broken. The framework persists not because the founding problem remains live, but because it serves as the legitimating structure for sovereignty primacy — a classic mandatrophy candidate. The theater ratio captures this: the 'two systems' performance continues while the 'one country' extraction hardens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the sovereignty_primacy_reading a distinct constraint from the autonomy_primacy_reading, or do they represent observer perspectives on a single constraint?',
    'Test ε-invariance: if measuring extraction via NPCSC interpretations vs. HK court rulings vs. treaty obligations yields structurally different ε values that cannot be reconciled by scope/directionality, they are distinct constraints. The engine''s ε-invariance principle requires decomposition when observables yield different ε.',
    'If distinct, each reading gets its own constraint story with independent classification. If unified, the framework must model observer-relative classification — which the architecture rejects. Current evidence (NSL, electoral reform, NPCSC interpretations) suggests structural divergence sufficient for separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel''s contested readings instantiate separate constraints per ε-invariance.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (distinct HK systems within PRC sovereignty) structurally separable from the extraction function (central override of autonomy), or is the coordination purely performative cover?',
    'Counterfactual: if PRC maintained common law, currency, customs autonomy but removed NSL, restored LegCo powers, ended NPCSC interpretations — would the framework function as coordination? If yes, coordination is real and extraction is layered on top (tangled_rope). If no — if any autonomy maintenance requires central permission that can be withdrawn — coordination is not structurally independent (snare).',
    'Determines whether claimed_type tangled_rope (coordination + extraction) or snare (extraction with coordination cover). The theater_ratio of 0.48 suggests partial but not total performativity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.88) primarily structural (NSL, NPCSC interpretations, electoral reform, mainland agents) or substantially internalized (self-censorship, anticipatory compliance, identity fusion with ''Chinese nation'' narrative)?',
    'Post-exit suppression trajectory: track HK residents who emigrate — if political self-censorship persists abroad, internalized component is significant. Survey diaspora media, academic freedom in overseas HK studies, BNO migrant political behavior.',
    'If internalized suppression is substantial, effective suppression exceeds structural measure — the constraint travels with the subject. This would increase χ for identity_locked agents (judiciary, civil society) beyond what structural d predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/political constraint.').

omega_variable(
    extraction_referent_ambiguity,
    'Does the base extractiveness (0.82) measure extraction FROM Hong Kong people (political rights, judicial independence) or extraction FROM the autonomy arrangement itself (the framework''s integrity as a coordination mechanism)?',
    'Decompose ε: measure extraction from each victim group separately. If hk_judiciary_independence and hk_residents_political_rights show high ε but pro_beijing_hk_establishment shows negative ε (subsidy), extraction is from people. If the framework''s operational coherence (legal certainty, investor confidence) also degrades, extraction is from the arrangement.',
    'If extraction is from the arrangement itself, the constraint may be degrading toward piton (function atrophied, structure persists). If extraction is only from people, it remains tangled_rope/snare with active beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, empirical, 'Whether extractiveness targets the people or the coordination mechanism itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_sp_tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(octs_sp_tr_t3, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(octs_sp_tr_t7, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement(octs_sp_tr_t10, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(octs_sp_tr_t15, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(octs_sp_tr_t20, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(octs_sp_tr_t23, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 23, 0.45).
narrative_ontology:measurement(octs_sp_tr_t27, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 27, 0.48).

% Extraction over time
narrative_ontology:measurement(octs_sp_be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(octs_sp_be_t3, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 3, 0.18).
narrative_ontology:measurement(octs_sp_be_t7, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 7, 0.25).
narrative_ontology:measurement(octs_sp_be_t10, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(octs_sp_be_t15, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(octs_sp_be_t20, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(octs_sp_be_t23, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 23, 0.78).
narrative_ontology:measurement(octs_sp_be_t27, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 27, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(octs_sp_su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(octs_sp_su_t3, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 3, 0.2).
narrative_ontology:measurement(octs_sp_su_t7, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 7, 0.35).
narrative_ontology:measurement(octs_sp_su_t10, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(octs_sp_su_t15, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(octs_sp_su_t20, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(octs_sp_su_t23, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 23, 0.85).
narrative_ontology:measurement(octs_sp_su_t27, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 27, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__sovereignty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hk_national_security_law).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hk_electoral_reform_2021).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, taiwan_one_country_two_systems_template).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, sino_british_joint_declaration_implementation).

% DUAL FORMULATION NOTE:
% This constraint (sovereignty_primacy_reading) and autonomy_primacy_reading are sibling constraints in the one_country_two_systems_framework family. They share the kernel (Basic Law + Joint Declaration) but instantiate different constraints: this reading extracts autonomy for sovereign primacy; the autonomy reading would extract sovereign restraint for autonomy primacy. Their ε values differ substantially (this: 0.82; autonomy reading would author low ε for HK residents, high ε for PRC). The balanced_coexistence_reading occupies a third structural position. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
