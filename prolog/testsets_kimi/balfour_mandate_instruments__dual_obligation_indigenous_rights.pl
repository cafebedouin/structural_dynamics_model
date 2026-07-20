% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Mandate Dual Obligation: Indigenous Rights Supremacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates the dual_obligation_indigenous_rights
 *   reading of the contested Balfour Mandate kernel. Under this reading, the
 *   Mandate for Palestine and associated instruments impose equal or superior
 *   obligations on the British mandatory authority to protect existing Arab
 *   civil, political, and land-tenure rights, while the 'national home'
 *   commitment is subordinated to self-determination norms and
 *   minority-protection principles. The constraint is structurally a tangled
 *   rope: it carries a genuine coordination function (colonial
 *   administration, minority protection under international law) but extracts
 *   heavily from Zionist organizations (blocked land acquisition and
 *   immigration) and from British administrators (whose discretion is
 *   constrained by international oversight). The high extractiveness and
 *   active enforcement requirement reflect that the dual obligation could
 *   only persist through sustained suppression of Zionist political and
 *   territorial objectives.
 *
 * KEY AGENTS:
 *   - palestinian_arab_communities: Primary beneficiary (moderate/constrained) â receives protective land-tenure and civil-rights obligations
 *   - zionist_organizations: Primary target (organized/identity_locked) â bears extraction through blocked land acquisition and immigration quotas
 *   - british_mandatory_authority: Dual-positioned administrator (institutional/constrained) â both enforces and is constrained by the dual obligation regime
 *   - league_mandates_commission: Analytical observer (institutional/analytical) â monitors compliance from the international level
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.79).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.85).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.79).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate Dual Obligation: Indigenous Rights Supremacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'cae2cff6-c686-4d9c-b101-88673b497caa').
narrative_ontology:cs_kernel_codification('cae2cff6-c686-4d9c-b101-88673b497caa', formalized).
narrative_ontology:cs_authority_grounding('cae2cff6-c686-4d9c-b101-88673b497caa', lineage).
narrative_ontology:cs_interpretation_layer_present('cae2cff6-c686-4d9c-b101-88673b497caa').
narrative_ontology:cs_reading_relation('cae2cff6-c686-4d9c-b101-88673b497caa', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('cae2cff6-c686-4d9c-b101-88673b497caa', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('cae2cff6-c686-4d9c-b101-88673b497caa', foundational, arab_majority_self_determination_supremacy).
narrative_ontology:cs_axiom_status(arab_majority_self_determination_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('cae2cff6-c686-4d9c-b101-88673b497caa', arab_majority_self_determination_supremacy, conventional).
narrative_ontology:cs_axiom('cae2cff6-c686-4d9c-b101-88673b497caa', foundational, national_home_subordination_clause).
narrative_ontology:cs_axiom_status(national_home_subordination_clause, holdable).
narrative_ontology:cs_axiom_grounding('cae2cff6-c686-4d9c-b101-88673b497caa', national_home_subordination_clause, conventional).
narrative_ontology:cs_reference_frame('cae2cff6-c686-4d9c-b101-88673b497caa', mandate_as_minority_protection_regime).
narrative_ontology:cs_drift_state('cae2cff6-c686-4d9c-b101-88673b497caa', mandate_termination_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cae2cff6-c686-4d9c-b101-88673b497caa', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_authority).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, self_determination_norm).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitute the majority population under the mandate, holding existing land tenure and civil/political rights that the mandate instruments expressly obligate the mandatory power to protect. Their political claims to self-determination and representative government are grounded in majority status and the mandate's minority-protection framework. Exit is constrained by colonial borders and the absence of sovereign statehood.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    moderate, generational, constrained, national).

% Seek to acquire land and facilitate Jewish immigration to build the national home in Palestine. Under this reading of the mandate instruments, they are blocked from land acquisition by transfer restrictions and their demographic objectives are capped by immigration quotas designed to preserve Arab majority status. Their organizational identity is locked to territorial and demographic transformation, making exit from the constraint unthinkable without abandoning the core mission.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    organized, generational, identity_locked, global).

% Administers the mandate territory under League of Nations supervision. While possessing formal legislative and executive authority, the dual obligation reading constrains their discretion: they must actively restrict land transfers and immigration to protect Arab rights, against both Zionist political pressure and imperial strategic preferences. They bear the political and administrative costs of enforcing an obligation that satisfies neither Zionist nor Arab constituencies fully.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_authority, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_authority, payer).

% Oversees mandate compliance from Geneva, reviewing British annual reports and petitioning on behalf of indigenous populations. Represents the international legal framework that purports to bind the mandatory authority to dual obligations, though its enforcement power is primarily moral and procedural.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_mandates_commission, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the governance of a multi-communal colonial territory by imposing legal obligations on the mandatory power to protect an indigenous majority's existing rights while accommodating a minority national home project.
% TRANSFER_FUNCTION: Transfers administrative discretion from the mandatory power to international legal oversight; transfers land security and political standing from prospective Jewish immigrants and settlers to the existing Arab majority.
% ABSENT_VOICES: Jewish immigrants who would have settled but were blocked by immigration quotas; rival European powers with competing colonial claims; the Ottoman successor state interests that were extinguished by the mandate system; Palestinian Arab peasants without elite representation before the Permanent Mandates Commission.
% DISAPPEARANCE_RATIONALE: If the dual obligation constraint vanished overnight, Zionist organizations would rush land acquisition and immigration, the Arab majority's political and territorial position would rapidly erode, and the British administration would face an unregulated colonial scramble rather than a legally structured mandatory regime.
% FOUNDING_PROBLEM: The collapse of Ottoman imperial authority after World War I left Palestine without sovereign governance, requiring an international legal framework to administer the territory while balancing indigenous majority rights with the Balfour Declaration's national home commitment.
% FOUNDING_PROBLEM_CORROBORATION: British Colonial Office records and the League of Nations Covenant Article 22 corroborate the post-Ottoman administrative vacuum from outside the Arab beneficiary set. However, the specific dual-obligation supremacy framing is contested: Zionist organizations attest the problem was Jewish homelessness and that the mandate was the solution, while anti-colonial critics reject the mandate system entirely. No neutral international party independently corroborates the indigenous-rights-supremacy design as the necessary legal response.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) because the constraint systematically transfers land security and demographic majority protection from Zionist organizations to the Arab majority, backed by mandatory enforcement. Suppression is high (0.85) because the constraint's persistence depends on actively blocking land sales and capping immigration against organized Zionist resistance. Theater ratio rises to 0.52 because British enforcement became increasingly performative after the 1939 White Paper, maintaining legal rhetoric of dual obligation while actual administrative capacity to protect Arab rights eroded under wartime and post-war pressures. Accessibility collapse is moderate (0.60): alternative legal frameworks (Ottoman continuity, outright British sovereignty, or Jewish statehood) were suppressed but never fully extinguished in discourse. Resistance is high (0.72) from sustained Zionist political, legal, and paramilitary opposition to land-transfer restrictions and immigration caps.
 *
 * PERSPECTIVAL GAP:
 *   The Palestinian Arab communities experience the constraint as protective coordination (legal shelter against settler-colonial displacement), computing toward rope or tangled-rope from the beneficiary seat. Zionist organizations experience it as extractive suppression of their national project, computing toward snare from the target seat. British administrators experience asymmetry: formally they are agenda-setters, but under this reading they are payers constrained by international legal obligations that override imperial preference. The engine computes this divergence from the structural data â the same legal instrument reads as protection, extraction, or administrative burden depending on seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab communities are named beneficiaries (low d, subsidized by the constraint's protective function). Zionist organizations are named victims (high d, targeted by land and immigration restrictions). British mandatory authority is simultaneously agenda_setter and payer: they administer the constraint but are structurally victimized by it in the sense that their discretion is cabined. The League Mandates Commission sits at analytical scope with no extractive stake. Because Zionist organizations are identity_locked to the national home project, their effective extraction is amplified; because the Arab communities are constrained within a colonial system without exit, their beneficiary status is partial and conditional.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â administering a post-Ottoman territory without sovereign statehood â was live in 1920 but became increasingly contested as both Arab and Zionist national movements matured. By the late mandate period, the dual obligation framework operated as a zombie constraint: its protective coordination function had atrophied (British enforcement weakened), but the legal structure persisted, maintained performatively through white papers and commissions. The classification as tangled_rope prevents mislabeling the Arab protective function as mere cover (snare) or the Zionist extraction as mere coordination cost (rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the Mandate for Palestine structurally subordinate the Jewish national home to Arab civil and political rights, or is this an imposed interpretive frame?',
    'Textual and archival analysis of the mandate drafting history, comparing the July 1922 mandate text against the correspondence and minutes of the Permanent Mandates Commission.',
    'If the text is genuinely ambiguous, this reading is one plausible construction rather than a legally compelled constraint; if the text clearly establishes dual obligation with indigenous supremacy, the reading is a faithful legal interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: textual ambiguity at the kernel level').

omega_variable(
    enforcement_capacity_vs_legal_obligation,
    'Did the British mandatory authority possess the institutional capacity and political will to enforce dual obligations against Zionist organizational pressure and imperial strategic interests?',
    'Comparative colonial administration analysis measuring land-transfer restriction enforcement rates, immigration quota adherence, and prosecution of violations across the mandate period.',
    'If enforcement was structurally impossible, the constraint operated as theatrical legal performance (high theater_ratio) rather than effective coordination; if enforcement was strategically withheld, the constraint was a snare using legal form to mask discretionary extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_legal_obligation, empirical, 'Whether legal obligation translated into material enforcement').

omega_variable(
    sibling_reading_structural_delta,
    'How would classification change if the Jewish national home primacy reading were adopted as the operative framework?',
    'Construct the sibling constraint story and compare beneficiary/victim sets, extraction directionality, and enforcement requirements.',
    'The primacy reading would invert beneficiary and victim sets (Zionist organizations become beneficiaries, Palestinian Arab communities become victims) and shift claimed type toward snare or tangled_rope with different directionalities, demonstrating that kernel contestation is not semantic but structurally divergent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta between sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_dual_obligation_tr_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0, 0.25).
narrative_ontology:measurement(balfour_dual_obligation_tr_t6, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 6, 0.3).
narrative_ontology:measurement(balfour_dual_obligation_tr_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 12, 0.35).
narrative_ontology:measurement(balfour_dual_obligation_tr_t18, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 18, 0.42).
narrative_ontology:measurement(balfour_dual_obligation_tr_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 24, 0.48).
narrative_ontology:measurement(balfour_dual_obligation_tr_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 28, 0.52).

% Extraction over time
narrative_ontology:measurement(balfour_dual_obligation_be_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(balfour_dual_obligation_be_t6, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(balfour_dual_obligation_be_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(balfour_dual_obligation_be_t18, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(balfour_dual_obligation_be_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(balfour_dual_obligation_be_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 28, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(balfour_dual_obligation_su_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(balfour_dual_obligation_su_t6, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(balfour_dual_obligation_su_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(balfour_dual_obligation_su_t18, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(balfour_dual_obligation_su_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(balfour_dual_obligation_su_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 28, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one member of the balfour_mandate_instruments kernel family. The kernel decomposes into three structurally distinct readings because the same mandate text supports divergent epsilon profiles and opposite beneficiary/victim sets. This reading (dual_obligation_indigenous_rights) is linked to both siblings as part of the family decomposition required by the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
