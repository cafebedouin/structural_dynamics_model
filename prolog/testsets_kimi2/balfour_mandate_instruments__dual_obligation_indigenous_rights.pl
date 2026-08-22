% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
 *   human_readable: Mandate Dual Obligation â Indigenous Rights Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates the dual_obligation_indigenous_rights
 *   reading of the balfour_mandate_instruments kernel. Under this reading,
 *   the Mandate for Palestine and associated League of Nations instruments
 *   impose an equal or superior obligation to protect existing Arab civil and
 *   political rights and land tenure, subordinating the 'national home'
 *   commitment to self-determination norms and minority-protection
 *   principles. The constraint is structurally a Tangled Rope: it coordinates
 *   the protection of an indigenous majority against settler-colonial
 *   displacement while extracting foregone opportunities from Zionist
 *   organizations and constraining British administrative discretion. The
 *   claim and metrics are authored independently: the reading claims a
 *   binding legal coordination function while the metrics capture the high
 *   extraction this imposes on Zionist organizations and the British
 *   mandatory administration under contested enforcement.
 *
 * KEY AGENTS:
 *   - palestinian_arab_elites: Primary beneficiary (moderate/constrained) â receive land tenure protections and representative-government promises.
 *   - palestinian_arab_communities: Primary beneficiary (powerless/trapped) â receive civil-rights and tenure protection but lack exit from mandatory jurisdiction.
 *   - zionist_organizations: Primary target (organized/constrained) â bear blocked land acquisition and immigration quotas.
 *   - british_mandate_administrators: Agenda-setter and secondary target (institutional/constrained) â enforce the mandate instruments while bearing diplomatic and political costs of restraining Zionist demands.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.76).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.76).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate Dual Obligation â Indigenous Rights Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e4908d68-09f3-4a90-82b5-41d420aac73d').
narrative_ontology:cs_kernel_codification('e4908d68-09f3-4a90-82b5-41d420aac73d', fixed_text).
narrative_ontology:cs_authority_grounding('e4908d68-09f3-4a90-82b5-41d420aac73d', lineage).
narrative_ontology:cs_interpretation_layer_present('e4908d68-09f3-4a90-82b5-41d420aac73d').
narrative_ontology:cs_reading_relation('e4908d68-09f3-4a90-82b5-41d420aac73d', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('e4908d68-09f3-4a90-82b5-41d420aac73d', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('e4908d68-09f3-4a90-82b5-41d420aac73d', foundational, arab_majority_self_determination_principle).
narrative_ontology:cs_axiom_status(arab_majority_self_determination_principle, holdable).
narrative_ontology:cs_axiom_grounding('e4908d68-09f3-4a90-82b5-41d420aac73d', arab_majority_self_determination_principle, deontological).
narrative_ontology:cs_axiom('e4908d68-09f3-4a90-82b5-41d420aac73d', foundational, mandate_minority_protection_floor).
narrative_ontology:cs_axiom_status(mandate_minority_protection_floor, holdable).
narrative_ontology:cs_axiom_grounding('e4908d68-09f3-4a90-82b5-41d420aac73d', mandate_minority_protection_floor, conventional).
narrative_ontology:cs_reference_frame('e4908d68-09f3-4a90-82b5-41d420aac73d', dual_obligation_equilibrium).
narrative_ontology:cs_drift_state('e4908d68-09f3-4a90-82b5-41d420aac73d', mandate_termination_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e4908d68-09f3-4a90-82b5-41d420aac73d', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandate_administrators).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_doctrine).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, arab_majority_self_determination_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communal and municipal leaders under the British Mandate who benefit from land tenure protections, reserved legislative seats, and political representation promises; their authority is mediated through and subordinate to the mandatory administration.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    moderate, generational, constrained, national).

% Rural and urban Arab populations whose existing land tenure and civil rights are nominally protected by the mandate instruments; they lack exit from mandatory jurisdiction and depend on British enforcement to block Zionist land acquisition.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    powerless, generational, trapped, national).

% Jewish Agency and Zionist Executive seeking unrestricted land acquisition and immigration facilitation to build the national home; they are blocked by land transfer restrictions and immigration quotas imposed under the dual obligation reading.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    organized, generational, constrained, national).

% Colonial administration charged with enforcing the mandate instruments; they set local policy but are constrained by the dual obligation terms, League of Nations oversight, and the political costs of restraining Zionist demands while managing Arab expectations.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandate_administrators, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandate_administrators, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of protecting an indigenous majority's land tenure and political rights under colonial administration by binding the mandatory power to minority-protection and self-determination norms.
% TRANSFER_FUNCTION: Transfers administrative discretion away from unconditional Zionist facilitation toward Arab tenure protection; transfers demographic capacity from Zionist immigration to Arab majority preservation; transfers political legitimacy from national home expansion to representative institutions.
% ABSENT_VOICES: Jewish settlers directly affected by immigration caps but without international diplomatic voice; later Palestinian refugees displaced in 1948 who were not yet a formed political constituency; anti-colonial critics who rejected the mandate framework entirely rather than its internal balance.
% DISAPPEARANCE_RATIONALE: If the dual obligation vanished overnight, Zionist organizations would accelerate land acquisition and immigration, Arab land tenure would erode rapidly, and the political trajectory would shift toward Jewish majority dominance and statehood, eliminating the Arab-majority representative path.
% FOUNDING_PROBLEM: How to administer the Palestine Mandate after World War I while reconciling the Balfour Declaration's national home commitment with Arab self-determination claims and League of Nations minority protection obligations.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian Arab delegations and the Arab Higher Committee attested the problem from outside the beneficiary set, arguing the dual obligation was insufficient; the Zionist Executive contested the dual-obligation framing entirely. No neutral international authority corroborated this specific reading as the definitive settlement.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.76, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.76) is high because the constraint systematically blocks Zionist land acquisition and caps immigration, transferring demographic and territorial opportunity to Arab majority preservation. Suppression (0.68) is high because the dual obligation requires active state enforcement of land transfer restrictions and immigration controls against organized resistance. Theater ratio (0.42) reflects significant performative maintenance: British public rhetoric of dual obligation often exceeded actual enforcement, which periodically tilted toward Zionist facilitation. Accessibility collapse (0.70) is high because alternatives to mandatory administration (independent Arab statehood, open Zionist colonization) were structurally foreclosed by the colonial system. Resistance (0.72) is high due to sustained Zionist lobbying, parametric pressure, and the Arab revolt of 1936â1939.
 *
 * PERSPECTIVAL GAP:
 *   From the Arab beneficiary seats, the constraint is protective coordination enforcing international law against settler-colonial expansion; from the Zionist payer seat, it is an extractive block on national self-realization enforced by imperial power; from the British administrative seat, it is a diplomatic trap that extracts political capital and administrative capacity while delivering neither stable Arab trust nor Zionist satisfaction.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab elites and communities are declared beneficiaries (low d, low Ï): they receive protective enforcement and land security. Zionist organizations are declared victims (high d, high Ï): they bear the direct extraction of blocked immigration and land purchase. British administrators are also declared victims (moderate-high d): while they enforce the constraint, they pay through constrained diplomatic maneuverability and the political costs of suppressing Zionist demands. The engine will compute per-seat classifications that diverge accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â reconciling the Balfour Declaration with Arab self-determination â was contested from inception and never achieved stable equilibrium. The constraint persisted beyond its soluble form: by the 1939 White Paper it was enforcing a demographic and territorial freeze that neither party accepted, and by 1948 it collapsed entirely. The classification as Tangled Rope prevents mislabeling the Arab-protection function as pure extraction (it genuinely coordinated tenure security) while also preventing mislabeling the Zionist blockade as incidental (it was structurally necessary to the coordination and actively enforced).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_text_indeterminacy,
    'Do the Palestine Mandate instruments structurally encode a dual obligation prioritizing Arab rights, or is this reading an externally imposed interpretation of ambiguous text?',
    'Forensic textual analysis of the Mandate for Palestine (1922) against League of Nations Covenant Article 22 and minority protection treaties; comparison with other Class A mandates.',
    'If the text is ambiguous, this reading is a construction rather than a discovery, shifting classification toward higher theater_ratio and lowering accessibility_collapse; if the text is determinate, the constraint''s legal fixity is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_text_indeterminacy, conceptual, 'Whether the dual obligation is textually encoded or interpretively constructed.').

omega_variable(
    national_home_semantic_foreclosure,
    'Does interpreting ''national home'' as subordinate to Arab self-determination logically foreclose the Zionist proto-state reading, or can both remain live within international law?',
    'Examination of whether the Mandate text''s ''national home'' clause is semantically compatible with Arab majority political supremacy.',
    'If foreclosed, the sibling reading is structurally impossible within the same legal framework, strengthening the extraction asymmetry; if coexistent, the kernel is genuinely indeterminate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_home_semantic_foreclosure, conceptual, 'Logical relationship between dual-obligation and proto-state readings.').

omega_variable(
    british_enforcement_sincerity,
    'Did British administrators enforce the dual obligation sincerely, or was enforcement performative cover for gradual Zionist facilitation?',
    'Archival analysis of Colonial Office deliberations, land-transfer permit data, and immigration quota enforcement rates against public diplomatic statements.',
    'If performative, theater_ratio is higher than operational metrics suggest and the coordination function is weaker; if sincere, the extraction from Zionists is direct and the constraint functions as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(british_enforcement_sincerity, empirical, 'Sincerity of British enforcement of dual obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0, 0.25).
narrative_ontology:measurement(balf_tr_t7, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 7, 0.3).
narrative_ontology:measurement(balf_tr_t14, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 14, 0.38).
narrative_ontology:measurement(balf_tr_t19, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 19, 0.45).
narrative_ontology:measurement(balf_tr_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 24, 0.5).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 28, 0.55).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(balf_be_t7, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(balf_be_t14, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 14, 0.62).
narrative_ontology:measurement(balf_be_t19, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 19, 0.78).
narrative_ontology:measurement(balf_be_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 28, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(balf_su_t7, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 7, 0.55).
narrative_ontology:measurement(balf_su_t14, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 14, 0.65).
narrative_ontology:measurement(balf_su_t19, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 19, 0.8).
narrative_ontology:measurement(balf_su_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 28, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the balfour_mandate_instruments kernel, decomposed per the epsilon-invariance principle. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. Network edges link the family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
