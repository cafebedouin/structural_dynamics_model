% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as European Settler-Colonial Project (Settler-Colonial Reading)
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint instantiates the settler_colonial_reading of the
 *   contested kernel jewish_self_determination. It treats Zionism not as a
 *   national liberation movement but as a European settler-colonial project
 *   whose operation required and requires the dispossession of the indigenous
 *   Palestinian population through systematic violence, legal exclusion
 *   (notably the Law of Return and absentee property laws), and territorial
 *   fragmentation. The kernel is contested: the liberal_nationalist_reading
 *   frames it as equal national self-determination; the
 *   indigenous_return_reading frames it as indigenous decolonization; the
 *   religious_covenant_reading grounds it in divine promise; the
 *   diasporist_reading rejects territorial sovereignty altogether. This story
 *   isolates the settler-colonial reading as a structurally distinct
 *   constraint with its own epsilon, beneficiary/victim structure, and
 *   classification.
 *
 * KEY AGENTS:
 *   - israeli_state: Primary agenda-setter (institutional/constrained) â designs and enforces the legal-military framework of occupation and settlement.
 *   - european_jewish_settlers: Primary beneficiary (organized/mobile) â receive land, housing, and citizenship through discriminatory legal frameworks.
 *   - palestinian_arabs: Primary target (powerless/trapped) â bear displacement, occupation, and legal exclusion.
 *   - international_human_rights_bodies: Analytical observer (institutional/analytical) â documents violations without enforcement capacity.
 *   - regional_arab_states: Excluded institutional actors (institutional/constrained) â host refugees but are marginalized in decision-making.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.85).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as European Settler-Colonial Project (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '76b6bee2-a68e-4e00-8bf1-34e47dd90383').
narrative_ontology:cs_kernel_codification('76b6bee2-a68e-4e00-8bf1-34e47dd90383', formalized).
narrative_ontology:cs_authority_grounding('76b6bee2-a68e-4e00-8bf1-34e47dd90383', extraction).
narrative_ontology:cs_interpretation_layer_present('76b6bee2-a68e-4e00-8bf1-34e47dd90383').
narrative_ontology:cs_reading_relation('76b6bee2-a68e-4e00-8bf1-34e47dd90383', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('76b6bee2-a68e-4e00-8bf1-34e47dd90383', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('76b6bee2-a68e-4e00-8bf1-34e47dd90383', jewish_self_determination__religious_covenant_reading, influences).
narrative_ontology:cs_reading_relation('76b6bee2-a68e-4e00-8bf1-34e47dd90383', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('76b6bee2-a68e-4e00-8bf1-34e47dd90383', foundational, zionism_as_european_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_as_european_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('76b6bee2-a68e-4e00-8bf1-34e47dd90383', zionism_as_european_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('76b6bee2-a68e-4e00-8bf1-34e47dd90383', foundational, palestinian_return_as_colonial_rectification).
narrative_ontology:cs_axiom_status(palestinian_return_as_colonial_rectification, holdable).
narrative_ontology:cs_axiom_grounding('76b6bee2-a68e-4e00-8bf1-34e47dd90383', palestinian_return_as_colonial_rectification, deontological).
narrative_ontology:cs_reference_frame('76b6bee2-a68e-4e00-8bf1-34e47dd90383', european_settler_sovereignty).
narrative_ontology:cs_drift_state('76b6bee2-a68e-4e00-8bf1-34e47dd90383', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('76b6bee2-a68e-4e00-8bf1-34e47dd90383', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, terra_nullius_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, demographic_majority_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal and military framework of occupation and settlement, enforces the Law of Return while denying Palestinian return, and manages territorial fragmentation through military law, planning regimes, and citizenship discrimination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Receive subsidized housing, land access, water allocations, and citizenship rights through the Law of Return, benefiting from the exclusion of Palestinian land claims and the suppression of Palestinian political presence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of displacement, military occupation, resource confiscation, and legal exclusion; unable to return to seized property, subject to separate military and civil legal regimes, and structurally excluded from the state's democratic promises.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Document violations of international humanitarian law, issue reports on occupation and settlement illegality, and provide an analytical frame that classifies the structure as unlawful but lack enforcement capacity to alter it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% Host refugee populations and periodically advocate for Palestinian rights in international forums, but are structurally marginalized in decisions over Palestinian territory and lack effective leverage to alter the constraint.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, regional_arab_states, excluded,
    institutional, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the territorial acquisition and demographic transformation of Palestine by a European Jewish settler population under state auspices, replacing indigenous Palestinian society with a Jewish-majority state.
% TRANSFER_FUNCTION: Moves land, water, housing stock, and political sovereignty from Palestinian Arabs to the Israeli state and settler population, sustained by military occupation, legal asymmetry, and settlement expansion.
% ABSENT_VOICES: Palestinian refugees and exiles are structurally excluded from political frameworks determining their status; diasporist Jewish voices rejecting territorial sovereignty are marginalized within institutional Zionism.
% DISAPPEARANCE_RATIONALE: If the structure of dispossession and legal exclusion vanished, land tenure would revert or require fundamental renegotiation, the Israeli state as currently constituted would lose its demographic and territorial anchor, and Palestinian refugees could claim return â the regional political order would fundamentally rearrange.
% FOUNDING_PROBLEM: European antisemitism and the failure of Jewish emancipation created a crisis of Jewish statelessness in Europe; Zionism proposed to solve this through the colonization of Palestine.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions attest the founding problem. Palestinian, anti-colonial, and some Jewish historians outside the Zionist framework corroborate the reality of European antisemitism but contest the legitimacy and necessity of the colonial solution; no neutral party corroborates the colonial method as the only possible resolution.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint systematically transfers land, resources, and sovereignty from Palestinians to the Israeli state and settler population, with no reciprocal flow. Suppression is high (0.85) because the arrangement depends on active military occupation, legal discrimination, and the suppression of Palestinian resistance and return. Theater ratio is moderate (0.45): Israeli state discourse presents the structure as liberal democracy and security necessity, performing democratic norms while maintaining colonial legal asymmetries. Accessibility collapse is high (0.82) because alternatives (one-state equality, refugee return, genuine sovereign Palestinian state) have been systematically closed off through settlement facts, military control, and diplomatic capture. Resistance is substantial (0.78) because Palestinian armed and popular resistance, Boycott/Divestment/Sanctions campaigns, and international legal challenges meet the constraint with continuous opposition. Temporal measurements show extraction rising overall with fluctuations around diplomatic junctures, theater rising then stabilizing as the state professionalized its legitimating discourse, and suppression requirement trending upward as enforcement infrastructure hardened and recent military operations intensified.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Israeli state) experiences the constraint as necessary security architecture and demographic preservation; the beneficiary seat (settlers) experiences it as benign national project with material benefits. The payer seat (Palestinian Arabs) experiences the identical structure as violent dispossession and subjugation. The engine computes this divergence from the structural data â the Israeli state's constrained exit reflects geopolitical lock-in, while Palestinian trapped exit reflects physical and legal imprisonment. The large power asymmetry amplifies effective extraction for the powerless target.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and European Jewish settlers are declared beneficiaries, deriving low directionality (near the beneficiary end). Their exit options (constrained/mobile) further reduce effective extraction. Palestinian Arabs are declared victims with trapped exit, placing them near full target (d approaching 1.0). The spatial scope (national/regional) amplifies extraction for the trapped target because verification and intervention are harder at larger scope. International observers have analytical exit and are not in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by testing whether the coordination story (Jewish self-determination as a national right) can be separated from the extraction (Palestinian dispossession). In this reading, they are structurally inseparable: the same legal instruments that enable Jewish return (Law of Return) directly block Palestinian return (Absentees' Property Law, Citizenship and Entry into Israel Law). The founding problem (European antisemitism) is contested in its connection to this specific solution, and the arrangement persists far beyond the original historical emergency, suggesting the constraint's mandate has outlived its claimed function and now serves pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the settler_colonial_reading of kernel jewish_self_determination. How do the sibling readings (liberal_nationalist, indigenous_return, religious_covenant, diasporist) alter the epsilon, beneficiary, and victim structure when instantiated as separate constraints?',
    'Generate the sibling constraints as separate JSON stories and compare their structural deltas.',
    'If sibling readings produce mutually exclusive victim/beneficiary structures, the kernel is irreducibly contested and no single reading captures the constraint''s full operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer frame ambiguity: this constraint is one reading of a contested kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Palestinian resistance primarily structural (military occupation, legal barriers, walls) or internalized (political docility induced by Oslo-era authority structures and economic dependency)?',
    'Post-exit suppression trajectory: if suppression persists after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s hold on Palestinian political agency is deeper than overt coercion suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in occupation context.').

omega_variable(
    snare_vs_tangled_rope,
    'Does the constraint contain any genuine coordination function for Jewish collective survival that is structurally separable from the extraction from Palestinians, or is the coordination story entirely cover?',
    'Counterfactual analysis of whether a non-colonial Jewish self-determination framework would have required the same structural suppression of Palestinian rights.',
    'If separable, reclassification to tangled_rope is warranted; if inseparable, snare classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(snare_vs_tangled_rope, conceptual, 'Whether any coordination function exists separate from extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__settler_colonial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t19, jewish_self_determination__settler_colonial_reading, theater_ratio, 19, 0.25).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__settler_colonial_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(jewi_tr_t42, jewish_self_determination__settler_colonial_reading, theater_ratio, 42, 0.45).
narrative_ontology:measurement(jewi_tr_t56, jewish_self_determination__settler_colonial_reading, theater_ratio, 56, 0.5).
narrative_ontology:measurement(jewi_tr_t66, jewish_self_determination__settler_colonial_reading, theater_ratio, 66, 0.48).
narrative_ontology:measurement(jewi_tr_t76, jewish_self_determination__settler_colonial_reading, theater_ratio, 76, 0.45).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__settler_colonial_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(jewi_be_t19, jewish_self_determination__settler_colonial_reading, base_extractiveness, 19, 0.82).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__settler_colonial_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(jewi_be_t42, jewish_self_determination__settler_colonial_reading, base_extractiveness, 42, 0.8).
narrative_ontology:measurement(jewi_be_t56, jewish_self_determination__settler_colonial_reading, base_extractiveness, 56, 0.83).
narrative_ontology:measurement(jewi_be_t66, jewish_self_determination__settler_colonial_reading, base_extractiveness, 66, 0.86).
narrative_ontology:measurement(jewi_be_t76, jewish_self_determination__settler_colonial_reading, base_extractiveness, 76, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__settler_colonial_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(jewi_su_t19, jewish_self_determination__settler_colonial_reading, suppression_requirement, 19, 0.85).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__settler_colonial_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(jewi_su_t42, jewish_self_determination__settler_colonial_reading, suppression_requirement, 42, 0.8).
narrative_ontology:measurement(jewi_su_t56, jewish_self_determination__settler_colonial_reading, suppression_requirement, 56, 0.85).
narrative_ontology:measurement(jewi_su_t66, jewish_self_determination__settler_colonial_reading, suppression_requirement, 66, 0.88).
narrative_ontology:measurement(jewi_su_t76, jewish_self_determination__settler_colonial_reading, suppression_requirement, 76, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_self_determination kernel. The colloquial label 'Zionism' conflates multiple structurally distinct claims. This story isolates the settler-colonial reading; sibling stories instantiate the liberal-nationalist, indigenous-return, religious-covenant, and diasporist readings. Each has a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
