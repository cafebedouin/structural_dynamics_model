% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Jewish Sovereignty in Palestine â Settler-Colonial Reading
 *   domain: political/postcolonial/nationalism
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of Jewish
 *   sovereignty in Palestine. Under this reading, Zionism is not primarily a
 *   national-liberation movement but a European settler-colonial project
 *   whose structural logic is the displacement of an indigenous population
 *   and the transfer of territory to an immigrant settler society,
 *   underwritten by imperial powers. The constraint is the regime of Jewish
 *   immigration, sovereignty, and territorial control that operates as a
 *   zero-sum displacement mechanism regardless of the intent of individual
 *   immigrants. This reading is one of five contested readings of the kernel
 *   jewish_sovereignty_palestine; it is structurally distinguished by
 *   identifying Palestinians as primary victims of dispossession, Jewish
 *   immigrants as settlers (even when refugees), and the colonial metropole
 *   as the strategic beneficiary.
 *
 * KEY AGENTS:
 *   - palestinian_arab_population: Primary target (powerless/trapped) â bears dispossession, refugeehood, and territorial fragmentation
 *   - zionist_settler_society: Primary beneficiary (organized/identity_locked) â receives land, sovereignty, and state infrastructure
 *   - western_imperial_powers: Secondary beneficiary (institutional/arbitrage) â gains strategic foothold and ideological cover
 *   - israeli_state_apparatus: Agenda setter (institutional/constrained) â administers enforcement and demographic engineering
 *   - regional_arab_states: Excluded voice (institutional/constrained) â hosts refugees, excluded from decision-making
 *   - international_human_rights_observers: Analytical observer (analytical/analytical) â documents but cannot alter structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.82).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Jewish Sovereignty in Palestine â Settler-Colonial Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political/postcolonial/nationalism").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '3f04f90b-9c46-4247-b238-6abb8e4773c0').
narrative_ontology:cs_kernel_codification('3f04f90b-9c46-4247-b238-6abb8e4773c0', distributed).
narrative_ontology:cs_authority_grounding('3f04f90b-9c46-4247-b238-6abb8e4773c0', extraction).
narrative_ontology:cs_interpretation_layer_present('3f04f90b-9c46-4247-b238-6abb8e4773c0').
narrative_ontology:cs_reading_relation('3f04f90b-9c46-4247-b238-6abb8e4773c0', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('3f04f90b-9c46-4247-b238-6abb8e4773c0', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f04f90b-9c46-4247-b238-6abb8e4773c0', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f04f90b-9c46-4247-b238-6abb8e4773c0', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('3f04f90b-9c46-4247-b238-6abb8e4773c0', foundational, zionism_as_settler_colonialism_structural).
narrative_ontology:cs_axiom_status(zionism_as_settler_colonialism_structural, holdable).
narrative_ontology:cs_axiom_grounding('3f04f90b-9c46-4247-b238-6abb8e4773c0', zionism_as_settler_colonialism_structural, empirically_contingent).
narrative_ontology:cs_axiom('3f04f90b-9c46-4247-b238-6abb8e4773c0', foundational, indigenous_dispossession_as_regime_logic).
narrative_ontology:cs_axiom_status(indigenous_dispossession_as_regime_logic, holdable).
narrative_ontology:cs_axiom_grounding('3f04f90b-9c46-4247-b238-6abb8e4773c0', indigenous_dispossession_as_regime_logic, empirically_contingent).
narrative_ontology:cs_reference_frame('3f04f90b-9c46-4247-b238-6abb8e4773c0', settler_sovereignty_as_colonial_norm).
narrative_ontology:cs_drift_state('3f04f90b-9c46-4247-b238-6abb8e4773c0', contemporary_post_1967, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3f04f90b-9c46-4247-b238-6abb8e4773c0', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, zionist_settler_society).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, western_imperial_powers).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_arab_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Indigenous population displaced by successive waves of Jewish immigration and state-building; denied return under the regime of refugee exclusion, subject to military occupation, territorial fragmentation, and legal marginalization inside and outside the 1948 armistice lines. Exit looks like refugeehood without citizenship, besiegement, or second-class residency under foreign sovereignty.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Collective beneficiary of land transfers, state sovereignty, immigration facilitation, and security infrastructure; demographic majority is actively engineered through immigration policy and territorial expansion. Exit is psychically and politically unthinkable for most because collective identity is fused with territorial possession and statehood.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, zionist_settler_society, beneficiary,
    organized, generational, identity_locked, national).

% Gains strategic military foothold, intelligence cooperation, and ideological legitimacy for Western civilizational mission in the Middle East; provides funding, diplomatic shielding, and international legal cover. Can reduce exposure without cost by reallocating regional alliances.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, western_imperial_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Administers the displacement regime through military, legal, and demographic institutions; enforces borders, settlement expansion, and the exclusion of Palestinian refugees. Constrained by its own constituency and security doctrine from transforming into a non-extractive arrangement without institutional collapse.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Host Palestinian refugees and bear regional stability costs but are systematically excluded from determining the political future of Palestine by imperial and settler institutions; their interventions are marginalized or suppressed in international forums.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, regional_arab_states, excluded,
    institutional, generational, constrained, regional).

% Documents displacement, occupation, and apartheid practices through institutional reporting; possesses no enforcement power and is routinely ignored or accused of bias by the agenda-setting state and its imperial sponsors.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_human_rights_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes Jewish immigrant settlement, state-building, and territorial acquisition into a coherent demographic-political project; provides collective security and institutional framework for the settler population.
% TRANSFER_FUNCTION: Moves land, sovereignty, and demographic dominance from the indigenous Palestinian population to the Zionist settler society and its imperial sponsors, underwritten by international legal and military enforcement.
% ABSENT_VOICES: Palestinian refugees expelled in 1948 and their descendants are structurally excluded from territorial and political decision-making; anti-Zionist Jewish voices and binationalist movements are marginalized within the settler political sphere.
% DISAPPEARANCE_RATIONALE: If the displacement regime vanished, Palestinian refugees would exercise return, land tenure would reorganize, the settler state's demographic and territorial architecture would collapse, and imperial strategic interests would lose their primary regional instrument.
% FOUNDING_PROBLEM: European antisemitic persecution and the crisis of Jewish minority status in Eastern and Central Europe; the search for a colonial solution to the 'Jewish question' within the logic of European imperial expansion.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and early colonial officials attest the problem. Palestinian and postcolonial scholars contest that the solution required indigenous dispossession; no corroboration from outside the benefiting parties accepts the displacement mechanism as necessary.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the territorial logic is zero-sum: settler acquisition requires indigenous dispossession. Suppression is higher (0.88) because the regime depends on actively denying Palestinian return, fragmenting their political existence, and suppressing alternatives to partition or equal citizenship. Theater ratio is moderate-high (0.60 at interval end) because the regime invests heavily in democratic, security, and peace-process narratives that obscure the structural continuity of displacement. Accessibility collapse is very high (0.85) for Palestinians because once the regime is understood, exit options (return, sovereign statehood) remain structurally blocked. Resistance is substantial (0.75) because Palestinian refusal and ongoing struggle meet the constraint at every phase. The measurement series tracks the escalation from mandate-era colonial facilitation through partition, Nakba, occupation, and contemporary de facto annexation.
 *
 * PERSPECTIVAL GAP:
 *   The settler seat experiences the constraint as collective self-determination and security coordination; the Palestinian seat experiences it as elimination and displacement. The engine computes divergent classifications from identical structural data because the settler is coded as beneficiary with identity-locked exit (low directionality) while the Palestinian is coded as victim with trapped exit (high directionality). The imperial seat sits near the beneficiary pole despite high mobility because it structurally profits without bearing costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinians are declared victims and are trapped in refugeehood, occupation, and exclusion from sovereignty â this produces high directionality and amplified effective extraction. The settler society is a declared beneficiary and is identity-locked into the territorial project, but the beneficiary declaration overrides exit modulation to keep directionality low. Imperial powers are beneficiaries with arbitrage-grade exit, placing them at the full-beneficiary end. The state apparatus is agenda-setter with constrained exit; it is neither full beneficiary nor full target, sitting near symmetric but leaning beneficiary because its institutional survival depends on the regime's continuation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â European antisemitic persecution â was substantially addressed by the creation of the Israeli state and the migration it enabled. However, the constraint persists and has intensified beyond that resolution because it now serves the structural interests of the settler society and imperial powers (territorial expansion, strategic control). The R5 genealogy interview flags this as contested obsolescence: the original problem is dead or radically transformed, yet the arrangement persists as a tangled rope, preventing misclassification as a rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_reading_kernel_contest,
    'Does the settler-colonial framing of Zionism capture the structural logic of the regime, or does the liberal-nationalist reading of self-determination describe an independent constraint?',
    'Comparative analysis of land acquisition mechanisms, demographic policy, and international legal status across settler-colonial and national-liberation typologies.',
    'If the self-determination reading instantiates a structurally independent constraint, this reading''s epsilon is overstated and the kernel decomposes into two non-equivalent constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_reading_kernel_contest, conceptual, 'Whether the kernel contains one constraint or multiple structurally distinct constraints.').

omega_variable(
    intent_vs_structure_ambiguity,
    'Does the structural displacement outcome persist independently of the ideological intent (refugee haven vs. colonial project) of Jewish immigration waves?',
    'Counterfactual analysis of immigration volumes and land-transfer mechanisms under alternative imperial sponsors and without Zionist institutional framing.',
    'If outcomes are intent-independent, the regime is structurally extractive; if intent fundamentally reshapes the distributive structure, the epsilon reading is sensitive to ideological framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_structure_ambiguity, conceptual, 'Whether displacement is structurally invariant to immigrant intent.').

omega_variable(
    coordination_function_genuineness,
    'Does the coordination provided to the settler population constitute a genuine collective-action solution or merely the organizational infrastructure of dispossession?',
    'Evaluate whether the institutional forms (state agencies, military) could be repurposed for non-extractive coordination or exist solely to maintain the extraction.',
    'If no genuine coordination exists, the constraint is a snare rather than a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuineness, empirical, 'Whether the coordination component is genuine or extraction-only.').

omega_variable(
    suppression_source_ambiguity,
    'Is the suppression of Palestinian alternatives primarily external (military occupation, border control) or internalized (political fragmentation, Oslo-era autopoiesis)?',
    'Measure resistance and organizational capacity across Palestinian territorial sectors (diaspora, West Bank, Gaza, 48 Palestinians).',
    'If internalized, effective suppression is higher than structural measures suggest; if external, coalition formation remains possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsc_scr_tr_t0, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jsc_scr_tr_t20, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(jsc_scr_tr_t40, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(jsc_scr_tr_t60, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(jsc_scr_tr_t80, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(jsc_scr_tr_t100, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(jsc_scr_be_t0, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jsc_scr_be_t20, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(jsc_scr_be_t40, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(jsc_scr_be_t60, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement(jsc_scr_be_t80, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(jsc_scr_be_t100, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jsc_scr_su_t0, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jsc_scr_su_t20, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(jsc_scr_su_t40, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(jsc_scr_su_t60, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(jsc_scr_su_t80, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement(jsc_scr_su_t100, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel jewish_sovereignty_palestine. It decomposes the colloquial label 'Zionism' into structurally distinct claims: the settler-colonial reading (this file), the liberal-nationalist reading, the religious-zionist reading, the cultural-zionist reading, and the post-zionist reading. Each has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
