% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination as Liberal-Nationalist Statehood Claim
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the liberal-nationalist reading of the contested
 *   Jewish sovereignty in Palestine kernel: the claim that Jewish people
 *   constitute a nation with a collective right to self-determination, and
 *   that establishing statehood in the ancestral homeland is a legitimate —
 *   not merely permissible but rights-vindicating — exercise of that right,
 *   analogous in structure to other national liberation movements recognized
 *   under 20th-century international law. This reading is distinguished from
 *   its siblings by treating Palestinians as co-equal self-determination
 *   claimants over the same territory, which structurally commits it to
 *   partition or binational frameworks as the normatively required
 *   resolution, rather than to exclusive claim (religious_zionist_reading),
 *   non-political cultural presence (cultural_zionist_reading), inherent
 *   colonial illegitimacy (settler_colonial_reading), or retrospective
 *   critique of the founding framework itself (post_zionist_reading). The
 *   extractiveness is authored as moderate rather than severe because the
 *   reading's own internal logic anticipates and requires territorial
 *   compromise — the extraction that occurs in practice (occupation,
 *   settlement expansion beyond negotiated frameworks) represents the
 *   reading's aspiration being structurally underdelivered, not fulfilled.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.42).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination as Liberal-Nationalist Statehood Claim").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '2febeb19-b38b-4bd5-82e3-6e5c6c94a177').
narrative_ontology:cs_kernel_codification('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', distributed).
narrative_ontology:cs_authority_grounding('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', distributed).
narrative_ontology:cs_reading_relation('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', foundational, jewish_peoplehood_constitutes_nation_entitled_to_self_determination).
narrative_ontology:cs_axiom_status(jewish_peoplehood_constitutes_nation_entitled_to_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', jewish_peoplehood_constitutes_nation_entitled_to_self_determination, conventional).
narrative_ontology:cs_axiom('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', foundational, palestinian_claim_is_coequal_requiring_partition_or_binational_accommodation).
narrative_ontology:cs_axiom_status(palestinian_claim_is_coequal_requiring_partition_or_binational_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', palestinian_claim_is_coequal_requiring_partition_or_binational_accommodation, deontological).
narrative_ontology:cs_reference_frame('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', liberal_international_self_determination_doctrine).
narrative_ontology:cs_drift_state('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', post_oslo_collapse_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2febeb19-b38b-4bd5-82e3-6e5c6c94a177', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_seeking_national_refuge).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_residents_of_occupied_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_national_movement).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, national_self_determination_as_universal_right).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, peoplehood_grounds_territorial_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise collective self-determination through a sovereign state with Jewish majority governance, army, and immigration law (Law of Return). Also bear costs of prolonged conflict, security burdens, and international legitimacy contestation, but the arrangement is built around and for their national self-determination claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens, payer).

% Hold a standing right of return and citizenship regardless of prior residence, grounded in the claim that Jewish peoplehood constitutes a nation entitled to self-determination in its ancestral homeland. This right functions as insurance against renewed persecution and as vindication of the historical claim to territorial continuity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_seeking_national_refuge, beneficiary,
    moderate, generational, mobile, global).

% Displaced in 1948 and 1967 and their descendants; denied a symmetrical right of return recognized by the liberal-nationalist framework, which treats Jewish return as vindication of self-determination but treats Palestinian return as a demographic threat to be negotiated away or excluded. Bear the actual territorial cost of the Jewish national project's realization.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under varying degrees of military occupation, settlement expansion, and movement restriction. The liberal-nationalist reading acknowledges their co-equal self-determination claim in principle (endorsing partition or two-state frameworks) but the territorial and security arrangements built to secure Jewish sovereignty directly constrain their political and physical freedom of movement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_residents_of_occupied_territories, payer,
    powerless, biographical, trapped, regional).

% Administer the sovereignty claim through law, military, and diplomacy, translating the self-determination principle into territorial control, citizenship law, and security policy. Sets the terms under which partition or binational proposals are entertained or rejected.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, analytical, national).

% The UN partition framework, subsequent peace processes, and liberal international law doctrine on self-determination provided the normative architecture this reading depends on. These institutions periodically re-adjudicate the claim (UN votes, ICJ opinions, peace negotiations) without being bound by either party.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_liberal_order_institutions, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, international_liberal_order_institutions, agenda_setter).

% Advances a co-equal self-determination claim to the same territory. The liberal-nationalist reading formally recognizes this claim as requiring partition or binational accommodation, but in practice the movement's voice in shaping the actual territorial settlement has been structurally weaker than its formal recognition implies — negotiations proceed largely on terms set by the stronger party.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_national_movement, excluded,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_national_movement, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal-political mechanism (statehood) through which a historically persecuted and stateless people secures collective physical safety, cultural continuity, and political agency — solving the coordination problem of diaspora vulnerability by consolidating national self-defense capacity in one territory.
% TRANSFER_FUNCTION: Moves land, water, residency rights, and political sovereignty from the pre-existing Arab population of Mandatory Palestine and their descendants to the incoming and established Jewish national community, formalized through statehood, citizenship law, and asymmetric return rights.
% ABSENT_VOICES: Palestinian refugees displaced in 1948 and 1967 have no meaningful voice in Israeli citizenship or return policy; their own right of return is treated as negotiable or void where the liberal-nationalist framework treats Jewish return as an inalienable expression of the same self-determination principle. The framework's formal recognition of Palestinian co-equal claims has not translated into symmetrical negotiating power.
% DISAPPEARANCE_RATIONALE: If the liberal-nationalist self-determination claim were withdrawn as the legitimating frame for Israeli statehood, the entire diplomatic, legal, and constitutional architecture of the state — citizenship law, security doctrine, international recognition claims — would require a wholly different justificatory basis (religious, purely security-based, or none), and alternative political settlements (binational state, confederation, altered partition) would become live options that the current frame forecloses.
% FOUNDING_PROBLEM: Sustained European and Middle Eastern antisemitism, culminating in the Holocaust, demonstrated that diaspora existence left Jewish communities without sovereign capacity for self-defense; a state was proposed as the structural remedy ensuring a national people could not again be rendered stateless and undefended.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies, historians of the Mandate period, and UN institutions from outside the Zionist movement corroborate that statelessness and persecution were real historical conditions motivating the claim (League of Nations Mandate debates, postwar refugee law development). However, the same outside observers — UN human rights rapporteurs, international law scholars, and Palestinian civil society — dispute whether the founding problem's contemporary instantiation (ongoing occupation, settlement expansion) remains proportionate to or justified by the original safety rationale, making the status of the founding problem itself contested rather than settled by either side alone.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) reflecting that this reading's own normative commitments require compromise and co-equal recognition of Palestinian claims — the extraction that has occurred historically (1948 displacement, 1967 occupation, ongoing settlement) runs ahead of what the liberal-nationalist framework itself would sanction, which is precisely the gap the post_zionist_reading and settler_colonial_reading seize on. Suppression is moderate (0.42) because maintaining the sovereignty claim requires active military and legal enforcement of borders and citizenship distinctions, but the reading does not require suppressing all alternatives (it explicitly countenances partition). Resistance is high (0.72) because the claim is met with sustained organized opposition from the co-equal claimant population and from significant portions of the international community. Accessibility collapse is moderate (0.35): alternative arrangements (binational state, full partition, confederation) remain actively discussed and are not foreclosed by this reading's own logic, even though political conditions have made them increasingly difficult to realize.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (Israeli state institutions), the arrangement appears as the fulfillment of a coordination function — providing safety and self-governance for a previously stateless people — administered through necessary but regrettable territorial and security measures. From the payer seats (Palestinian refugees, residents of occupied territories), the same structure appears as enforced territorial and demographic transfer, with the 'co-equal claimant' recognition remaining largely rhetorical against the asymmetry of actual negotiating power and military capacity. The engine's per-seat computation should reflect this divergence without the authored claim adjudicating which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish Israeli citizens and diaspora seeking national refuge are coded as beneficiaries: the constraint's central function (sovereignty as vindication of self-determination) directly serves their security and political agency, even though citizens also bear real costs (conflict, international isolation). Palestinian refugees and residents of occupied territories are coded as victims: the same structure that grounds Jewish return rights and sovereign control correspondingly denies or constrains their return and self-governance, despite the reading's formal endorsement of their co-equal claim. The palestinian_national_movement is coded as excluded rather than purely payer because the story's own six-questions data show its formal recognition under this reading exceeds its practical influence over settlement terms — an asymmetry the framework itself does not resolve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora statelessness and vulnerability to persecution) was real and severe at the kernel's origin. Whether it remains fully live in its original form, given the existence of a militarily robust and internationally supported Jewish state, is genuinely contested — hence founding_problem_status is authored as 'contested' rather than 'dead,' since credible corroborating voices (including Israeli security establishment figures) maintain the underlying vulnerability persists in different form (regional hostility, antisemitism resurgence), while other outside observers argue the state's own security capacity has substantially resolved the original problem, leaving territorial expansion beyond the 1948/1967 lines unmoored from the founding rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coequal_claim_recognition_vs_practice_gap,
    'Does the liberal-nationalist reading''s formal recognition of Palestinian co-equal self-determination claims meaningfully constrain actual territorial and security policy, or does it function as legitimating rhetoric while material practice proceeds asymmetrically?',
    'Longitudinal analysis of negotiation outcomes (Oslo, Camp David 2000, Annapolis) against the formal partition/binational commitments nominally endorsed by liberal Zionist political factions, cross-referenced with settlement expansion data during the same periods.',
    'If the gap is large and persistent, this reading functions closer to a snare wearing tangled_rope''s coordination language; if the gap reflects genuine but repeatedly frustrated attempts at compromise (rejected by counterparties or derailed by violence on both sides), the tangled_rope classification with moderate extraction is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coequal_claim_recognition_vs_practice_gap, empirical, 'Whether formal co-equal-claim recognition constrains practice or merely legitimates it.').

omega_variable(
    national_self_determination_scope_ambiguity,
    'Does the liberal international law principle of national self-determination, developed primarily for decolonization contexts, apply straightforwardly to a diaspora people seeking to establish sovereignty in a territory already inhabited by another people claiming indigenous status?',
    'Comparative analysis against other contested applications of self-determination doctrine (Kurdistan, Western Sahara, Kosovo) to assess whether the doctrine''s application here is principled extension or motivated analogy.',
    'If the doctrine does not straightforwardly extend to this case, the liberal-nationalist reading''s normative grounding is weaker than it presents itself, closer to policy preference dressed as universal right; if it does extend coherently, the reading''s coordination function is more robustly grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_self_determination_scope_ambiguity, conceptual, 'Whether liberal self-determination doctrine coherently extends to this specific territorial claim.').

omega_variable(
    kernel_reading_selection_bias,
    'Is the liberal-nationalist reading selected as ''the'' mainstream account of Zionism because it is structurally the most defensible reading, or because it is the reading most palatable to international liberal audiences whose approval Israeli diplomacy has needed?',
    'Historical analysis of when and how liberal-nationalist framing was adopted in Zionist diplomatic communication relative to religious and settler-colonial framings used in internal Israeli political discourse during the same periods.',
    'If the reading is primarily a diplomatic packaging choice rather than the operative internal logic, its extractiveness score may understate the actual mechanism at work, which might better track the religious_zionist_reading or settler_colonial_reading''s structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether this reading''s prominence reflects structural fit or diplomatic utility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1917, 0.22).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the jewish_sovereignty_palestine kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. The liberal_nationalist_reading is distinguished by explicit recognition of Palestinian co-equal self-determination claims (driving moderate rather than severe extractiveness) and by grounding legitimacy in secular international-law doctrine rather than theology (religious_zionist_reading) or cultural presence alone (cultural_zionist_reading). It stands in tension with settler_colonial_reading, which denies the self-determination premise's applicability altogether, and with post_zionist_reading, which accepts the state's existence but argues the ethnonational framework this reading defends has become an obstacle to civic equality. Each sibling carries its own beneficiary/victim structure and metrics; do not average or reconcile ε across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
