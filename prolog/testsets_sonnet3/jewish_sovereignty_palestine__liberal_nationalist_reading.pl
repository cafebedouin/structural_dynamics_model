% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Jewish Collective Self-Determination Right (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the liberal nationalist reading of the contested
 *   jewish_sovereignty_palestine kernel: Jewish people, as a nation with a
 *   well-documented history of statelessness-driven persecution, possess a
 *   collective self-determination right, and establishing sovereign statehood
 *   in the ancestral homeland is a legitimate exercise of that right —
 *   legitimate but not unbounded, since the same self-determination principle
 *   extends co-equally to Palestinians. On this reading the state's founding
 *   is a real coordination achievement (solving the diaspora's existential
 *   vulnerability) that carries genuine, non-trivial costs to a second
 *   national claimant whose parallel state was never realized. The 1993 Oslo
 *   peak in theater_ratio reflects a period of heavy diplomatic process
 *   (summits, declarations, interim authorities) that did not convert into
 *   the two-state outcome the reading's own logic requires, which is why the
 *   metrics show enforcement (suppression_requirement) and extraction
 *   settling at a moderate, unresolved plateau rather than either resolving
 *   toward a rope (post-partition coexistence) or hardening into a full
 *   snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.42).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination Right (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '753de47c-9196-4743-96a7-84fefa3939df').
narrative_ontology:cs_kernel_codification('753de47c-9196-4743-96a7-84fefa3939df', distributed).
narrative_ontology:cs_authority_grounding('753de47c-9196-4743-96a7-84fefa3939df', distributed).
narrative_ontology:cs_reading_relation('753de47c-9196-4743-96a7-84fefa3939df', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('753de47c-9196-4743-96a7-84fefa3939df', jewish_sovereignty_palestine__religious_zionist_reading, influences).
narrative_ontology:cs_reading_relation('753de47c-9196-4743-96a7-84fefa3939df', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('753de47c-9196-4743-96a7-84fefa3939df', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('753de47c-9196-4743-96a7-84fefa3939df', foundational, jewish_ancestral_continuity_grounds_claim).
narrative_ontology:cs_axiom_status(jewish_ancestral_continuity_grounds_claim, holdable).
narrative_ontology:cs_axiom_grounding('753de47c-9196-4743-96a7-84fefa3939df', jewish_ancestral_continuity_grounds_claim, empirically_contingent).
narrative_ontology:cs_axiom('753de47c-9196-4743-96a7-84fefa3939df', foundational, coequal_self_determination_requires_reciprocal_recognition).
narrative_ontology:cs_axiom_status(coequal_self_determination_requires_reciprocal_recognition, holdable).
narrative_ontology:cs_axiom_grounding('753de47c-9196-4743-96a7-84fefa3939df', coequal_self_determination_requires_reciprocal_recognition, deontological).
narrative_ontology:cs_reference_frame('753de47c-9196-4743-96a7-84fefa3939df', un_partition_two_state_framework).
narrative_ontology:cs_drift_state('753de47c-9196-4743-96a7-84fefa3939df', post_oslo_collapse_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('753de47c-9196-4743-96a7-84fefa3939df', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, national_self_determination_doctrine).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, two_peoples_two_states_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold full citizenship, security guarantees, and state institutions organized around Jewish national self-determination — army, immigration law (Law of Return), land administration, and cultural infrastructure. Some are recent refugees or descendants of refugees from Europe and Middle Eastern/North African expulsions; the state's founding is experienced as rescue and vindication of a right long denied. Exit from the arrangement (e.g. binational restructuring) is available in principle through political process but not something most contemplate abandoning.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens, agenda_setter).

% Retain an automatic right of immigration and citizenship (Law of Return) grounded in the claim that Jewish self-determination requires a guaranteed refuge given documented histories of persecution. Most never exercise this right but its existence is treated as the substantive good the arrangement secures against a recurrence of statelessness.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_seeking_refuge, beneficiary,
    moderate, biographical, mobile, global).

% Displaced during the 1948 war and its aftermath, they and their descendants hold no right of return under Israeli law and remain stateless or held in refugee status across neighboring states and camps. The liberal nationalist reading treats their claims as requiring negotiated resolution (compensation, limited return, or resettlement) rather than as foreclosing Jewish statehood, but the reading concedes their displacement is a real cost the state's founding imposed and has not remedied.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Live under military occupation or blockade in the West Bank and Gaza without citizenship in the state that controls their movement, land use, and security environment. Their own self-determination claim is, on this reading, co-equal in principle but has not been realized in practice; the occupation's persistence past any transitional justification is the sharpest tension the liberal reading must account for.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinians_under_occupation, payer,
    powerless, biographical, trapped, regional).

% Hold formal citizenship and vote, but live inside a state constitutionally defined as the nation-state of the Jewish people, with land, budgetary, and symbolic asymmetries relative to Jewish citizens. They benefit from state services and rule of law while bearing the structural cost of a national project defined around a collective identity that is not theirs.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel, beneficiary).

% Host large refugee populations and have historically rejected or conditioned recognition of Israeli statehood on resolution of Palestinian claims. Their own interests (security, refugee absorption costs, domestic legitimacy) shape whether partition or binational solutions are politically viable, but they are not party to the internal Israeli-Palestinian negotiation over the kernel.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, regional_arab_states, excluded,
    institutional, generational, constrained, regional).

% UN bodies, international courts, and human rights organizations assess the arrangement against self-determination doctrine, humanitarian law, and historical resolutions (e.g. UNGA 181, UNSC 242). They document the gap between the liberal reading's declared aspiration (two co-equal peoples) and the occupation's actual duration and structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the Jewish people, dispersed and repeatedly subject to statelessness and persecution, with a sovereign state that guarantees self-determination, physical security, and an automatic refuge — solving a genuine collective-action problem (a stateless nation cannot reliably protect its members) that predates and is structurally independent of any harm to Palestinians.
% TRANSFER_FUNCTION: Sovereignty, land administration, and demographic control over the territory move to the Jewish national collective and its state institutions; Palestinian communities lose land, refugee return rights, and in the occupied territories, political self-determination and free movement, without a reciprocal state of their own having been established.
% ABSENT_VOICES: Palestinian refugees and residents of the occupied territories are the parties most affected by the unresolved half of the self-determination bargain (the promised 'two states' side) but have no vote in Israeli sovereign decisions and limited standing in negotiations conducted between states and blocs; regional Arab states carry refugee burdens without a seat in the bilateral framework.
% DISAPPEARANCE_RATIONALE: If Jewish statehood were dissolved overnight, the security, legal, and immigration architecture organizing millions of lives would collapse or require complete reconstruction; conversely, if the underlying self-determination claim were rejected as illegitimate rather than merely incompletely reciprocated, the entire legal and diplomatic basis for the state (UN partition resolution, subsequent recognitions, security guarantees) would be void — either direction is a total rearrangement, not a marginal one.
% FOUNDING_PROBLEM: Centuries of diaspora statelessness culminating in the Holocaust demonstrated that a people without sovereign self-determination and territorial refuge remained existentially vulnerable to persecution with no state obligated to protect them; the Zionist movement and the 1947 UN partition plan were built to solve that specific problem by establishing a sovereign Jewish national home.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust and pre-state persecution history is independently corroborated by historians, international tribunals, and the UN's own 1947 partition deliberations — the founding problem's historical reality is not seriously disputed outside fringe positions. What is contested, including by international legal bodies and human rights organizations outside either party's benefiting set, is whether the SOLUTION as currently implemented (open-ended occupation, unresolved refugee claims) remains proportionate to that founding problem or has outrun it — that status question is attested from outside both the Israeli state and Palestinian national movements by UN special rapporteurs, ICJ advisory proceedings, and comparative self-determination scholarship.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.48) rather than low or high: the liberal reading itself expects territorial compromise, so it does not claim zero cost to Palestinians, but it also does not claim the arrangement is purely extractive since it names a genuine coordination good (refuge, security) achieved for a previously stateless people. Suppression (0.42) reflects the occupation's continuing enforcement apparatus (checkpoints, permit regimes, military administration) required to hold the unresolved territorial status quo, moderated by the fact that within Israel proper the arrangement functions through ordinary legal and political institutions rather than raw coercion. accessibility_collapse (0.4) and resistance (0.68) are authored to reflect that alternatives (binationalism, full partition, confederation) remain actively debated and mobilized around rather than foreclosed — this is precisely what distinguishes the liberal reading from readings that treat the current arrangement as either inevitable or as irredeemably colonial.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish Israeli citizen seat, the arrangement is experienced as a hard-won, still-necessary coordination structure (institutional power, arbitrage-grade exit via political voice, generational horizon of a secured homeland). From the Palestinian refugee or occupied-territory seat, the same structural arrangement computes as effectively extractive and enforced (powerless, trapped, biographical horizon under permit and movement regimes) — the engine's per-seat computation is expected to diverge sharply even though this story authors a single stable ε for the constraint as the liberal reading itself understands it.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish Israeli citizens and diaspora communities sit near the beneficiary end: the state secures self-determination and refuge without (on this reading) requiring the negation of anyone else's parallel claim — though the reading concedes the parallel claim's non-fulfillment is the arrangement's central unresolved cost. Palestinian refugees and residents of the occupied territories sit near the target end: trapped exit options, powerless structural position, and generational bearing of the transfer (land, movement, statelessness) that the coordination good was purchased against. Palestinian citizens of Israel are dual-positioned — real citizenship benefits alongside structural subordination within a state whose national self-definition is not theirs, hence the secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora statelessness culminating in existential persecution) is independently corroborated as historically live at founding and is not treated here as dead — this is why founding_problem_status is authored 'contested' rather than 'dead': Jewish self-determination advocates and much of the international legal community agree the founding problem was real, but dispute whether the SPECIFIC remedy (open-ended occupation, unresolved refugee return) remains proportionate to it seventy-plus years on. This is exactly the mandatrophy question the schema is built to isolate: a mandate can be genuinely founded in a real problem and still have outlived the form its remedy originally took, without the founding problem itself being fabricated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_solution_proportionality,
    'Has the specific remedy (open-ended occupation, unresolved refugee return, asymmetric citizenship) remained proportionate to the founding problem it was built to solve, or has the remedy''s form outlived the emergency conditions that justified it?',
    'Comparative self-determination case analysis (how other contested-sovereignty resolutions transitioned out of emergency administration), longitudinal review of ICJ/UN rapporteur findings, and negotiated-outcome tracking against declared two-state timelines.',
    'If the remedy is found disproportionate to current conditions, the tangled_rope classification strengthens toward snare on the occupation-specific sub-arrangement even while the founding statehood claim itself remains defensible; if proportionate, the tangled_rope reading is more stable as a durable, not merely transitional, coordination-with-cost structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_solution_proportionality, conceptual, 'Whether the current remedy has outrun the founding problem it was built to solve.').

omega_variable(
    coequal_claim_recognition_asymmetry,
    'Does the liberal nationalist reading''s formal recognition of Palestinian self-determination as co-equal in principle survive contact with seventy-plus years of non-realization in practice, or does the gap between principle and practice indicate the co-equal framing functions as legitimating cover?',
    'Track whether Israeli state policy and mainstream liberal Zionist political platforms have, over the interval, taken concrete steps (settlement freezes, negotiated withdrawal, sovereignty transfer) consistent with co-equal recognition, versus steps (settlement expansion, annexation moves) inconsistent with it.',
    'If practice has systematically diverged from the co-equal principle over time, this reading''s own self-description becomes harder to sustain against convergence with the settler_colonial_reading''s account of the same period; if practice has moved toward realization (however incompletely), the liberal reading''s account of a genuine-but-unfinished coordination project holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coequal_claim_recognition_asymmetry, empirical, 'Whether the gap between co-equal recognition in principle and non-realization in practice undermines the reading''s own coordination claim.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''ancestral homeland'' in this reading''s core premise best understood as a continuous, verifiable indigenous claim (supporting the liberal self-determination framing) or as a claim whose evidentiary and legal weight is itself contested territory between competing national historiographies?',
    'Historical and archaeological scholarship on continuity of Jewish presence and communal identity in the region, weighed against comparative indigeneity criteria used in other self-determination adjudications (ILO 169, UN Declaration on Rights of Indigenous Peoples).',
    'If the ancestral-homeland premise is read as robustly established, the liberal reading''s foundational axiom (jewish_ancestral_continuity_grounds_claim) holds without much erosion; if the premise is read as itself a site of contested historiography, the axiom''s grounding_type shifts from settled fact toward contested empirical claim, weakening this reading''s distinctiveness from the settler_colonial_reading''s framing of the same history.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the ancestral-homeland premise and what each implies for this reading''s axiomatic grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(jewi_tr_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1947, 0.35).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.4).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(jewi_be_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1947, 0.3).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.38).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(jewi_su_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language label 'Zionism' / 'the legitimacy of Jewish statehood' per the epsilon-invariance principle: each reading (liberal_nationalist, settler_colonial, religious_zionist, cultural_zionist, post_zionist) authors its own epsilon, beneficiary/victim structure, and type from within its own normative framework, rather than one story averaging across incompatible premises. The liberal_nationalist_reading is authored here with moderate extractiveness (0.48) reflecting its own expectation of territorial compromise; the settler_colonial_reading (sibling file) is expected to author substantially higher extraction over the same historical arrangement because its premise treats the coordination story as cover rather than as a genuine, if imperfectly realized, dual self-determination structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
