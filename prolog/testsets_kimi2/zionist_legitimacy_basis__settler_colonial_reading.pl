% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis â Settler-Colonial Reading
 *   domain: political_history/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story models the Zionist project as a settler-colonial
 *   legitimacy structure that establishes and maintains an exclusive Jewish
 *   ethno-state through the systematic displacement and subordination of the
 *   indigenous Palestinian population. Under this reading, the constraint is
 *   not a natural or inevitable arrangement but a historically specific
 *   colonial formation: it coordinates the collective action of the settler
 *   population (land allocation, security, national identity) while
 *   asymmetrically extracting land, sovereignty, and political rights from
 *   Palestinians. The claim of national self-determination functions as the
 *   coordination narrative, but the reading holds that displacement is
 *   constitutive rather than incidental. The story is authored from the
 *   settler-colonial analytical seat; it does not attempt to reconcile with
 *   Zionist self-understanding, which is the subject of separate kernel
 *   readings.
 *
 * KEY AGENTS:
 *   - zionist_state_apparatus: Agenda-setter (institutional/arbitrage) â designs and enforces the legal-geographic architecture of displacement
 *   - jewish_settler_society: Primary beneficiary (powerful/mobile) â receives land, rights, and security from the exclusivist national structure
 *   - palestinians_under_occupation: Primary target (powerless/trapped) â bears daily extraction of land, movement, and self-governance under military rule
 *   - palestinian_refugees_and_exiles: Secondary target (powerless/trapped) â bears generational extraction of territory and citizenship through exclusion from return
 *   - palestinian_citizens_of_israel: Same-level lateral target (moderate/constrained) â experiences discrimination and exclusion despite formal citizenship
 *   - international_human_rights_institutions: Analytical observer (institutional/analytical) â measures the gap between self-description and lived structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.85).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis â Settler-Colonial Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'ad014ba5-ff19-4047-a50f-0b7dbae5f5b9').
narrative_ontology:cs_kernel_codification('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', formalized).
narrative_ontology:cs_authority_grounding('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', extraction).
narrative_ontology:cs_interpretation_layer_present('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9').
narrative_ontology:cs_reading_relation('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', foundational, colonial_displacement_constitutive).
narrative_ontology:cs_axiom_status(colonial_displacement_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', colonial_displacement_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', european_settler_sovereignty_project).
narrative_ontology:cs_drift_state('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', contemporary_international_legitimacy_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ad014ba5-ff19-4047-a50f-0b7dbae5f5b9', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_society).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees_and_exiles).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal, military, and demographic architecture of the state: the Law of Return, land regimes, citizenship hierarchies, and the occupation military courts. It designs the policies that transfer land and sovereignty from the indigenous population to the settler society and enforces them through policing, military rule, and border control.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives land allocation, housing subsidies, preferential citizenship rights, security provision, and access to water and planning permits through state institutions. Membership in the national collective is the channel through which these flows arrive; exit means emigrating to third countries where many hold dual nationality.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_society, beneficiary,
    powerful, biographical, mobile, national).

% Descendants of the 1948 displaced, denied the right of return by citizenship and immigration laws, confined to refugee camps in neighboring territories or precarious legal status abroad. Their property and political status were transferred to the settler state, and their exit options are blocked by border regimes and the absence of a sovereign Palestinian authority to issue protective citizenship.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees_and_exiles, payer,
    powerless, generational, trapped, regional).

% Live under military administration in the West Bank and Gaza (or under blockade/siege in Gaza), subject to permit regimes, checkpoints, home demolitions, and land confiscation for settlements. Their movement, water access, and building rights are determined by military orders that serve the settler population; leaving requires resources and visas most do not have.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinians_under_occupation, payer,
    powerless, immediate, trapped, local).

% Hold Israeli citizenship but are structurally excluded from land ownership in most of the state (through the Israel Lands Authority and admission-committee villages), receive fewer municipal resources, and face discriminatory family-unification bans. Their formal political participation is capped by the permanent exclusion of their parties from governing coalitions; emigration means leaving ancestral towns inside the state.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Monitor and report on violations of international humanitarian law, apartheid findings, and refugee rights. They do not enforce but produce the documentary record that measures the gap between the state's self-description and the lived structure on the ground.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_human_rights_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_society).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective settlement, security, and demographic dominance of a Jewish-national polity in a territory with an indigenous Palestinian majority, solving the settler collective-action problem of land clearance, frontier defense, and national institution-building.
% TRANSFER_FUNCTION: Moves land, water, housing, sovereignty, and political rights from indigenous Palestinians to the settler state and its Jewish citizenry through legal confiscation, military order, exile, and residency revocation.
% ABSENT_VOICES: Palestinian refugees are structurally excluded from citizenship and return; anti-Zionist Jewish voices and indigenous Jewish communities opposed to state violence are marginalized from dominant political discourse and institutional memory.
% DISAPPEARANCE_RATIONALE: If the settler-colonial legitimacy structure vanished overnight, the exclusive Jewish ethno-state framework would lose its ideological and legal anchor. Land and citizenship regimes would face fundamental renegotiation, the military-legal architecture of occupation and exclusion would collapse, and millions of refugees would advance restitution claims.
% FOUNDING_PROBLEM: European persecution of Jews and the absence of a territorially sovereign Jewish state to serve as a national haven.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and state institutions attest the founding problem remains live, citing ongoing antisemitism. Palestinian historians, anti-colonial scholars, and refugee testimonies attest the founding problem has been superseded by the reality of dispossession and that the arrangement persists to maintain settler dominance rather than to shelter refugees; independent human rights investigations and comparative colonial historiography from outside the benefiting parties corroborate the displacement reading.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is high because the constraint transfers the majority of the territory's land and resources to the settler society while maintaining millions of Palestinians in exile, occupation, or second-class citizenship. Suppression (0.88) is higher still: the constraint depends on military occupation, legal exclusion of refugees, surveillance, and the suppression of Palestinian political organization to prevent reversal. Theater ratio (0.62 at interval end) captures the performative liberal-democratic institutions and humanitarian discourse that obscure the colonial structure for international audiences. Accessibility collapse (0.75) reflects the near-total closure of one-state and return alternatives under current geopolitical conditions. Resistance (0.70) registers sustained Palestinian popular, armed, and legal resistance plus growing international solidarity. The temporal series show extraction rising as the colonial frontier closes into a permanent regime of domination, with a notable theater peak during the Oslo period when diplomatic performance masked continued settlement expansion.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish settler society experiences the constraint as national self-determination and democratic citizenship, with directionality near the beneficiary pole. The Palestinian seats experience the identical structure as dispossession, occupation, and apartheid, with directionality near the target pole. The international human rights institutions occupy an analytical seat that measures the gap between these experiences. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The zionist_state_apparatus and jewish_settler_society are structurally positioned as beneficiaries of the territorial and demographic transfer, yielding low directionality. The Palestinian seatsârefugees, occupied populations, and discriminated citizensâare structurally targeted for extraction of land and sovereignty, yielding high directionality. The state apparatus is agenda_setter rather than mere beneficiary because it actively designs and enforces the legal-geographic architecture of displacement; the settler society is the human seat that captures the material gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâEuropean persecution of Jews and the need for a safe havenâwas the original mandate. Under this reading, that mandate has been superseded by the logic of colonial state preservation. The state now functions primarily to maintain Jewish demographic supremacy and territorial control rather than to shelter refugees from persecution. The founding problem is contested: Zionist proponents argue antisemitism keeps it live, while the settler-colonial reading argues the mandate has atrophied into a regime of domination. The R5 mismatch (status contested, disappearance rearranges) flags the constraint as a captured coordination structure where the coordination function for settlers and the extraction function against indigenous people are structurally inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_family_ambiguity,
    'Does the settler-colonial reading capture the constitutive structure of the Zionist state, or is it an externally imposed framework that misreads a national liberation project?',
    'Comparative archival analysis against other settler-colonial formations and examination of state planning documents for explicit demographic-engineering intent.',
    'If the reading is constitutive, the constraint remains tangled_rope/snare; if externally imposed, the constraint collapses toward the national liberation reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_family_ambiguity, conceptual, 'Whether the settler-colonial frame is constitutive or imposed').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem of Jewish persecution still live enough to justify continued exclusivist state structure, or has it been superseded by the reality of established statehood and Palestinian displacement?',
    'Empirical assessment of global antisemitism severity versus the structural persistence of Palestinian dispossession and the state''s demographic-control functions.',
    'If the founding problem is dead, the coordination function is hollow and the constraint leans toward snare; if live, some coordination legitimacy persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the original mandate remains live or has atrophied').

omega_variable(
    suppression_scope_ambiguity,
    'Does the measured suppression operate primarily through external military-legal coercion, or has it become partly internalized within Palestinian political subjectivity?',
    'Post-liberation or post-exit suppression trajectory: if coercive compliance persists after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s hold is deeper than institutional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_scope_ambiguity, conceptual, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zionist_settler_colonial_tr_t0, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(zionist_settler_colonial_tr_t19, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 19, 0.3).
narrative_ontology:measurement(zionist_settler_colonial_tr_t38, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 38, 0.4).
narrative_ontology:measurement(zionist_settler_colonial_tr_t48, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 48, 0.55).
narrative_ontology:measurement(zionist_settler_colonial_tr_t58, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 58, 0.5).
narrative_ontology:measurement(zionist_settler_colonial_tr_t68, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 68, 0.58).
narrative_ontology:measurement(zionist_settler_colonial_tr_t76, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 76, 0.62).

% Extraction over time
narrative_ontology:measurement(zionist_settler_colonial_be_t0, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(zionist_settler_colonial_be_t19, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 19, 0.8).
narrative_ontology:measurement(zionist_settler_colonial_be_t38, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 38, 0.78).
narrative_ontology:measurement(zionist_settler_colonial_be_t48, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 48, 0.76).
narrative_ontology:measurement(zionist_settler_colonial_be_t58, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 58, 0.82).
narrative_ontology:measurement(zionist_settler_colonial_be_t68, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 68, 0.84).
narrative_ontology:measurement(zionist_settler_colonial_be_t76, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 76, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(zionist_settler_colonial_su_t0, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(zionist_settler_colonial_su_t19, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 19, 0.82).
narrative_ontology:measurement(zionist_settler_colonial_su_t38, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 38, 0.8).
narrative_ontology:measurement(zionist_settler_colonial_su_t48, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 48, 0.75).
narrative_ontology:measurement(zionist_settler_colonial_su_t58, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 58, 0.85).
narrative_ontology:measurement(zionist_settler_colonial_su_t68, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 68, 0.87).
narrative_ontology:measurement(zionist_settler_colonial_su_t76, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 76, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint decomposes from the colloquial label 'Zionism' by reading the political arrangement through the lens of colonial structure rather than national liberation or religious restoration. The epsilon value is indexed to this reading's referent: the standing arrangement of ethno-state establishment through indigenous displacement. Sibling constraints instantiate the other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
