% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigenous Return and Decolonization (Indigenous Return Reading)
 *   domain: political_philosophy/nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates the indigenous-return reading of the
 *   jewish_self_determination kernel. It asserts that Jewish people's
 *   historical and unbroken connection to the land (Eretz Yisrael/Palestine)
 *   makes Jewish return to territorial sovereignty an act of decolonization,
 *   not colonization. The reading frames Zionism as restoration of indigenous
 *   rights after diaspora, grounded in 2,500+ years of historical, legal, and
 *   religious continuity. Under this reading, Palestinian presence is
 *   reframed as later arrival or subordinate co-indigenous claim, and the
 *   territorial project is presented as recovery of stolen sovereignty. This
 *   reading is one of five live readings of the contested kernel; it coexists
 *   with diasporist, liberal-nationalist, religious-covenant, and
 *   settler-colonial readings. The author generates THIS reading cleanly, not
 *   averaging across readings or hedging the frame.
 *
 * KEY AGENTS:
 *   - Jewish claimants to ancestral land: agenda-setters and beneficiaries; frame the narrative of indigenous return and set the terms of self-determination claims
 *   - Palestinian presence in territory: excluded from agenda-setting; reframed as later arrival or subordinate co-indigenous claim under this reading
 *   - Diasporist Jewish communities: partially excluded; their argument for diaspora pluralism is rejected as inauthentic or unsafe
 *   - International law and indigenous-rights norms: observer seat; would evaluate whether unbroken connection and indigenous status meet established standards
 *   - Academic historical scholarship: observer seat; would assess competing historical claims and continuity of presence across centuries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.78).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.71).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigenous Return and Decolonization (Indigenous Return Reading)").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '74a46129-9cee-4d4f-81d4-71d78cc4cdc4').
narrative_ontology:cs_kernel_codification('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', fixed_text).
narrative_ontology:cs_authority_grounding('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', lineage).
narrative_ontology:cs_interpretation_layer_present('74a46129-9cee-4d4f-81d4-71d78cc4cdc4').
narrative_ontology:cs_reading_relation('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', jewish_self_determination__religious_covenant_reading, influences).
narrative_ontology:cs_axiom('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', foundational, jewish_people_indigenous_to_levant).
narrative_ontology:cs_axiom_status(jewish_people_indigenous_to_levant, holdable).
narrative_ontology:cs_axiom_grounding('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', jewish_people_indigenous_to_levant, empirically_contingent).
narrative_ontology:cs_axiom('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', foundational, zionism_is_decolonization_not_colonization).
narrative_ontology:cs_axiom_status(zionism_is_decolonization_not_colonization, holdable).
narrative_ontology:cs_axiom_grounding('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', zionism_is_decolonization_not_colonization, deontological).
narrative_ontology:cs_reference_frame('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', jewish_indigenous_sovereignty_in_ancestral_land).
narrative_ontology:cs_drift_state('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', contemporary_postcolonial_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('74a46129-9cee-4d4f-81d4-71d78cc4cdc4', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, diasporist_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and maintain the narrative frame that Jewish people are indigenous to the land with unbroken historical and religious connection, making territorial sovereignty a matter of decolonization and indigenous self-determination. They control the canonical definition of Jewish interest and authenticate claims to land. Their situation is constituted by the reading itself—Jewishness becomes fused with territorial claim and indigenous status. Exit would require abandoning the foundational claim and accepting alternative framings (diaspora pluralism, liberal nationalism, or conceding settler-colonial critique). The identity-lock is total: institutional Judaism has embedded this reading into liturgy, education, and collective memory; departure is psychologically experienced as apostasy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, agenda_setter,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, beneficiary).

% Palestinian communities are structurally excluded from the agenda-setting conversation about indigeneity and restoration. Under this reading's frame, Palestinian presence is reframed as later arrival, subordinate co-indigenous claim, or even as external implantation subsequent to Jewish exodus. Palestinians have no voice in defining the terms of indigeneity, no seat at the table for negotiating what 'unbroken connection' means, and are trapped in a situation where the very framework denies their agency and historical claims. Their exit option would be to leave the territory—they are trapped because accepting the reading means accepting their subordination or erasure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_presence_in_territory, excluded,
    powerless, biographical, trapped, regional).

% Bear the costs of this reading through militarization of Jewish identity, securitization of Jewish collective survival, and subordination of their own reading of Jewish interest (diaspora pluralism and minority-rights security). They argue that Jewish survival is better secured through international law, minority-rights protections, and pluralistic democracies than through territorial sovereignty tied to a militarized state. Their exclusion from the agenda-setting conversation means their reading is treated as inauthentic or self-destructive. Their exit costs are high because rejecting this reading means fracturing Jewish collective identity as it is now institutionalized; they cannot exit without losing communal belonging.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, diasporist_jewish_communities, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, diasporist_jewish_communities, excluded).

% Other groups with historical and contemporary presence in the territory—including but not limited to Palestinian communities—who might assert their own indigeneity are structurally excluded from the reading's frame or subordinated within it. The binary treatment of indigeneity (Jewish vs. later arrival) leaves no logical space for competing equal claims. Any group asserting indigenous status in the same territory is automatically positioned as either a threat to Jewish indigeneity or a secondary claimant whose interests are subordinate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, competing_indigenous_claimants_regional, excluded,
    moderate, generational, trapped, regional).

% Neutral observer of whether this reading's claims about indigenous status, unbroken connection, and singular indigeneity align with internationally established definitions and evidentiary standards for indigeneity. Would assess: (a) whether continuous physical presence is required or if legal/religious continuity suffices; (b) whether indigeneity is binary or can be plural; (c) how the regime treats competing historical claims and intervening populations; (d) whether this reading meets the evidentiary burden the regime applies universally.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_indigenous_rights_regime, observer,
    institutional, generational, analytical, global).

% Analytical seat evaluating the empirical historical record: the degree and nature of Jewish presence and connection across 2,500+ years of the relevant period; the status of that connection during diaspora (religious, legal, cultural vs. physical); the dating, density, and status of Palestinian settlement patterns across the same interval; whether the archaeological and textual record supports continuous Jewish presence, sporadic presence, or absence; competing scholarly interpretations of what constitutes 'unbroken connection' across diaspora. This seat does not render judgment on which reading is 'correct' but documents the empirical record and its contestations.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, academic_historical_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unites Jewish communities globally around a shared narrative of indigenous return and restoration, providing collective identity and political legitimacy for state formation grounded in historical, religious, and legal continuity to the land. Solves the coordination problem of how a dispersed diaspora population reclaims sovereignty when majority populations in host countries deny collective self-determination.
% TRANSFER_FUNCTION: Transfers authority to define Jewish self-determination from dispersed diaspora communities, host-nation majorities, and international bodies to a territorially sovereign state that claims indigenous legitimacy. Transfers control of Jewish survival narratives from minority-rights frameworks to national-security frameworks centered on territorial control and military power.
% ABSENT_VOICES: Palestinian communities are entirely excluded from the reading's agenda-setting conversation about indigeneity, connection to land, and restoration of sovereignty. Diasporist Jewish communities arguing that survival is better secured through diaspora pluralism and minority-rights protections are excluded as inauthentic. Scholars and indigenous-rights advocates emphasizing competing claims, burden of proof for unbroken connection, and the possibility of plural indigeneity are structurally outside the frame. Religious pluralists within Judaism who ground claims in covenant rather than indigenous history are subordinated.
% DISAPPEARANCE_RATIONALE: This reading's proponents argue: if this frame disappeared, Jewish collective identity would lose its territorial anchor and foundational narrative, leading to diaspora fragmentation and vulnerability to majoritarianism; Jewish self-determination would be permanently subordinated to non-Jewish political systems; the historical connection to the land would be forgotten and erased. Critics and external observers argue: if this frame disappeared, the territorial conflict would remain but would be framed differently—not as indigenous recovery vs. settler colonization but as two populations with competing historical and contemporary claims negotiating coexistence; Jewish identity would persist through diaspora pluralism, minority-rights protections, and cultural continuity without requiring territorial sovereignty; securitization of the region and militarization of Jewish identity would decrease.
% FOUNDING_PROBLEM: Jewish communities faced existential vulnerability and marginalization in diaspora across centuries: persecution, legal disabilities, exclusion from majority political participation, denial of collective self-determination. Historical dispersion from the land created a condition of permanent minority status and dependence on host-nation goodwill. The foundational claim: indigenous status and unbroken connection to the land provide a path to secure self-determination through territorial restoration.
% FOUNDING_PROBLEM_CORROBORATION: This reading's proponents attest the founding problem is live and urgent: diaspora antisemitism remains active, Jewish communities continue to face marginalization and violence in many contexts, and territorial sovereignty offers the only reliable guarantee of collective security and dignity. Diasporist Jewish communities and external observers attest the founding problem is real but argue it is not necessarily solved by this reading's solution: international minority-rights law, pluralistic liberal democracies, and robust legal protections have proven effective for other diaspora populations; territorial sovereignty tied to a militarized state has created new foundational problems (territorial conflict, securitization of the region, subordination of Palestinian claims) that arguably exceed the original founding problem in severity. Historical scholarship corroborates that diaspora vulnerability and marginalization are documented realities; scholarship on whether territorial sovereignty is necessary (vs. sufficient or even helpful) for addressing that vulnerability remains contested. No corroboration from outside the benefiting parties exists for the claim that indigenous status and unbroken connection are established historical facts; external scholars remain divided on this question.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the reading's acceptance requires accepting a particular historical interpretation and rejecting competing interpretations, and that acceptance is defended by suppressing alternative narratives and excluding competing claimants from voice. The reading claims to be a factual account (mountain logic: indigenous status is a binary fact), but the extractiveness metric reflects the reading's actual operation: it functions as a constraining narrative that excludes Palestinian agency and diasporist Jewish alternatives. Suppression is high (0.71) because the reading requires active suppression of competing claims about continuous Palestinian presence, competing definitions of indigeneity, and diasporist alternatives. Theater is moderate (0.42): the reading's historical and legal scholarship is genuinely engaged, but an increasing share of enforcement activity defends the narrative against contrary evidence rather than substantiating it. The measurement series shows extractiveness and suppression rising over the interval as the reading's operation becomes more dependent on excluding evidence and suppressing competing voices (from ~0.55-0.52 to ~0.78-0.71 across 30 time units), with theater ratio stabilizing around 0.42 by endpoint, suggesting the functional-to-performative ratio has reached a steady state where historical scholarship and legal argument sustain approximately 58% of the constraint's operation and defensive exclusion of alternatives sustains 42%.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Jewish claimants to ancestral land) experiences this reading as genuine coordination and indigenous self-determination—a recovery of stolen sovereignty, a foundation for collective security and dignity. From their seat, the constraint is rope: solving a real coordination problem (how diaspora communities achieve self-determination) and benefiting them. The excluded seats (Palestinian communities, diasporist Jewish voices, competing claimants) experience it as extractive: their presence and agency are reframed as secondary or erased, and their exit options are collapsed. From their seats, the constraint is snare or tangled_rope: the coordination story (indigenous recovery) is cover for extraction (consolidation of power, exclusion of competing claims). The engine computes this divergence from the structural data: the reading's beneficiary list excludes Palestinians and diasporists despite shaping their situation; exclusion is the enforcement object itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish claimants to ancestral land are the agenda-setters (d ~ 0.15-0.25: they set the frame, benefit from its acceptance, and face low exit costs if it is rejected—they can maintain identity through diaspora pluralism or religious practice). Palestinians are excluded entirely from the reading's framework (not d~1.0 as 'targets' but structurally erased; if they could be brought inside the reading's logic, they would be reclassified as subordinate co-indigenous claimants, which keeps them in a weakened position with high d). Diasporist Jewish communities are the effective payers (d~0.75-0.85: they bear the cost of militarized identity, securitization of Jewish survival, and rejection of their own reading of Jewish interest; their exit costs are high because rejecting the reading means fracturing Jewish collective identity as it is now framed). International law observers are analytical (d=0.5 by definition). The directionality asymmetry is the constraint's core structure: one group sets the frame and benefits from it, another group is excluded and rendered powerless within it, and a third group bears the costs of the frame's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—Jewish vulnerability in diaspora and lack of collective self-determination—is contested. The reading's proponents argue it is live and urgent (diaspora antisemitism, marginalization, vulnerability). Critics and external observers argue the problem is real but that this reading's solution has created new foundational problems: territorial conflict, securitization of an entire region, subordination of Palestinian claims, and militarization of Jewish identity. The reading fails the mandatrophy test from the observer seat: the founding problem is not transparently dead, but the solution's costs exceed the founding problem's severity by a wide margin, and the reading requires active suppression of competing narratives to defend the connection between problem and solution. From the diasporist seat, mandatrophy is clear: the founding problem (diaspora vulnerability) is solved by international minority-rights law and pluralistic liberal democracies, not by territorial sovereignty tied to a militarized state, yet the reading persists by redefining Jewish interest to require sovereignty. This is classic mandatrophy at the ideological level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_indigenous_return_reading_contest,
    'Is this reading one of several defensible interpretations of the jewish_self_determination kernel, or does the empirical historical record determine one reading as correct?',
    'Consensus emergence in academic historical scholarship on: (a) timeline and density of Jewish presence across centuries; (b) definition and measurement of ''unbroken connection'' during diaspora; (c) timeline and status of Palestinian presence; (d) international law on whether indigeneity can be plural or binary. If consensus emerges decisively on any of these, the kernel collapses and one reading becomes determinable.',
    'If the kernel collapses, this reading either becomes certified (extractiveness drops toward mountain), rejected (extractiveness rises sharply toward snare), or determinable-as-rope. If the kernel remains open, extractiveness stays contested and the reading remains live as one of five coexisting positions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_indigenous_return_reading_contest, conceptual, 'Whether the kernel admits genuinely multiple readings or whether empirical facts determine one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__indigenous_return_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(jewi_tr_t5, jewish_self_determination__indigenous_return_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__indigenous_return_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(jewi_tr_t15, jewish_self_determination__indigenous_return_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__indigenous_return_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(jewi_tr_t25, jewish_self_determination__indigenous_return_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__indigenous_return_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__indigenous_return_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jewi_be_t5, jewish_self_determination__indigenous_return_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__indigenous_return_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(jewi_be_t15, jewish_self_determination__indigenous_return_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__indigenous_return_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(jewi_be_t25, jewish_self_determination__indigenous_return_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__indigenous_return_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__indigenous_return_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(jewi_su_t5, jewish_self_determination__indigenous_return_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__indigenous_return_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(jewi_su_t15, jewish_self_determination__indigenous_return_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__indigenous_return_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(jewi_su_t25, jewish_self_determination__indigenous_return_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__indigenous_return_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__indigenous_return_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, palestinian_self_determination__indigenous_presence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_self_determination kernel. It asserts indigenous status and unbroken connection as the foundation of claim. Sibling readings contest this foundation: settler-colonial reading denies indigeneity and asserts external colonization; diasporist reading argues against territorial sovereignty; liberal-nationalist reading claims equal self-determination without indigeneity; religious-covenant reading grounds claim in divine covenant rather than secular history. Each reading has its own ε, its own beneficiary/victim structure, and its own classification. The network links encode the kernel structure: multiple readings of one contested commitment, each constraining the others' viability through different mechanisms (foreclosure, coexistence, influence). Consumers reading this family should note: all five readings share the same empirical referent (the territory, the historical record, the present political reality) but interpret it through different normative frames. The frame determines which agents are beneficiaries, which are excluded, what counts as evidence, and what the constraint enforces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
