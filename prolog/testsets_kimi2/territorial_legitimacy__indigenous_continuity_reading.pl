% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Indigenous Continuity Reading of Territorial Legitimacy (1948 as Nakba)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint is the indigenous_continuity_reading of the
 *   territorial_legitimacy kernel. It treats 1948 as the Nakba â an ongoing
 *   settler-colonial dispossession rather than a legitimate partition â and
 *   grounds territorial legitimacy in continuous indigenous Palestinian
 *   habitation and anti-colonial self-determination. Sibling readings include
 *   partition_reading (UN Resolution 181, two-state legitimacy) and
 *   security_necessity_reading (defensive territorial control). The reading
 *   forecloses partition but coexists with security necessity as rival
 *   positions in global discourse. The claim/metric independence is
 *   maintained: the constraint is claimed as tangled_rope because it carries
 *   both genuine coordination for refugee communities and asymmetric
 *   legitimacy extraction from Israeli territorial claims, while the metrics
 *   independently describe a highly extractive, actively enforced
 *   arrangement.
 *
 * KEY AGENTS:
 *   - Palestinian refugee communities (beneficiary/powerless/identity_locked) â receive legitimization of return and sovereignty claims
 *   - Israeli state apparatus (payer/institutional/constrained) â bears total delegitimization of territorial foundation
 *   - Zionist settler society (payer/powerful/identity_locked) â residential and property claims rendered void
 *   - Anti-colonial advocacy networks (agenda_setter/organized/mobile) â construct and enforce the reading globally
 *   - Partition and security-necessity advocates (excluded/moderate/constrained) â structurally excluded from legitimacy discourse
 *   - Critical legal observers (observer/analytical/analytical) â map the reading's reconfiguration of international law categories
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.72).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.75).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Indigenous Continuity Reading of Territorial Legitimacy (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'b7e53f6f-e613-4bf1-88c8-efc24104e538').
narrative_ontology:cs_kernel_codification('b7e53f6f-e613-4bf1-88c8-efc24104e538', distributed).
narrative_ontology:cs_authority_grounding('b7e53f6f-e613-4bf1-88c8-efc24104e538', practice).
narrative_ontology:cs_interpretation_layer_present('b7e53f6f-e613-4bf1-88c8-efc24104e538').
narrative_ontology:cs_reading_relation('b7e53f6f-e613-4bf1-88c8-efc24104e538', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('b7e53f6f-e613-4bf1-88c8-efc24104e538', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('b7e53f6f-e613-4bf1-88c8-efc24104e538', foundational, indigenous_continuity_grounds_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_continuity_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b7e53f6f-e613-4bf1-88c8-efc24104e538', indigenous_continuity_grounds_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('b7e53f6f-e613-4bf1-88c8-efc24104e538', foundational, settler_colonialism_precludes_legitimacy).
narrative_ontology:cs_axiom_status(settler_colonialism_precludes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b7e53f6f-e613-4bf1-88c8-efc24104e538', settler_colonialism_precludes_legitimacy, deontological).
narrative_ontology:cs_reference_frame('b7e53f6f-e613-4bf1-88c8-efc24104e538', continuous_habitation_sovereignty).
narrative_ontology:cs_drift_state('b7e53f6f-e613-4bf1-88c8-efc24104e538', post_oslo_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7e53f6f-e613-4bf1-88c8-efc24104e538', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugee_communities).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, zionist_settler_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diaspora and camp communities for whom this constraint provides the primary legal-moral framework for return claims and sovereignty over all of historic Palestine. Their collective identity is fused with the right of return and the Nakba narrative; exiting this frame means abandoning the ancestral land claim and the refugee status that constitutes their political subjectivity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugee_communities, beneficiary,
    powerless, generational, identity_locked, global).

% Administers the territorial status quo that this reading delegitimizes root-and-branch. The constraint extracts all territorial legitimacy from the state, framing its founding as colonial crime rather than partition. The state cannot exit the frame without dismantling its Zionist constitutional structure and territorial assertions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus, payer,
    institutional, generational, constrained, national).

% Resides in territories classified by this reading as illegitimate colonial possession. Residential, property, and political claims are rendered void by the indigenous continuity principle. Identity is fused with the territorial project; exit would require renouncing the foundational self-understanding of the community.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, zionist_settler_society, payer,
    powerful, biographical, identity_locked, national).

% Constructs, circulates, and enforces the indigenous continuity reading through legal scholarship, solidarity campaigns, and institutional advocacy. They set the discursive agenda for what counts as legitimate territory, who may speak for it, and which frameworks are classified as normalization or colonial apologia.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_advocacy_networks, agenda_setter,
    organized, generational, mobile, global).

% Advocate for a two-state solution based on UN Resolution 181 and 1967 borders. Within the indigenous continuity framework, their position is structurally excluded from legitimacy discourse and classified as accommodation with settler-colonialism.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, partition_advocates, excluded,
    moderate, biographical, constrained, global).

% Frame Israeli territorial control as defensive security necessity requiring strategic depth. Within this reading, their arguments are read as post-hoc justification for colonial expansion; they are excluded from the legitimacy conversation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, security_necessity_advocates, excluded,
    moderate, biographical, constrained, global).

% Analyze the competing legitimacy claims without institutional allegiance to either side. They map how the indigenous continuity reading reconfigures categories of international law, self-determination, and refugee status.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, critical_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Palestinian national identity, refugee return claims, and anti-colonial international solidarity around a single territorial legitimacy framework that resists fragmentation into partition-based or security-based frames.
% TRANSFER_FUNCTION: Transfers moral and legal legitimacy from settler-colonial territorial claims to indigenous continuity claims; moves the locus of sovereignty from partition boundaries to pre-1948 habitation and refugee return.
% ABSENT_VOICES: Liberal Zionist partitionists who accept 1948 as legitimate state-founding and realist security strategists who frame territorial control as defensive necessity are structurally excluded; their presence would require acknowledging the legitimacy of Israeli statehood within any part of historic Palestine.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, Palestinian sovereignty claims would lose their indigenous-continuity grounding, the right of return would become negotiable rather than structural, anti-colonial solidarity networks would fragment, and the partition and security readings would expand to fill the legitimacy vacuum.
% FOUNDING_PROBLEM: Colonial dispossession and the fragmentation of the indigenous Palestinian polity via the 1948 Nakba and ongoing settler-colonial replacement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by UNRWA records documenting ongoing refugee status across generations, independent human rights organizations documenting ongoing displacement and settlement expansion, and anti-colonial legal scholars outside the direct Palestinian beneficiary community. Israeli state historians and liberal international lawyers contest the framing, asserting the problem is resolved by statehood and partition.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint extracts all territorial legitimacy from Israeli claims, framing them as void ab initio. Suppression is high (0.75) because the persistence of this reading depends on actively delegitimizing partition and security-necessity frameworks as colonial or apologetic. Theater ratio is moderate-low (0.26): much of the reading is substantive historical and legal argument, though solidarity performance and institutional careerism introduce some proxy-goal drift. Accessibility collapse is substantial (0.70) because once the indigenous continuity frame is adopted, partition appears as moral impossibility. Resistance is very high (0.78) because the constraint faces institutional opposition from the Israeli state, Western liberal international order, and Arab normalization regimes. Temporal measurements share a single grid and show extraction and suppression intensifying through the Oslo period and normalization eras, with theater peaking during the maximum divergence between rhetoric and institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Israeli state apparatus, Zionist settler society) experience this constraint as a delegitimizing structure that denies their entire political foundation; the beneficiary seat (Palestinian refugee communities) experiences it as restorative coordination that reassembles dispersed national subjectivity; the agenda_setter seat (advocacy networks) experiences it as both genuine solidarity work and professional identity investment. The engine computes these divergent seat types from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugee communities receive low directionality (subsidized by the constraint's legitimizing function). The Israeli state apparatus and Zionist settler society receive high directionality (full targets of legitimacy extraction). Anti-colonial advocacy networks sit near symmetric or moderate agenda-setter directionality â they are not primarily financial beneficiaries but accrue moral and professional capital. The excluded partition and security advocates sit outside the directionality derivation because they are not governed by the constraint but rather excluded from its discursive space.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents the mislabeling of this reading as pure extraction (snare) by preserving its genuine coordination function for refugee communities and dispersed Palestinian identity. It simultaneously prevents mislabeling as pure coordination (rope) by acknowledging the asymmetric legitimacy extraction from Israeli territorial claims and the active suppression of alternative frameworks. The theater ratio guards against piton misclassification: the constraint is not merely performative maintenance of an atrophied function, though performative elements exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_continuity_empirical_status,
    'Is the Israeli state better understood as an ongoing settler-colonial project or as a postcolonial state with security challenges?',
    'Comparative decolonial legal analysis and empirical study of settler demographics, institutional continuity, and territorial control mechanisms over the full interval.',
    'Would shift the extraction-coordination balance: if the colonial framing is overstated, the constraint''s extractiveness is higher than structurally warranted; if understated, the constraint functions more as defensive identity coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_continuity_empirical_status, empirical, 'Empirical ambiguity about whether the target is an ongoing colonial structure or a consolidated state').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression in this reading enforced through structural institutional dominance in specific arenas, or through moral-paradigm internalization that makes partition unthinkable from within the frame?',
    'Discourse analysis tracking how partition advocates are excluded â via institutional gatekeeping or via moral-framework boundary maintenance.',
    'If primarily internalized, the constraint''s effective suppression is higher than structural measures suggest because targets carry the delegitimization with them across contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism in ideological legitimacy constraints').

omega_variable(
    right_of_return_materiality,
    'Is the right of return structurally central as a material demographic transformation or primarily as a symbolic legitimacy anchor for the indigenous continuity frame?',
    'Survey of refugee community prioritization versus elite advocacy discourse; demographic modeling of return scenarios.',
    'If purely symbolic, base extractiveness may be lower than material framing suggests because the extraction is legitimacy rather than territorial dispossession.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_materiality, conceptual, 'Material versus symbolic function of the right of return').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indigenous_continuity_tr_t0, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(indigenous_continuity_tr_t15, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(indigenous_continuity_tr_t30, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(indigenous_continuity_tr_t45, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(indigenous_continuity_tr_t60, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(indigenous_continuity_tr_t76, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 76, 0.26).

% Extraction over time
narrative_ontology:measurement(indigenous_continuity_be_t0, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(indigenous_continuity_be_t15, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(indigenous_continuity_be_t30, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(indigenous_continuity_be_t45, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(indigenous_continuity_be_t60, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(indigenous_continuity_be_t76, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 76, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(indigenous_continuity_su_t0, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(indigenous_continuity_su_t15, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(indigenous_continuity_su_t30, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(indigenous_continuity_su_t45, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(indigenous_continuity_su_t60, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(indigenous_continuity_su_t76, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 76, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, security_necessity_reading).

% DUAL FORMULATION NOTE:
% The kernel 'territorial legitimacy in historic Palestine' decomposes into three structurally distinct readings: indigenous_continuity_reading (legitimacy via pre-1948 habitation and anti-colonial self-determination), partition_reading (legitimacy via UN Resolution 181 and recognized borders), and security_necessity_reading (legitimacy via defensive territorial control). They share the referent (the territorial order in historic Palestine) but author different epsilon values, beneficiary/victim structures, and coordination/extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
