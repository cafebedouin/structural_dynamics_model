% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Symbolic Ritual as Identity Survival (Symbol-Survival Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story instantiates the symbol_survival_reading of the
 *   catastrophe_memory_survival kernel. The reading claims that Jewish
 *   survival across catastrophe depends on the continuity of ritual form
 *   itself — symbolic practice as the vessel of identity. The beneficiary is
 *   rabbinic authority, which maintains interpretive control over what counts
 *   as authentic ritual and who is inside the boundary. The victims are
 *   secularized Jews, intermarried families, and cultural Jews who experience
 *   the ritual boundary as extraction: they must perform alien practices to
 *   gain recognition, or accept exclusion. The constraint is a tangled rope
 *   because it genuinely coordinates collective identity (a real coordination
 *   function for observant members) while simultaneously extracting
 *   compliance from those who do not share the ritual framework. Active
 *   enforcement is required: conversion standards, marriage rules, burial
 *   rights, and institutional recognition are actively policed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.78).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.72).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Symbolic Ritual as Identity Survival (Symbol-Survival Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, 'e1901a79-028d-4951-98e0-4b657ad69cc1').
narrative_ontology:cs_kernel_codification('e1901a79-028d-4951-98e0-4b657ad69cc1', formalized).
narrative_ontology:cs_authority_grounding('e1901a79-028d-4951-98e0-4b657ad69cc1', lineage).
narrative_ontology:cs_interpretation_layer_present('e1901a79-028d-4951-98e0-4b657ad69cc1').
narrative_ontology:cs_reading_relation('e1901a79-028d-4951-98e0-4b657ad69cc1', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1901a79-028d-4951-98e0-4b657ad69cc1', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('e1901a79-028d-4951-98e0-4b657ad69cc1', foundational, ritual_form_continuity_as_survival_itself).
narrative_ontology:cs_axiom_status(ritual_form_continuity_as_survival_itself, holdable).
narrative_ontology:cs_axiom_grounding('e1901a79-028d-4951-98e0-4b657ad69cc1', ritual_form_continuity_as_survival_itself, deontological).
narrative_ontology:cs_axiom('e1901a79-028d-4951-98e0-4b657ad69cc1', secondary, boundary_rigidity_as_identity_preservation).
narrative_ontology:cs_axiom_status(boundary_rigidity_as_identity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('e1901a79-028d-4951-98e0-4b657ad69cc1', boundary_rigidity_as_identity_preservation, conventional).
narrative_ontology:cs_reference_frame('e1901a79-028d-4951-98e0-4b657ad69cc1', rabbinic_survival_paradigm).
narrative_ontology:cs_drift_state('e1901a79-028d-4951-98e0-4b657ad69cc1', contemporary_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1901a79-028d-4951-98e0-4b657ad69cc1', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, intermarried_families).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, cultural_jews_nonobservant).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, cultural_jews_nonobservant).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, observant_community_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, ritual_continuity_as_survival).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, boundary_maintenance_through_symbolic_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretive authority over ritual practice and communal boundary-definition. Collects legitimacy, institutional continuity, and resource flows (donations, affiliation, educational control) from maintaining the claim that survival depends on ritual form preservation. Sets the agenda for what counts as authentic practice and who is inside/outside the boundary.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, beneficiary).

% Identify as Jewish but do not observe ritual law. Experience the boundary-norms as exclusionary: denied full communal recognition, marriage eligibility, burial rights, and institutional access unless they perform ritual observance they experience as alien. Exit from the identity is structurally difficult — Jewish identity is ascribed by descent and history, not chosen; the constraint makes ritual performance the price of recognition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    organized, biographical, identity_locked, global).

% Families with one Jewish partner and one non-Jewish partner. Face ritual boundary enforcement around conversion, children's status, and lifecycle rituals. The constraint extracts compliance (conversion rituals, ritual commitments for children) as the condition for communal acceptance. Exit means abandoning the Jewish partner's heritage and community — a costly identity rupture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, intermarried_families, payer,
    moderate, biographical, constrained, global).

% Engage with Jewish culture, history, and peoplehood without ritual observance. Benefit from the continuity of the tradition (cultural capital, communal infrastructure, historical narrative) but pay extraction when ritual gatekeeping denies them leadership roles, ritual honors, or full membership. Their situation is dual: they draw from the tradition while being measured against its ritual boundary.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, cultural_jews_nonobservant, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, cultural_jews_nonobservant, beneficiary).

% Fully observe ritual practice and experience it as the constitutive structure of their identity and survival. They are coordinated by the constraint — it provides the framework for communal life, transmission, and meaning. They do not experience it as extraction; they experience it as the condition of their existence. Exit is identity-destructive.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, observant_community_members, beneficiary,
    organized, generational, identity_locked, global).

% Study the ritual system from outside its authority claims. Document the boundary-maintenance function, the extraction patterns, and the historical contingency of the ritual forms. Their analytical seat sees the full structure: coordination for some, extraction for others, enforced through identity-locked exit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secular_jewish_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% Descendants of Jews who fully assimilated generations ago. They have no communal connection, no ritual knowledge, and often no self-identification as Jewish — yet the constraint's boundary logic (matrilineal descent, halakhic status) still classifies them as Jewish, subjecting them to ritual obligations they never chose and communal claims they never consented to. They cannot exit an identity they never entered.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, assimilated_diaspora_descendants, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a coherent, trans-generational collective identity across diaspora, persecution, and cultural dissolution by anchoring it in ritual practice that is portable, repeatable, and symbolically dense — solving the coordination problem of 'how does a people survive without territory, sovereignty, or shared language?'
% TRANSFER_FUNCTION: Moves interpretive authority, communal resources, and boundary-definition power from the diverse lived expressions of Jewishness toward a centralized rabbinic authority that defines authentic ritual practice. The extraction is compliance: non-observant Jews must perform ritual they experience as alien to gain recognition; the authority collects legitimacy and institutional control.
% ABSENT_VOICES: Assimilated diaspora descendants (classified as Jewish by descent but with zero communal connection) — they would object to being bound by ritual obligations they never chose, but they are not in the conversation because the constraint defines them as subjects before they can speak. Also: progressive rabbinic voices arguing for boundary permeability — they are structurally excluded from the agenda-setting seat by the authority structure this reading instantiates.
% DISAPPEARANCE_RATIONALE: If the claim 'survival is continuity of ritual practice itself' vanished overnight, the rabbinic authority's interpretive monopoly would collapse, boundary-permeable Judaisms would proliferate rapidly, secular and cultural Jewish identities would gain full communal recognition without ritual performance, and the institutional architecture of Orthodox control over marriage, conversion, and burial in Israel and diaspora would face immediate legitimacy crisis. The world of Jewish communal life would rearrange fundamentally.
% FOUNDING_PROBLEM: Post-exilic and post-Temple survival: how does a people maintain coherent identity and boundary-norms without land, Temple, priesthood, or sovereign institutions? The rabbinic answer: ritual practice as portable homeland, symbolic experience as the survival mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (stateless survival) is attested by historical scholarship outside the benefiting parties — e.g., Seth Schwartz on the rabbinic reorganization of Jewish life, Shaye Cohen on the boundary-shift from ethnic to religious definition. The STATUS of the problem (whether it is still live) is contested: rabbinic authority attests it remains live (assimilation, intermarriage, secularism as existential threats); secular Jewish studies scholars and progressive Jewish movements attest the problem has shifted (survival now depends on cultural vitality, ethical coherence, and voluntary affiliation, not ritual boundary enforcement).
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the ritual boundary extracts ritual compliance from non-observant Jews as the price of communal recognition — a transfer from diverse Jewish expressions to a centralized authority. Suppression is high (0.72) because alternatives (cultural Judaism, secular Jewish identity, permeable boundaries) are actively delegitimized and institutionally blocked, especially in Israel where rabbinic control over personal status is state-enforced. Theater ratio is moderate-low (0.25): the coordination function for observant members is real and not merely performative, but a growing share of enforcement energy defends boundary-rigidity rather than ritual vitality. Accessibility collapse (0.68) reflects that once the ritual boundary is understood as the survival mechanism, permeable alternatives appear as existential betrayal. Resistance (0.55) is significant: secular Jewish movements, progressive denominations, and Israeli civil society actively contest the boundary.
 *
 * PERSPECTIVAL GAP:
 *   The observant beneficiary seat experiences this constraint as genuine coordination — the ritual system IS their survival. The payer seats (secularized, intermarried, cultural) experience the SAME constraint as enforced extraction — ritual performance demanded as entry fee to an identity they already inhabit by descent and history. The agenda-setter seat (rabbinic authority) experiences it as necessary guardianship. The engine computes this divergence; the authored claim (tangled_rope) names the structural reality that contains both experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority sits at d ≈ 0.15 (full beneficiary: collects legitimacy, resources, and control; analytical exit). Observant community members sit at d ≈ 0.25 (beneficiary: coordinated by the constraint, identity-locked exit makes them structural beneficiaries). Secularized Jews and intermarried families sit at d ≈ 0.85 (targets: bear the extraction, identity-locked or constrained exit traps them). Cultural Jews sit at d ≈ 0.55 (dual: benefit from tradition's continuity but pay extraction at the boundary). Assimilated descendants sit at d ≈ 0.95 (trapped: subject to classification without consent). The engine derives these from beneficiary/victim declarations + exit_options + power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stateless survival) was real and the ritual solution was a genuine coordination innovation. But the problem has mutated: statelessness is largely solved (Israel exists), yet the ritual boundary has hardened rather than softened. The constraint now extracts from the very diversity that characterizes contemporary Jewish life. This is mandatrophy: the mandate (boundary-maintenance for survival) has outlived its function (survival no longer depends on ritual rigidity) but the constraint persists and intensifies extraction. The 'survival' justification now serves as cover for authority preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_practical_survival,
    'Does Jewish survival empirically depend on ritual form continuity, or on the practical knowledge and adaptive capacities that ritual may encode?',
    'Comparative historical analysis: do communities that preserved ritual form but lost practical knowledge survive? Do communities that lost ritual form but retained practical knowledge survive? The hybrid_encoding_reading predicts both are necessary; this reading predicts form is sufficient; competence_transmission_reading predicts practical knowledge is sufficient.',
    'If survival depends on practical knowledge, this reading''s high ε for form-preservation is extractive overhead — the constraint preserves the wrong thing. If form is sufficient, the extraction is the price of coordination. This is the kernel''s central empirical dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_vs_practical_survival, empirical, 'Whether the constraint''s coordination function (survival) is actually served by the ritual forms it enforces, or by something else the forms may or may not carry.').

omega_variable(
    boundary_rigidity_as_survival_mechanism,
    'Is the rigidity of the ritual boundary (exclusion of non-observant, intermarried, assimilated) a necessary condition for the coordination function, or an extractive addition that serves authority interests?',
    'Natural experiment: progressive Jewish communities with permeable boundaries (Reform, Reconstructionist, secular humanist) — do they survive, thrive, or dissolve? Longitudinal demographic and institutional vitality data across boundary-permeability gradients.',
    'If permeable boundaries survive, the rigidity is extractive (serves authority, not survival). If only rigid boundaries survive, the extraction is the coordination cost. This determines whether the constraint is tangled_rope (coordination + extraction) or snare (extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_rigidity_as_survival_mechanism, empirical, 'Whether the constraint''s asymmetric extraction (boundary rigidity) is structurally necessary for its coordination function.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the catastrophe_memory_survival kernel best framed as (a) a system of ritual practice, (b) a system of textual interpretation, or (c) a system of communal boundary-maintenance?',
    'Structural analysis: each framing produces different ε values, different beneficiary/victim sets, and different constraint classifications. The framings are not empirically distinguishable — they are constitutive choices about what the kernel IS.',
    'Framing (a) favors this reading (symbol_survival). Framing (b) favors competence_transmission_reading. Framing (c) makes the extraction visible as the kernel''s core function. The choice of framing is not resolved by evidence — it is the analytic decision that generates the constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Irreducible framing ambiguity in the kernel itself — the constraint story''s structure depends on which framing the author adopts.').

omega_variable(
    identity_locked_exit_mechanism,
    'What specific mechanism makes exit identity-destructive for secularized Jews — is it internalized (self-concept fused with communal recognition), structural (institutional gatekeeping over marriage/burial/status), or relational (kinship ties that would rupture)?',
    'Qualitative interview studies with secularized Jews navigating communal recognition: map the phenomenology of exit — what would they lose, what do they fear, what is the lived experience of the boundary?',
    'If primarily internalized, the constraint''s suppression is partly carried by the target (harder to measure, harder to remedy). If primarily structural, the extraction is more directly addressable through institutional reform. If both, the constraint operates on two suppression channels simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Mechanism of identity-locked exit for secularized Jews — structural vs. internalized vs. relational suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_tr_t20, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_tr_t60, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_tr_t80, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_tr_t100, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_be_t20, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_be_t60, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_be_t80, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_be_t100, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_su_t20, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_su_t60, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_su_t80, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement(catastrophe_memory_survival__symbol_survival_reading_su_t100, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__symbol_survival_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, israeli_rabbinic_monopoly_personal_status).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, diaspora_orthodox_institutional_control).

% DUAL FORMULATION NOTE:
% This constraint (symbol_survival_reading) and its siblings (competence_transmission_reading, hybrid_encoding_reading) form a constraint family decomposing the 'catastrophe_memory_survival' kernel. They share the referent (Jewish survival across catastrophe) but instantiate different constraints with different ε, different beneficiary/victim structures, and different types. The symbol_survival_reading has highest ε for form-preservation; competence_transmission_reading has lower ε (practical knowledge is more shareable); hybrid_encoding_reading sits between. All three link to institutional constraints (Israeli rabbinic monopoly, diaspora Orthodox control) that implement the boundary enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, institutional, 0.15).
constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, organized, 0.25).
constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, moderate, 0.55).
constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
