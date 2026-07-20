% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Epistemic Mandate in Religious Communities
 *   domain: epistemological/philosophical/religious
 *
 * SUMMARY:
 *   The constraint is the institutionalized epistemic mandate within certain
 *   religious communities that the anthropological and paleontological record
 *   must be interpreted as revealing divine creation events compatible with a
 *   scriptural timeline or designed complexity. Creationist institutional
 *   authorities enforce this reading through control of educational
 *   curricula, publication venues, and social sanctions, suppressing
 *   materialist accounts and maintaining adjudicative monopoly over human
 *   origins within the community. The constraint coordinates communal
 *   identity and theological anthropology while asymmetrically extracting
 *   epistemic autonomy from dissenting members and youth.
 *
 * KEY AGENTS:
 *   - creationist_institutional_authority: Agenda setter (institutional/arbitrage) â enforces the mandate, captures legitimacy and revenue
 *   - religious_community_adherents: Primary beneficiaries/secondary payers (organized/identity_locked) â receive identity cohesion, pay epistemic constraint
 *   - epistemically_dissenting_members: Primary targets (powerless/identity_locked) â bear silencing, educational limits, social exile
 *   - materialist_scientific_community: Excluded observers (institutional/analytical) â structurally barred from adjudication in this domain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.72).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.8).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Epistemic Mandate in Religious Communities").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemological/philosophical/religious").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, 'ee0595b5-005a-461d-83c2-de18f74bf882').
narrative_ontology:cs_kernel_codification('ee0595b5-005a-461d-83c2-de18f74bf882', fixed_text).
narrative_ontology:cs_authority_grounding('ee0595b5-005a-461d-83c2-de18f74bf882', lineage).
narrative_ontology:cs_interpretation_layer_present('ee0595b5-005a-461d-83c2-de18f74bf882').
narrative_ontology:cs_reading_relation('ee0595b5-005a-461d-83c2-de18f74bf882', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ee0595b5-005a-461d-83c2-de18f74bf882', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('ee0595b5-005a-461d-83c2-de18f74bf882', foundational, divine_causation_required_for_human_origins).
narrative_ontology:cs_axiom_status(divine_causation_required_for_human_origins, holdable).
narrative_ontology:cs_axiom_grounding('ee0595b5-005a-461d-83c2-de18f74bf882', divine_causation_required_for_human_origins, theological).
narrative_ontology:cs_axiom('ee0595b5-005a-461d-83c2-de18f74bf882', foundational, scriptural_timeline_authoritative_for_deep_history).
narrative_ontology:cs_axiom_status(scriptural_timeline_authoritative_for_deep_history, holdable).
narrative_ontology:cs_axiom_grounding('ee0595b5-005a-461d-83c2-de18f74bf882', scriptural_timeline_authoritative_for_deep_history, theological).
narrative_ontology:cs_reference_frame('ee0595b5-005a-461d-83c2-de18f74bf882', scriptural_anthropological_integrity).
narrative_ontology:cs_drift_state('ee0595b5-005a-461d-83c2-de18f74bf882', contemporary_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ee0595b5-005a-461d-83c2-de18f74bf882', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_institutional_authority).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, religious_community_adherents).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, epistemically_dissenting_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, religious_community_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the epistemic rules for the community. Funds creation science research, operates educational institutions and museums, publishes apologetic materials, and disciplines theological dissent. Frames the mandate as protecting faith from corrosive materialism and preserving scriptural authority.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_institutional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive communal belonging, moral order, existential meaning, and a coherent theological anthropology from the creation narrative. Pay by accepting epistemic constraints on education and inquiry, and by suppressing doubts that would threaten family and community standing. Exit means excommunication or family rupture.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_community_adherents, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, religious_community_adherents, payer).

% Experience cognitive dissonance between observed evidence and required belief. Bear costs of silence, social surveillance, and limited educational or career horizons. Cannot openly pursue scientific training or express evolutionary views without risking social exile. Often remain invisible to protect relationships.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, epistemically_dissenting_members, payer,
    powerless, biographical, identity_locked, local).

% Possess methods and evidence that the constraint excludes from adjudicative standing within the religious community. Their epistemic authority is explicitly rejected in this domain, though it remains intact in broader society. They are not party to the constraint's internal negotiations.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, materialist_scientific_community, excluded,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, creationist_institutional_authority).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining communal religious identity, moral order, and existential meaning in the face of materialist accounts of human origins that would dissolve theological anthropology.
% TRANSFER_FUNCTION: Moves epistemic authority from credentialed scientific institutions to religious interpretive authorities, and transfers the costs of doctrinal maintenance â suppressed inquiry, silenced dissent, limited educational horizons â onto dissenting members and youth.
% ABSENT_VOICES: Materialist scientists and theistic evolutionists are structurally excluded from adjudicative roles within the community; dissenting youth and members who accept evolutionary anthropology are present but muted by social sanctions.
% DISAPPEARANCE_RATIONALE: If the mandate to read the record through creationist lenses disappeared, the community would lose its primary boundary marker against secular modernity; educational institutions would reorganize, dissenting voices would emerge, and epistemic authority would shift toward scientific institutions or more moderate theological frameworks.
% FOUNDING_PROBLEM: The challenge of preserving theological anthropology and communal identity after the rise of evolutionary theory and deep-time geology threatened to undermine scriptural authority and the moral order built upon it.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of science and sociologists of religion attest the founding problem's historical reality from outside the benefiting parties; theistic evolutionists attest that the problem has alternate solutions that the constraint actively suppresses.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically redirects epistemic authority and educational resources toward creationist institutions while suppressing alternative frameworks. Suppression (0.80) is high due to active institutional barriers against materialist timelines. Theater_ratio (0.55) reflects the mature creation-science apparatus â journals, museums, and curricula â that perform scientific legitimacy without producing scientific knowledge. Accessibility_collapse (0.72) is high: within the community, alternatives are rendered literally unthinkable or morally toxic. Resistance (0.48) is moderate: external scientific resistance is strong but ineffective inside the community, while internal dissent is largely silenced. The measurement series share one time grid to prevent misaligned temporal substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary coordination protecting a community from epistemic colonization; dissenting members experience the same structure as extraction of their cognitive autonomy. Adherents in the middle experience mixed costs and benefits. The engine computes this divergence from structural data: identical scope but reversed beneficiary/victim declarations produce divergent directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Creationist institutional authorities are structural beneficiaries (low d â the constraint subsidizes their authority and resource base). Religious community adherents are net beneficiaries of coordination but diffuse payers of epistemic constraint; their identity_locked exit modulates directionality toward mid-range. Epistemically dissenting members are declared victims with identity_locked exit, placing them near full target (high d). The materialist scientific community is excluded from the arrangement entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by clearly separating the genuine coordination function (communal identity, existential meaning, moral order) from the extractive mechanism (suppression of dissent, monopoly on education, silencing of internal doubt). A pure snare would lack the genuine coordination benefit adherents report; a pure rope would lack the declared victims and active enforcement. The temporal measurements show theater_ratio rising over the interval, indicating that the coordination story is increasingly performed rather than delivered, but the base coordination function remains structurally present in the community's social fabric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creationist_reading_authority_basis,
    'Does the authority of this reading rest on empirical claims about the anthropological record, or on the social function of preserving communal identity against materialist epistemology?',
    'Historical tracing of the reading''s response to empirical challenges: if it systematically retreats to theological authority when empirical claims fail, the basis is social-functional; if it updates empirical claims, the basis is genuinely epistemic.',
    'A purely social-functional authority would confirm high extraction and identity_coordination classification; a genuinely epistemic basis would require re-evaluation of epsilon and potentially reclassification toward contested mountain or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creationist_reading_authority_basis, conceptual, 'Epistemic vs social-functional authority basis for creationist reading').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional control of education and media) or internalized (adherents cannot contemplate alternatives without identity collapse)?',
    'Post-exit cognitive trajectory: if former adherents regain epistemic openness after leaving the structural environment, suppression was primarily structural; if openness remains impaired, it was internalized.',
    'If internalized, effective suppression exceeds the structural measure and pushes classification toward snare; if structural, the constraint operates more like tangled rope with identifiable institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in creationist epistemic communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arc_cr_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arc_cr_tr_t12, anthropological_record__creationist_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(arc_cr_tr_t24, anthropological_record__creationist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(arc_cr_tr_t36, anthropological_record__creationist_reading, theater_ratio, 36, 0.48).
narrative_ontology:measurement(arc_cr_tr_t48, anthropological_record__creationist_reading, theater_ratio, 48, 0.52).
narrative_ontology:measurement(arc_cr_tr_t60, anthropological_record__creationist_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(arc_cr_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(arc_cr_be_t12, anthropological_record__creationist_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(arc_cr_be_t24, anthropological_record__creationist_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(arc_cr_be_t36, anthropological_record__creationist_reading, base_extractiveness, 36, 0.64).
narrative_ontology:measurement(arc_cr_be_t48, anthropological_record__creationist_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement(arc_cr_be_t60, anthropological_record__creationist_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(arc_cr_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(arc_cr_su_t12, anthropological_record__creationist_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(arc_cr_su_t24, anthropological_record__creationist_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(arc_cr_su_t36, anthropological_record__creationist_reading, suppression_requirement, 36, 0.75).
narrative_ontology:measurement(arc_cr_su_t48, anthropological_record__creationist_reading, suppression_requirement, 48, 0.78).
narrative_ontology:measurement(arc_cr_su_t60, anthropological_record__creationist_reading, suppression_requirement, 60, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is the creationist reading of the anthropological_record kernel. It is one of three structurally distinct constraints emerging from the same kernel: the naturalist reading (materialist origins), the indigenous epistemology reading (relational continuity), and this creationist reading (divine creation events). Each reading has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
