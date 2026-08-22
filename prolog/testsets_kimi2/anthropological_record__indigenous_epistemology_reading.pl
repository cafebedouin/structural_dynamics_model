% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__indigenous_epistemology_reading, []).

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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Indigenous Epistemology Reading of the Anthropological Record
 *   domain: epistemology/anthropology/philosophy_of_science
 *
 * SUMMARY:
 *   This constraint story models the indigenous_epistemology_reading of the
 *   anthropological_record kernel: the claim that the material record of
 *   human ancestry is properly known through relational continuity with
 *   ancestors and place, mediated by sustained oral tradition, and that both
 *   credentialed scientific and scriptural frameworks must be subordinated to
 *   community authority over ancestral remains. The constraint operates as an
 *   epistemic gate that reallocates control of physical remains and
 *   interpretive legitimacy from holding institutions and researchers to
 *   indigenous community authorities. It is authored as a tangled rope
 *   because it carries a genuine coordination function (protecting sacred
 *   relationships, reversing colonial extraction) alongside asymmetric
 *   extraction (suppressing scientific inquiry, transferring institutional
 *   assets).
 *
 * KEY AGENTS:
 *   - indigenous_communities: Primary agenda_setter and beneficiary (organized/identity_locked) â vest authority in oral tradition and govern repatriation.
 *   - scientific_researchers: Primary payer (institutional/constrained) â lose research access and epistemic standing regarding ancestral remains.
 *   - holding_institutions: Secondary payer (institutional/constrained) â compelled to repatriate collections and cede curatorial authority.
 *   - creationist_advocates: Excluded voice (organized/mobile) â scriptural readings subordinated and excluded from institutional discourse.
 *   - human_rights_observers: Analytical observer (institutional/analytical) â monitor rights compliance without direct stake.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.62).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.7).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Indigenous Epistemology Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/anthropology/philosophy_of_science").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, 'c09fab65-9da7-44d4-bf3c-2cccd857928b').
narrative_ontology:cs_kernel_codification('c09fab65-9da7-44d4-bf3c-2cccd857928b', distributed).
narrative_ontology:cs_authority_grounding('c09fab65-9da7-44d4-bf3c-2cccd857928b', lineage).
narrative_ontology:cs_interpretation_layer_present('c09fab65-9da7-44d4-bf3c-2cccd857928b').
narrative_ontology:cs_reading_relation('c09fab65-9da7-44d4-bf3c-2cccd857928b', anthropological_record__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('c09fab65-9da7-44d4-bf3c-2cccd857928b', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('c09fab65-9da7-44d4-bf3c-2cccd857928b', foundational, oral_tradition_sufficient_for_ancestral_knowledge).
narrative_ontology:cs_axiom_status(oral_tradition_sufficient_for_ancestral_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('c09fab65-9da7-44d4-bf3c-2cccd857928b', oral_tradition_sufficient_for_ancestral_knowledge, conventional).
narrative_ontology:cs_axiom('c09fab65-9da7-44d4-bf3c-2cccd857928b', foundational, community_authority_supersedes_institutional_science).
narrative_ontology:cs_axiom_status(community_authority_supersedes_institutional_science, holdable).
narrative_ontology:cs_axiom_grounding('c09fab65-9da7-44d4-bf3c-2cccd857928b', community_authority_supersedes_institutional_science, conventional).
narrative_ontology:cs_reference_frame('c09fab65-9da7-44d4-bf3c-2cccd857928b', relational_continuity_framework).
narrative_ontology:cs_drift_state('c09fab65-9da7-44d4-bf3c-2cccd857928b', contemporary_repatriation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c09fab65-9da7-44d4-bf3c-2cccd857928b', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, scientific_researchers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, holding_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Vest interpretive and custodial authority over ancestral remains in sustained oral tradition and community protocols. Govern repatriation claims and control access to sacred knowledge. Exit would mean severing relational continuity with ancestors and place, which is structurally unthinkable within the framework.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_communities, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, indigenous_communities, beneficiary).

% Conduct physical anthropological and genomic research on human remains. Under this constraint, access to ancestral remains is contingent on community authorization and oral tradition gates, often resulting in denied research requests and disrupted career pathways.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, scientific_researchers, payer,
    institutional, biographical, constrained, national).

% Museums and universities that curate ancestral remains. Must comply with repatriation requests and transfer physical control and interpretive authority to descendant communities, losing research assets and collection scope.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, holding_institutions, payer,
    institutional, generational, constrained, national).

% Advance scriptural readings of human origins. Structurally excluded from the anthropological record discourse under this reading, as oral tradition and community authority do not admit scriptural epistemic claims.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, creationist_advocates, excluded,
    organized, biographical, mobile, national).

% Monitor repatriation compliance and indigenous rights implementation. They observe the tension between community authority and scientific access without bearing direct costs or collecting direct benefits from the constraint.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains relational continuity between living communities, ancestors, and place by vesting interpretive and custodial authority in sustained oral tradition and community protocols, protecting against epistemic colonialism.
% TRANSFER_FUNCTION: Moves control of ancestral remains and interpretive legitimacy from credentialed scientific institutions and scriptural frameworks to indigenous community authorities.
% ABSENT_VOICES: Creationist interpreters and physical anthropologists who treat material evidence as epistemically autonomous are structurally subordinated; their frameworks are inadmissible without community authorization, leaving them outside the authority structure even when they hold competing claims to the same material.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, museums and laboratories would retain or resume custodial and analytical control of ancestral remains, scientific publication protocols would replace oral tradition as the default epistemic gate, and the legal framework compelling community authority would dissolve â fundamentally reorganizing the anthropology-museum-indigenous political economy.
% FOUNDING_PROBLEM: Colonial extraction of ancestral remains and systematic epistemic displacement of indigenous knowledge systems by scientific and missionary frameworks.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies and indigenous rights tribunals attest that colonial harm persists in institutional collections. Conversely, some museum associations and legal scholars attest that repatriation frameworks have substantially reversed the harm and that the current arrangement reflects settled rights rather than ongoing emergency. No single outside seat corroborates the status unanimously; the dispute itself is evidence.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the substantial transfer of authority and material control from scientific institutions to community authorities. Suppression (0.70) captures the active enforcement of repatriation regimes and the exclusion of unauthorized research. Theater ratio (0.45) acknowledges growing institutional performativity in compliance. Accessibility collapse (0.75) indicates that once oral tradition is accepted as the epistemic gate, scientific alternatives collapse as legitimate options for these remains. Resistance (0.70) reflects sustained opposition from scientific communities and museums. The temporal series track the post-1990 intensification of repatriation law and the corresponding rise in extraction and enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The indigenous community seat should compute as near-beneficiary: the constraint subsidizes their authority and protects ancestral relationships, with identity-locked exit reinforcing low directionality. The scientific researcher and holding institution seats should compute as near-target: they bear the costs of lost access, constrained exit amplifies their effective extraction, and their institutional power is overridden by the constraint's enforcement. The observer seat sits at analytical distance with no directional bias.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are declared beneficiaries because the constraint transfers control of remains and interpretive authority to them; their identity-locked exit places them near d=0.0. Scientific researchers and holding institutions are declared victims (payers) because the constraint extracts research access and curatorial assets from them; their constrained exit places them near d=1.0. Creationist advocates are excluded rather than victims because the constraint does not extract tangible resources from them, merely epistemic standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â colonial extraction of remains and epistemic violence â is treated as contested/live. If that problem were definitively dead and the constraint persisted purely as a gate to exclude outsiders, it would drift toward snare. The temporal measurements show rising extraction, which triggers T17 monitoring. However, the ongoing live status of the founding problem and the genuine coordination of relational continuity keep the classification at tangled rope rather than snare or piton. The framework catches the drift before false naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_ambiguity,
    'Does the indigenous_epistemology_reading of the anthropological_record kernel represent a coexisting epistemic framework alongside naturalist and creationist readings, or does it structurally foreclose them in institutional practice?',
    'Comparative legal analysis across jurisdictions: where indigenous authority is absolute, foreclosure is high; where collaborative frameworks exist, coexistence is demonstrated.',
    'If institutional practice forecloses siblings, the constraint''s suppression and extraction metrics are higher than authored; if genuinely coexisting, it reads closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Structural relationship to sibling kernel readings').

omega_variable(
    coordination_extraction_ambiguity,
    'Is the authority of sustained oral tradition over the anthropological record primarily coordinating indigenous relational continuity, or extracting epistemic authority from scientific institutions?',
    'Ethnographic and institutional analysis of cases where oral tradition gates were relaxed versus strictly enforced, measuring cultural outcomes and scientific exclusion.',
    'If strict gating produces no measurable cultural harm when relaxed, the constraint functions as extraction; if relaxation severs relational continuity, the coordination is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ambiguity, empirical, 'Coordination versus extraction in oral tradition authority').

omega_variable(
    scope_of_oral_tradition_legitimacy,
    'Does community authority extend universally to all ancestral remains, or only where an unbroken oral tradition can be demonstrated?',
    'Jurisprudential review of repatriation cases testing evidentiary standards: courts accepting oral tradition alone versus courts requiring archaeological corroboration.',
    'Universal scope without demonstration requirement would raise extraction by expanding the victim set; limited scope would bound the constraint''s spatial reach and reduce effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_oral_tradition_legitimacy, empirical, 'Scope ambiguity of oral tradition legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t7, anthropological_record__indigenous_epistemology_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement(anth_tr_t14, anthropological_record__indigenous_epistemology_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(anth_tr_t21, anthropological_record__indigenous_epistemology_reading, theater_ratio, 21, 0.33).
narrative_ontology:measurement(anth_tr_t28, anthropological_record__indigenous_epistemology_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement(anth_tr_t35, anthropological_record__indigenous_epistemology_reading, theater_ratio, 35, 0.45).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anth_be_t7, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 7, 0.38).
narrative_ontology:measurement(anth_be_t14, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 14, 0.45).
narrative_ontology:measurement(anth_be_t21, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 21, 0.52).
narrative_ontology:measurement(anth_be_t28, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 28, 0.58).
narrative_ontology:measurement(anth_be_t35, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(anth_su_t7, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 7, 0.42).
narrative_ontology:measurement(anth_su_t14, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 14, 0.5).
narrative_ontology:measurement(anth_su_t21, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 21, 0.58).
narrative_ontology:measurement(anth_su_t28, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(anth_su_t35, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 35, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, creationist_reading).

% DUAL FORMULATION NOTE:
% The anthropological_record kernel decomposes into three structurally distinct readings because their epsilon values, epistemic authorities, and beneficiary/victim structures differ. The naturalist reading centers material evidence and scientific method; the creationist reading centers divine design and scriptural interpretation; this reading centers relational continuity and oral tradition. Each is a separate constraint linked by shared referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
