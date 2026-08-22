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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Anthropological Record as Relational Continuity Knowable via Oral Tradition (Indigenous Epistemology Reading)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story instantiates the indigenous epistemology reading of the
 *   anthropological-record kernel: the archaeological and skeletal record is
 *   read as evidence of relational continuity between a living people, their
 *   ancestors, and a place, and that continuity is knowable through sustained
 *   oral tradition rather than (or prior to) material dating and genetic
 *   analysis. Under this reading, both the naturalist framework (which would
 *   settle origin questions by scientific method alone) and the creationist
 *   framework (which would settle them by scriptural timeline) are
 *   subordinated whenever they conflict with a nation's own account of its
 *   continuity. The reading has become progressively more consequential as
 *   legal recognition (statutory consultation requirements, repatriation
 *   review processes) has hardened it from a moral claim into an enforceable
 *   one, which is what the extractiveness and suppression trajectories trace.
 *   This is a kernel-reading story: the sibling readings (naturalist_reading,
 *   creationist_reading) are separate constraint files with their own ε,
 *   beneficiaries, and victims — they are not folded into this one's
 *   classification per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - indigenous_nations_asserting_continuity: primary agenda-setter and beneficiary (organized/identity_locked) — asserts and administers the continuity claim
 *   - tribal_historic_preservation_offices: institutional beneficiary and co-agenda-setter — exercises statutory consultation/veto authority
 *   - academic_archaeologists_denied_access: primary payer (moderate/constrained) — loses research access once continuity is recognized
 *   - museum_curators_holding_disputed_collections: payer (moderate/constrained) — bears repatriation and deaccessioning costs
 *   - naturalist_framework_researchers: excluded — credentialed method subordinated when it conflicts with the continuity claim
 *   - scriptural_framework_communities: excluded — scriptural timeline has no standing over the affected nation's account
 *   - federal_and_state_agencies: analytical observer and partial agenda-setter — administers the legal process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.63).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.58).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Anthropological Record as Relational Continuity Knowable via Oral Tradition (Indigenous Epistemology Reading)").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, 'ce741d7e-48fa-412e-9265-c94fe2c7f081').
narrative_ontology:cs_kernel_codification('ce741d7e-48fa-412e-9265-c94fe2c7f081', distributed).
narrative_ontology:cs_authority_grounding('ce741d7e-48fa-412e-9265-c94fe2c7f081', practice).
narrative_ontology:cs_interpretation_layer_present('ce741d7e-48fa-412e-9265-c94fe2c7f081').
narrative_ontology:cs_reading_relation('ce741d7e-48fa-412e-9265-c94fe2c7f081', anthropological_record__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('ce741d7e-48fa-412e-9265-c94fe2c7f081', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('ce741d7e-48fa-412e-9265-c94fe2c7f081', foundational, oral_tradition_constitutes_valid_historical_record).
narrative_ontology:cs_axiom_status(oral_tradition_constitutes_valid_historical_record, holdable).
narrative_ontology:cs_axiom_grounding('ce741d7e-48fa-412e-9265-c94fe2c7f081', oral_tradition_constitutes_valid_historical_record, conventional).
narrative_ontology:cs_axiom('ce741d7e-48fa-412e-9265-c94fe2c7f081', foundational, community_authority_supersedes_external_credentialed_or_scriptural_adjudication).
narrative_ontology:cs_axiom_status(community_authority_supersedes_external_credentialed_or_scriptural_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('ce741d7e-48fa-412e-9265-c94fe2c7f081', community_authority_supersedes_external_credentialed_or_scriptural_adjudication, deontological).
narrative_ontology:cs_reference_frame('ce741d7e-48fa-412e-9265-c94fe2c7f081', unbroken_relational_continuity_with_ancestors_and_land).
narrative_ontology:cs_drift_state('ce741d7e-48fa-412e-9265-c94fe2c7f081', post_repatriation_statute_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ce741d7e-48fa-412e-9265-c94fe2c7f081', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_nations_asserting_continuity).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, tribal_historic_preservation_offices).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, academic_archaeologists_denied_access).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, museum_curators_holding_disputed_collections).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, community_authority_over_ancestral_remains).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, oral_tradition_as_valid_evidentiary_record).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert that oral tradition documenting generational continuity with a place and its ancestors constitutes the authoritative record, and that this authority overrides purely material analysis. They administer repatriation claims, control access to sacred sites and remains, and set the terms under which any material evidence may be interpreted or handled. Their continuity claim is inseparable from their standing as a people, so exit from the epistemic framework is not a live option for them.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_nations_asserting_continuity, agenda_setter,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, indigenous_nations_asserting_continuity, beneficiary).

% Operate under NAGPRA-style statutory authority to require consultation and consent before excavation, analysis, or display of ancestral remains and objects. They gain formal veto power and reburial authority; their leverage depends on courts and agencies continuing to recognize oral-tradition-based continuity claims as legally sufficient.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, tribal_historic_preservation_offices, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, tribal_historic_preservation_offices, agenda_setter).

% Have collections, sites, or remains reclassified as off-limits to further material study once a nation's oral-tradition-based continuity claim is recognized, sometimes before radiocarbon dating, isotopic, or genetic analysis is completed. They bear a direct cost in blocked or destroyed research programs and cannot appeal to material evidentiary standards alone once community authority is asserted; their professional recourse is limited to litigation or negotiated partial access.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, academic_archaeologists_denied_access, payer,
    moderate, biographical, constrained, national).

% Hold collections acquired historically, often without consent, and must now repatriate or renegotiate custody once continuity is established through oral tradition rather than (or in addition to) documentary provenance. They absorb deaccessioning costs, loss of research and exhibition holdings, and reputational exposure for prior acquisition practices.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, museum_curators_holding_disputed_collections, payer,
    moderate, biographical, constrained, national).

% Would argue that migration and genetic/archaeological dating evidence should independently settle questions of origin and population history regardless of oral tradition's account of relational continuity. Under this reading their credentialed method is explicitly subordinated to community authority when the two conflict, and they are not the parties who adjudicate the continuity claim.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, naturalist_framework_researchers, excluded,
    institutional, generational, constrained, global).

% Hold that the record should be read against a divine creation timeline; under this reading their framework has no privileged standing over the affected nation's own continuity account, and they are excluded from adjudicating claims that are not theirs to make.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, scriptural_framework_communities, excluded,
    organized, civilizational, constrained, national).

% Administer the legal processes (consultation requirements, repatriation review boards) that determine when oral-tradition-based continuity claims trigger binding authority over remains and objects. They referee disputes between nations, institutions, and researchers and can alter the weight given to oral tradition through rulemaking.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, federal_and_state_agencies, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, federal_and_state_agencies, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, indigenous_nations_asserting_continuity).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single recognized channel — the affected nation's own oral tradition and self-declared continuity — through which disputes over ancestral remains, sacred sites, and cultural patrimony are resolved, replacing an unregulated free-for-all of excavation, display, and study.
% TRANSFER_FUNCTION: Moves control over remains, sites, and interpretive authority away from academic and museum institutions that formerly held it by default and toward the indigenous nations asserting continuity; moves research access, specimens, and collection holdings from researchers/curators to nations exercising repatriation and reburial rights.
% ABSENT_VOICES: Naturalist-framework researchers and scriptural-framework communities are structurally excluded from adjudicating a given nation's continuity claim under this reading — they may contest the underlying facts in public discourse but hold no standing within the recognized decision process once oral tradition establishes continuity.
% DISAPPEARANCE_RATIONALE: If oral-tradition-based continuity claims lost legal and institutional recognition overnight, repatriation statutes would lose their evidentiary basis, museums and universities would regain unilateral custody and research access to contested collections, and nations currently exercising consultation authority would lose their principal lever over ancestral remains and sites.
% FOUNDING_PROBLEM: Indigenous nations' accounts of continuous relationship with ancestors and land were historically dismissed as myth or folklore by colonial and academic institutions, which used that dismissal to justify excavation, removal, and display of remains and objects without consent.
% FOUNDING_PROBLEM_CORROBORATION: Tribal historic preservation offices and allied legal scholars attest the problem remains live — dismissal of oral tradition as evidence continues in contested cases. Some archaeologists and museum professionals, speaking from outside the benefiting nations, attest that the founding problem (categorical dismissal) has been substantially remedied by existing statutory consultation requirements, and that current claims sometimes extend beyond correcting historical dismissal into blocking evidence-based inquiry altogether; this is a genuine outside-seat dispute, not merely benefiting-party assertion.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.63, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as substantial (0.63 by interval end) because recognizing oral-tradition-based continuity as dispositive transfers material control (remains, sites, research access) away from institutions that formerly held it, and this transfer is enforced through statute and litigation, not voluntary agreement. Suppression (0.58) reflects the real coercive apparatus behind the claim — legal consultation mandates and site-access denial — but is authored below extraction because the underlying continuity claim itself is not coercively manufactured; the coercion is in how absolutely it is made to override competing evidentiary frameworks once asserted. Theater ratio is modest and rising (0.30) — most consultation activity is functionally consequential, but a growing share of institutional compliance work (advisory committees, symbolic consultation) is performative relative to actual decision authority. Accessibility collapse (0.42) is moderate: material-evidence methods are not eliminated, only subordinated in specific disputed cases, so alternatives persist outside the contested cases. Resistance (0.72) is high because archaeological and museum professional communities actively contest the subordination of material method through litigation, professional association statements, and public argument.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations and their preservation offices are coded as beneficiaries/agenda-setters: the reading directly increases their control over remains, sites, and interpretive authority, and — critically — their exit from the epistemic framework is not available, since the continuity claim is constitutive of their peoplehood (identity_locked, not merely constrained). Archaeologists and curators are coded as payers: material control and research access move away from them under enforcement they cannot simply opt out of, though their exit is 'constrained' rather than 'trapped' since alternative research sites and collections remain available elsewhere. Naturalist and scriptural communities are excluded rather than payers or beneficiaries in the strict sense — they are not extracted from directly, but their frameworks are denied adjudicating authority, which is a distinct structural cost from bearing a material transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification (rather than snare or rope) is deliberate: there is a genuine coordination function here — replacing ad hoc, colonially-inflected unilateral excavation and display with a recognized channel for resolving disputes over ancestral remains — that benefits both the affected nations and, arguably, the broader project of correcting historical harm. But the same structure that delivers that coordination also enforces an asymmetric transfer onto researchers and curators who bear real costs (blocked studies, deaccessioned collections) without a corresponding voice in adjudication. Treating this purely as extraction would erase the genuine historical-harm-correction function; treating it purely as coordination would erase the real, enforced cost imposed on the excluded and paying parties. The classification should hold both facts simultaneously rather than collapsing to either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oral_tradition_evidentiary_sufficiency,
    'Is sustained oral tradition, absent corroborating material or documentary evidence, sufficient on its own to establish a specific continuity claim between a living community and particular ancestral remains or a particular site?',
    'Case-by-case adjudication combining oral tradition, linguistic and cultural continuity evidence, and (where available) genetic/archaeological corroboration; comparative analysis of cases where oral tradition and material dating have subsequently agreed or diverged.',
    'If oral tradition is generally corroborated when later checked against independent evidence, its evidentiary weight as a stand-alone record is strengthened; if it is frequently found to diverge from independently derived material chronologies, the reading''s claim to make material evidence unnecessary is weakened and the constraint looks more purely political than epistemic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_evidentiary_sufficiency, empirical, 'Whether oral tradition alone is evidentiarily sufficient for continuity claims.').

omega_variable(
    subordination_scope_ambiguity,
    'Does subordinating credentialed and scriptural frameworks to community authority apply only to interpretive claims about meaning and disposition of remains, or does it also extend to foreclosing independent material dating and genetic analysis outright?',
    'Track specific case outcomes (e.g., contested repatriation rulings) for whether material analysis was permitted alongside community authority or excluded entirely once continuity was asserted.',
    'A narrow reading (subordination only over disposition/interpretation, material study still permitted) is closer to tangled_rope with moderate extraction; a broad reading (material study foreclosed entirely once continuity is asserted) pushes the constraint toward snare, since accessibility_collapse would be far higher than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_scope_ambiguity, conceptual, 'How far the subordination of competing frameworks actually extends in practice.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three kernel readings (naturalist, creationist, indigenous epistemology) genuinely incommensurable claims about different things (scientific origin, theological meaning, relational continuity), or do they make overlapping empirical claims about the same physical facts that could in principle be jointly adjudicated?',
    'Philosophical and anthropological analysis of whether ''continuity with ancestors and place'' is a claim about the same referent as ''materialist origins'' or a categorically different kind of claim (relational/normative rather than causal-historical).',
    'If the readings are genuinely about different things, the sibling constraints (naturalist_reading, creationist_reading) properly coexist without contradiction; if they make overlapping causal claims about the same physical facts, at least one relation should be authored as foreclosing rather than coexisting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel readings are commensurable claims about the same referent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__indigenous_epistemology_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__indigenous_epistemology_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__indigenous_epistemology_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(anth_tr_t32, anthropological_record__indigenous_epistemology_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__indigenous_epistemology_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anth_be_t8, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(anth_be_t16, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(anth_be_t24, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(anth_be_t32, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(anth_be_t40, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 40, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(anth_su_t8, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(anth_su_t16, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(anth_su_t24, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(anth_su_t32, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(anth_su_t40, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints decompose the single natural-language kernel 'the anthropological record': naturalist_reading (materialist origins, scientific method authoritative), creationist_reading (divine creation, scriptural timeline authoritative), and this file, indigenous_epistemology_reading (relational continuity, oral tradition authoritative, subordinating both other frameworks). Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; none averages or blends the others' claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
