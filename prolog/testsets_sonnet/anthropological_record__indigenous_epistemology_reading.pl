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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Indigenous Epistemology Reading of the Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   anthropological-record kernel: the claim that the record reveals
 *   relational continuity between living communities, their ancestors, and
 *   specific places, and that this continuity is knowable primarily through
 *   sustained oral tradition rather than (or in addition to) material dating
 *   or genetic analysis. Under this reading, community authority over
 *   ancestral remains subordinates both credentialed scientific method and
 *   scriptural timeline claims — a tribal government's assertion of
 *   continuity, grounded in oral tradition, can override archaeological
 *   dating results or doctrinal origin claims in determining disposition of
 *   remains. This is not a hedge across the naturalist and creationist
 *   readings; it is a distinct constraint with its own beneficiary/victim
 *   structure, its own extraction profile, and its own enforcement mechanism
 *   (repatriation statutes and negotiated institutional agreements). The
 *   naturalist_reading and creationist_reading are separate constraints,
 *   linked only via network edges and the shared kernel_id.
 *
 * KEY AGENTS:
 *   - indigenous_nations_and_tribal_governments: agenda_setter/beneficiary (organized/constrained) — administer and enforce continuity claims
 *   - traditional_knowledge_keepers: beneficiary (moderate/identity_locked) — source and bearers of the oral tradition that grounds the claim
 *   - academic_archaeologists: payer (institutional/constrained) — bear cost of subordinated findings and halted research
 *   - museum_collections_holding_ancestral_remains: payer (institutional/constrained) — bear repatriation and deaccession costs
 *   - religious_creationist_organizations: excluded (organized/trapped) — equally subordinated but not consulted
 *   - descendant_communities_without_recognized_status: excluded (powerless/trapped) — structurally identical claim, no legal standing
 *   - comparative_epistemologists: observer (analytical/analytical) — studies the reading without adjudicating it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.42).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.55).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Indigenous Epistemology Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, 'ac0fe057-7356-4f45-9d83-a75684781fc9').
narrative_ontology:cs_kernel_codification('ac0fe057-7356-4f45-9d83-a75684781fc9', distributed).
narrative_ontology:cs_authority_grounding('ac0fe057-7356-4f45-9d83-a75684781fc9', practice).
narrative_ontology:cs_interpretation_layer_present('ac0fe057-7356-4f45-9d83-a75684781fc9').
narrative_ontology:cs_reading_relation('ac0fe057-7356-4f45-9d83-a75684781fc9', anthropological_record__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('ac0fe057-7356-4f45-9d83-a75684781fc9', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('ac0fe057-7356-4f45-9d83-a75684781fc9', foundational, oral_tradition_as_sufficient_evidence).
narrative_ontology:cs_axiom_status(oral_tradition_as_sufficient_evidence, holdable).
narrative_ontology:cs_axiom_grounding('ac0fe057-7356-4f45-9d83-a75684781fc9', oral_tradition_as_sufficient_evidence, conventional).
narrative_ontology:cs_axiom('ac0fe057-7356-4f45-9d83-a75684781fc9', foundational, community_authority_supersedes_credentialed_and_scriptural_claims).
narrative_ontology:cs_axiom_status(community_authority_supersedes_credentialed_and_scriptural_claims, holdable).
narrative_ontology:cs_axiom_grounding('ac0fe057-7356-4f45-9d83-a75684781fc9', community_authority_supersedes_credentialed_and_scriptural_claims, conventional).
narrative_ontology:cs_reference_frame('ac0fe057-7356-4f45-9d83-a75684781fc9', relational_continuity_through_transmitted_memory).
narrative_ontology:cs_drift_state('ac0fe057-7356-4f45-9d83-a75684781fc9', post_repatriation_statute_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ac0fe057-7356-4f45-9d83-a75684781fc9', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_nations_and_tribal_governments).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, traditional_knowledge_keepers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, academic_archaeologists).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, museum_collections_holding_ancestral_remains).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, oral_tradition_as_epistemically_sufficient_evidence).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, community_authority_over_ancestral_remains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert legal and epistemic authority over ancestral remains and cultural items found on or traced to their traditional territories, using statutory repatriation mechanisms (e.g. NAGPRA-type frameworks) and direct negotiation with museums and universities. They administer which claims of relational continuity are recognized as legitimate, and can compel return or reburial of remains regardless of what carbon dating or genetic analysis concludes. Their exit from the arrangement would mean losing the only legal lever that currently forces institutions to listen.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_nations_and_tribal_governments, agenda_setter,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, indigenous_nations_and_tribal_governments, beneficiary).

% Hold and transmit oral histories that establish continuity between living communities, specific ancestors, and specific places. Their authority is recognized only when tribal governments and legal frameworks accept oral tradition as evidence; their standing is constituted by the epistemology itself, so they cannot step outside it to adjudicate their own claims from a neutral position.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, traditional_knowledge_keepers, beneficiary,
    moderate, civilizational, identity_locked, regional).

% Conduct excavation, dating, and osteological analysis on remains and artifacts, but under this reading their conclusions are subordinated to community determinations of relational continuity — a genetic or stratigraphic result can be overridden by a tribal government's assertion of oral-tradition continuity. They bear the cost of halted excavations, blocked publication of findings, and repatriation of collections built over generations of research; their disciplinary credentialing gives them no override authority here.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, academic_archaeologists, payer,
    institutional, biographical, constrained, national).

% Hold collections of remains and funerary objects acquired historically, often without consent. Under this reading they must recognize community-asserted continuity claims as sufficient grounds for repatriation and cannot require independent scientific corroboration as a precondition. They bear direct costs: loss of collection holdings, deaccessioning labor, and reputational exposure for past acquisition practices.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, museum_collections_holding_ancestral_remains, payer,
    institutional, generational, constrained, national).

% Hold a scriptural-timeline account of human origins that this reading also subordinates to community authority over remains — their doctrinal claims carry no more weight than material science does when a tribal government asserts continuity. They are not party to the negotiations between tribes and institutions and have no forum in which to press their reading against this one.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, religious_creationist_organizations, excluded,
    organized, generational, trapped, national).

% Communities whose historical continuity claims are contested or unrecognized by the legal frameworks that operationalize this reading (e.g. non-federally-recognized tribes) cannot access the authority this reading grants, even though their epistemic situation is structurally identical to recognized communities. They would object that the reading's protections are gated by state recognition rather than by the epistemic principle itself.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, descendant_communities_without_recognized_status, excluded,
    powerless, generational, trapped, regional).

% Study how different evidentiary regimes (scientific, scriptural, oral-traditional) each construct authority claims over the same physical remains, without needing to adjudicate which reading is correct.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, comparative_epistemologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, diffuse).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates recognition of descendant communities' relationship to ancestral remains and territory in contexts where colonial-era removal severed communities from control over their own dead, and where material science alone (radiocarbon dating, genetic analysis) cannot establish which specific community holds legitimate relational claim.
% TRANSFER_FUNCTION: Moves decision-making authority over disposition of remains and artifacts from academic and museum institutions to tribal governments and knowledge keepers; moves physical custody of remains and cultural items from collections back to communities; moves research access and publication opportunities away from archaeologists toward community-controlled narratives.
% ABSENT_VOICES: Non-federally-recognized descendant communities have no forum under this reading's legal instantiation, despite an identical epistemic claim to relational continuity. Religious creationist organizations, whose account is equally subordinated here, are not consulted or represented in the negotiations that operationalize this reading.
% DISAPPEARANCE_RATIONALE: If community authority over ancestral remains were withdrawn overnight, museums and universities would resume unilateral control over excavation, retention, and publication decisions; ongoing repatriations would halt; oral tradition would lose its standing as sufficient evidence, and disputes over remains would revert entirely to material-evidence adjudication controlled by credentialed institutions.
% FOUNDING_PROBLEM: Colonial-era archaeology and museology removed ancestral remains and funerary objects from indigenous communities without consent, treating them as scientific specimens or curiosities, while dismissing the affected communities' own accounts of ancestry and continuity as myth rather than evidence.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal historians and human-rights bodies documenting the history of unconsented remains collection corroborate that the founding problem (non-consensual appropriation and epistemic dismissal) is real and ongoing in unresolved collections; some archaeologists and museum professionals outside the beneficiary set have testified in favor of repatriation reform, but a substantial portion of the discipline continues to contest whether oral tradition should carry evidentiary weight equal to material dating methods — corroboration from outside the beneficiary communities is real but partial and contested.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).
:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: this reading redistributes decision authority and physical custody, but the underlying coordination function (correcting a real historical wrong — non-consensual appropriation of ancestral remains) is genuine, not a cover story. Suppression is moderate-high (0.55) because enforcement (statutory repatriation requirements, NAGPRA-style compliance obligations) is real and actively defended, and because the reading requires institutions to accept a form of evidence (oral tradition) that they cannot independently verify by their own disciplinary standards — that non-verifiability functions as a suppression of alternative evidentiary claims once accepted. Accessibility collapse is moderate (0.4): archaeologists and museums retain the option of contesting specific claims through legal or diplomatic channels, so alternatives are not fully foreclosed, but the reading's legal instantiation increasingly forecloses the option of proceeding on material evidence alone. Resistance is fairly high (0.68), reflecting ongoing disciplinary pushback from archaeology and museology sectors that argue the subordination of material evidence undermines scientific method.
 *
 * DIRECTIONALITY LOGIC:
 *   Tribal governments and knowledge keepers are declared beneficiaries: they gain authority, custody, and narrative control that colonial-era institutions previously denied them — d sits near the beneficiary end. Archaeologists and museums are declared victims: they bear the transfer of authority and the direct costs of repatriation and halted excavation — d sits near the target end, and their exit is constrained by public accountability and legal compliance requirements, not by real alternative arrangements. Excluded parties (religious organizations, non-recognized descendant communities) are neither beneficiaries nor victims in the formal sense — they simply have no seat, which is a distinct structural fact captured in absent_voices, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (non-consensual removal and epistemic dismissal of indigenous communities' accounts of their own ancestors) remains live and is corroborated in part by sources outside the beneficiary set (legal historians, some archaeologists), which weighs against a mandatrophy reading. However, the exclusion of non-federally-recognized communities from the very protections this reading is meant to secure suggests the current legal instantiation of the reading may already be drifting from its founding epistemic principle toward a narrower, state-recognition-gated version of itself — worth tracking as the interval progresses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_indigenous_epistemology_vs_siblings,
    'Is the anthropological record best read through relational/oral-tradition epistemology, materialist scientific method, or scriptural doctrine — and is community authority over remains a genuinely separable epistemic claim from the underlying origins question, or does it inevitably contest the sibling readings'' authority claims by proxy?',
    'This is a genealogical and jurisprudential question, not an empirical one resolvable by additional data: it depends on which evidentiary community is granted final adjudicating authority over disputed remains, a question settled by legal and political processes (repatriation statutes, court rulings, international declarations on indigenous rights) rather than by comparing predictive accuracy across readings.',
    'If courts and legislatures continue to expand recognition of oral tradition as sufficient evidence, this reading''s institutional footprint grows and its extraction profile may shift toward the naturalist_reading''s victim set (archaeologists, museums) more heavily. If recognition narrows to only federally-acknowledged tribes, the reading''s own excluded set (non-recognized descendant communities) grows, and the reading itself begins to resemble a scaffold serving a subset of its intended beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_indigenous_epistemology_vs_siblings, conceptual, 'Whether this reading''s community-authority claim is a genuinely independent epistemic instantiation or an institutional proxy contest with the sibling readings.').

omega_variable(
    oral_tradition_verifiability,
    'Can sustained oral tradition, as an evidentiary category, be independently corroborated or falsified by methods legible to institutions outside the originating community, or is its evidentiary status irreducibly internal to the tradition itself?',
    'Comparative ethnohistorical studies tracing oral accounts against independently dateable events (eclipses, volcanic events, documented population movements) where such cross-checks are available and consented to by the communities involved; absent consent, the question remains genuinely irreducible under this reading''s own terms.',
    'If oral tradition shows a strong track record of independent corroboration where checkable, the coordination function of this reading strengthens relative to the extraction interpretation. If no such corroboration path is accepted or available, external observers may read the arrangement as resting on an unverifiable epistemic claim, which sharpens the tangled_rope classification (genuine correction of colonial harm, but resting on a foundation resistant to independent audit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oral_tradition_verifiability, empirical, 'Whether oral tradition''s evidentiary sufficiency claim admits any external corroboration standard.').

omega_variable(
    recognition_gate_beneficiary_scope,
    'Should the protections and authority this reading grants extend to any community asserting relational continuity via oral tradition, or only to those already holding formal state recognition (e.g. federally recognized tribes)?',
    'Legislative and judicial expansion or contraction of recognition criteria; advocacy outcomes for non-recognized descendant communities seeking parity of treatment.',
    'A values question, not an empirical one: broadening recognition would strengthen the reading''s claim to be a principled epistemology rather than a state-administered category; keeping the gate narrow preserves current institutional beneficiaries'' exclusive standing but weakens the reading''s claim to universality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_gate_beneficiary_scope, preference, 'Whether the reading''s benefits should extend beyond currently state-recognized communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__indigenous_epistemology_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__indigenous_epistemology_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__indigenous_epistemology_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(anth_tr_t32, anthropological_record__indigenous_epistemology_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__indigenous_epistemology_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(anth_be_t8, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(anth_be_t16, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(anth_be_t24, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(anth_be_t32, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(anth_be_t40, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(anth_su_t8, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(anth_su_t16, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(anth_su_t24, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(anth_su_t32, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(anth_su_t40, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__indigenous_epistemology_reading, 0.1).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the anthropological record' per the ε-invariance principle. naturalist_reading treats material/genetic evidence as epistemically primary; creationist_reading treats scriptural/doctrinal compatibility as epistemically primary; this story (indigenous_epistemology_reading) treats sustained oral tradition and community authority as epistemically primary and subordinates both other frameworks to community determination over ancestral remains. Each carries its own ε, beneficiary/victim structure, and classification; they are linked here via network edges rather than merged into a single observer-relative constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
