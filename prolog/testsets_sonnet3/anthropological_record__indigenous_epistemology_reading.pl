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
 *   human_readable: Indigenous Epistemology Reading of the Anthropological Record (Relational Continuity via Oral Tradition)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story instantiates the indigenous epistemology reading of the
 *   anthropological record kernel: the claim that the record reveals
 *   relational continuity between living communities, ancestors, and place,
 *   and that this continuity is knowable through sustained oral tradition
 *   rather than exclusively through material-scientific or scriptural
 *   methods. Under this reading, both the credentialed naturalist framework
 *   and the scriptural creationist framework are subordinated, in matters of
 *   custody and interpretation of ancestral remains, to the authority of the
 *   descendant community. This is a distinct constraint from the
 *   naturalist_reading and creationist_reading stories — each has its own ε,
 *   its own beneficiary/victim structure, and its own classification; they
 *   are linked only through the shared kernel, not merged into one story.
 *
 * KEY AGENTS:
 *   - tribal_cultural_authorities: agenda_setter (organized/identity_locked) — administers repatriation and interpretive authority
 *   - descendant_indigenous_communities: primary beneficiary (organized/identity_locked) — regains custody and recognition
 *   - academic_archaeologists: primary target (powerful/constrained) — research access and conclusions subordinated to oral-tradition standard
 *   - museum_curatorial_institutions: secondary target (institutional/constrained) — bears repatriation costs
 *   - federal_and_state_regulators: analytical observer (institutional/analytical) — administers the statutory recognition apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.62).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.58).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Indigenous Epistemology Reading of the Anthropological Record (Relational Continuity via Oral Tradition)").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, 'bdc4de9c-4493-42c6-b4cb-c923fc16b7a0').
narrative_ontology:cs_kernel_codification('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', distributed).
narrative_ontology:cs_authority_grounding('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', practice).
narrative_ontology:cs_interpretation_layer_present('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0').
narrative_ontology:cs_reading_relation('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', anthropological_record__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', foundational, oral_tradition_constitutes_valid_knowledge).
narrative_ontology:cs_axiom_status(oral_tradition_constitutes_valid_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', oral_tradition_constitutes_valid_knowledge, conventional).
narrative_ontology:cs_axiom('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', foundational, community_authority_precedes_external_credentialing).
narrative_ontology:cs_axiom_status(community_authority_precedes_external_credentialing, holdable).
narrative_ontology:cs_axiom_grounding('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', community_authority_precedes_external_credentialing, deontological).
narrative_ontology:cs_reference_frame('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', pre_contact_relational_continuity).
narrative_ontology:cs_drift_state('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', post_nagpra_legal_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('bdc4de9c-4493-42c6-b4cb-c923fc16b7a0', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, descendant_indigenous_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, tribal_cultural_authorities).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, academic_archaeologists).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, museum_curatorial_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, descendant_indigenous_communities).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, oral_tradition_as_valid_epistemic_method).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, community_sovereignty_over_ancestral_remains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and exercise authority to determine what counts as legitimate knowledge about ancestral remains and sites, grounded in sustained oral tradition passed through generations. Administer repatriation claims under laws like NAGPRA, deciding which excavated material returns to community control and under what conditions research may proceed. Their standing to adjudicate is inseparable from their identity as tradition-bearers; there is no arbitrage exit from this role.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, tribal_cultural_authorities, agenda_setter,
    organized, civilizational, identity_locked, regional).

% Receive recognition of continuity with ancestors and place that centuries of settler archaeology denied or erased; regain custody of remains and objects previously held in museums or excavated without consent. Also bear costs where the framework requires continuous performance of tradition-bearing status to institutions that historically demanded assimilation, and where internal disagreements about oral tradition's content can be adjudicated by outside courts.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, descendant_indigenous_communities, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, descendant_indigenous_communities, payer).

% Trained to build claims from material evidence, stratigraphy, and dating methods; under this reading their conclusions are treated as necessarily incomplete or illegitimate without corroborating oral tradition, and access to sites and remains can be withheld or halted regardless of research design. They can relocate research programs but cannot dispute the framework's legitimacy without appearing to re-enact historical dispossession.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, academic_archaeologists, payer,
    powerful, biographical, constrained, national).

% Hold collections built through decades of extraction, often without consent; under this reading their scientific and educational rationale for retention is subordinated to community authority over ancestral remains. Face repatriation costs, reputational exposure, and the loss of research and display assets, with little room to negotiate the underlying legitimacy claim.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, museum_curatorial_institutions, payer,
    institutional, biographical, constrained, national).

% Hold a scriptural framework for human origins that this reading also subordinates to community authority over ancestral remains, though their claims are not centered in this dispute. They are not party to most repatriation proceedings and their framework is neither vindicated nor directly contested by this reading, only structurally deprioritized alongside the naturalist account.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, religious_creationist_communities, excluded,
    organized, civilizational, constrained, national).

% Administer the statutory apparatus (NAGPRA and analogous frameworks) that formally recognizes oral tradition and cultural affiliation as sufficient grounds for repatriation, mediating disputes between tribal authorities, museums, and researchers, and can adjust the evidentiary weight the law assigns to oral testimony versus material analysis.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, federal_and_state_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, descendant_indigenous_communities).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates recognition of which community holds legitimate authority over ancestral remains and sites, using oral tradition as an evidentiary channel that survives when written or excavatable material evidence is fragmentary, destroyed, or was never produced in a form archaeology can read.
% TRANSFER_FUNCTION: Moves custodial and interpretive authority over remains, objects, and site narratives from academic and museum institutions to descendant communities and their designated cultural authorities; moves evidentiary weight in origin disputes from material-empirical method toward testimonial and relational method.
% ABSENT_VOICES: Rival indigenous factions within a single descendant community who hold different oral traditions about the same remains are rarely given separate standing; the reading treats 'the community' as internally coherent, which can silence minority tradition-holders whose accounts would complicate the recognized narrative. Naturalist and creationist framework-holders are present in adjacent disputes but structurally excluded from adjudicating this specific evidentiary question.
% DISAPPEARANCE_RATIONALE: If this reading's authority were withdrawn overnight, repatriation claims would revert to requiring material or documentary corroboration, museums and academic institutions would regain unilateral custody and research access, and decades of returned remains and renegotiated site access could face fresh legal challenge — the current custodial and research landscape depends on this reading's standing.
% FOUNDING_PROBLEM: Centuries of excavation, collection, and display of indigenous remains and objects proceeded without community consent, using an evidentiary standard (material, written, credentialed) that systematically could not register oral tradition as knowledge, producing a record that erased or contested ancestral continuity communities had never stopped attesting to.
% FOUNDING_PROBLEM_CORROBORATION: Federal and state regulators who administer NAGPRA and comparable statutes attest, independent of tribal authorities themselves, that unconsented excavation and unresolved repatriation claims remain active and substantial; legislative hearing records and General Accounting Office / Government Accountability Office reports on repatriation backlogs corroborate the problem's continuation from outside the benefiting communities.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.62 at interval end) reflects the real transfer of custodial and interpretive authority away from institutions that built collections without consent, but also the real costs imposed on researchers and museums regardless of their individual conduct, once the community-authority standard is formally recognized and enforced. Suppression is measured as moderately high but declining over the interval (0.70 to 0.58): the earliest period required more active legal and institutional pressure to establish oral tradition as admissible evidence at all; as case law and statute matured, the suppressive force needed to secure recognition diminished even as substantive extraction (i.e., actual repatriation transfers) continued rising. Theater ratio stays low-to-moderate (0.28) because the coordination function — genuine recognition of continuity previously denied — remains substantially functional rather than performative, though some compliance activity by institutions is increasingly procedural rather than restorative.
 *
 * PERSPECTIVAL GAP:
 *   From the tribal cultural authority seat, this is straightforward coordination correcting a historical epistemic injustice — recognizing knowledge that was always there. From the archaeologist or museum seat, the same structure operates as an enforced subordination of their evidentiary method, imposed with real legal teeth (NAGPRA compliance obligations, loss of research access) regardless of the specific provenance of any given collection. The engine's per-seat computation should reflect this asymmetry: agenda_setter and beneficiary seats likely compute closer to coordination-flavored types, while the payer seats compute closer to extraction-flavored types, given identical structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Tribal cultural authorities and descendant communities sit near the beneficiary end: the reading transfers interpretive and custodial power to them and vindicates a form of knowledge historically dismissed. Academic archaeologists and museum institutions sit near the target end: their institutional practices and holdings are directly constrained or clawed back by the same structure, with constrained exit options since abandoning research programs or contesting the framework carries high reputational and legal cost. Descendant communities also carry a secondary payer role because the enforcement of a single 'community' voice can compress internal disagreement, and because sustained recognition requires continuous performance of tradition-bearing identity to outside legal and academic audiences.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unconsented excavation and epistemic erasure of oral tradition) remains live by external corroboration (regulatory and GAO-style reporting), which weighs against mandatrophy — this is not an arrangement whose justification has evaporated while the apparatus persists. However, the tangled_rope classification itself signals the risk: a coordination function (restoring recognition) is now paired with ongoing extraction (uniform subordination of the naturalist and scriptural frameworks in this domain) enforced through statute, and continued monitoring is needed to check whether the enforcement apparatus outlives active repatriation need in any specific case-by-case sense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oral_tradition_evidentiary_sufficiency,
    'Is sustained oral tradition, absent corroborating material or documentary evidence, sufficient on its own terms to establish cultural affiliation and continuity, or does its evidentiary force depend on partial alignment with material findings?',
    'Comparative case analysis of NAGPRA determinations where oral tradition was the sole basis for affiliation versus cases requiring converging material evidence; examination of whether courts or review boards have ever rejected oral-tradition-based claims for lack of independent corroboration.',
    'If oral tradition is treated as fully sufficient regardless of material convergence, the reading''s authority is maximally strong and independent; if courts consistently require some material corroboration, the reading is partially subordinated to the naturalist evidentiary standard it claims to displace, weakening the claimed independence of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_evidentiary_sufficiency, empirical, 'Whether oral tradition functions as sufficient or merely corroborating evidence in practice.').

omega_variable(
    internal_community_authority_contestation,
    'When multiple descendant groups or factions within a community hold divergent oral traditions about the same remains or site, who has standing to adjudicate, and does the framework''s reliance on ''community authority'' presuppose an internal consensus that does not always exist?',
    'Document cases of intra-community or inter-tribal repatriation disputes and how designated cultural authorities or federal mediators resolved competing oral-tradition claims.',
    'If internal contestation is common and unresolved by the framework, the ''excluded'' status of minority tradition-holders becomes a structural feature rather than an edge case, raising the story''s effective suppression and complicating the clean beneficiary/victim split currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_community_authority_contestation, empirical, 'Whether community authority presupposes an internal consensus that is not always present.').

omega_variable(
    kernel_framing_alternative,
    'Is the anthropological_record kernel better framed as a dispute over the CONTENT of human origins (naturalist vs. creationist vs. indigenous), or as a dispute over the METHOD of legitimate knowledge production (material-empirical vs. scriptural-revelatory vs. testimonial-relational)? The content framing groups this reading with the other two as competing origin claims; the method framing would instead group it as a distinct epistemic-authority contest layered above all three origin claims.',
    'Examine whether the reading''s practical effect (in repatriation law) is best modeled as competing WITH the origin claims for the same evidentiary slot, or as ADJUDICATING which evidentiary method governs any origin claim about a given set of remains.',
    'Under the content framing (adopted here, per the manifest''s declared reading_relations as coordinate siblings), this reading coexists with naturalist_reading and creationist_reading as parallel origin claims. Under the method framing, this reading would instead be classified as a meta-level authority structure influencing all origin claims, which would change its reading_relations from coexists_with toward influences for both siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the kernel is a content-dispute or a method-dispute, which changes the structural relation to siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__indigenous_epistemology_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__indigenous_epistemology_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__indigenous_epistemology_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(anth_tr_t32, anthropological_record__indigenous_epistemology_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__indigenous_epistemology_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anth_be_t8, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(anth_be_t16, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(anth_be_t24, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(anth_be_t32, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(anth_be_t40, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(anth_su_t8, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(anth_su_t16, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(anth_su_t24, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(anth_su_t32, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(anth_su_t40, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__indigenous_epistemology_reading, 0.1).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, creationist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'what the anthropological record reveals' (the anthropological_record kernel) per the ε-invariance principle. naturalist_reading (materialist origins, scientific method) and creationist_reading (divine creation, scriptural timeline) are separate constraint files with their own ε, beneficiaries, victims, and classification. This reading's ε (0.62) is authored independently and should not be averaged or reconciled with the siblings' ε values — the three constraints coexist as distinct, contested framings adjudicated differently by different institutional and legal actors, linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
