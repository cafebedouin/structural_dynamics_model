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
 *   domain: epistemology/anthropology
 *
 * SUMMARY:
 *   This constraint instantiates the indigenous_epistemology_reading of the
 *   anthropological_record kernel. It claims that the anthropological record
 *   reveals relational continuity with ancestors and place, and that this
 *   continuity is knowable primarily through sustained oral tradition rather
 *   than through material evidence or scriptural revelation. The constraint
 *   thereby establishes indigenous community authority over ancestral
 *   remains, sacred sites, and the interpretation of human origins,
 *   subordinating both credentialed scientific frameworks and scriptural
 *   religious frameworks. The arrangement coordinates intergenerational
 *   cultural transmission and sacred geography maintenance, while
 *   asymmetrically extracting epistemic authority and physical control from
 *   museums, universities, and religious institutions.
 *
 * KEY AGENTS:
 *   - indigenous_community_authority: Primary agenda-setter (organized/identity_locked) â holds epistemic sovereignty and repatriation authority over the record
 *   - credentialed_research_community: Primary target (institutional/constrained) â bears loss of epistemic priority, access, and publication autonomy
 *   - state_museum_systems: Secondary target (institutional/constrained) â bears physical loss of collections and administrative burden of repatriation
 *   - scriptural_religious_bodies: Tertiary target (organized/constrained) â bears subordination of scriptural frameworks to community authority in public discourse
 *   - national_courts: Analytical observer (institutional/analytical) â adjudicates between competing authority claims without collecting the extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.68).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.72).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Indigenous Epistemology Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '1a533136-3855-43ff-9e49-f20d158d1db7').
narrative_ontology:cs_kernel_codification('1a533136-3855-43ff-9e49-f20d158d1db7', distributed).
narrative_ontology:cs_authority_grounding('1a533136-3855-43ff-9e49-f20d158d1db7', lineage).
narrative_ontology:cs_interpretation_layer_present('1a533136-3855-43ff-9e49-f20d158d1db7').
narrative_ontology:cs_reading_relation('1a533136-3855-43ff-9e49-f20d158d1db7', anthropological_record__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('1a533136-3855-43ff-9e49-f20d158d1db7', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('1a533136-3855-43ff-9e49-f20d158d1db7', foundational, oral_tradition_epistemic_sovereignty).
narrative_ontology:cs_axiom_status(oral_tradition_epistemic_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1a533136-3855-43ff-9e49-f20d158d1db7', oral_tradition_epistemic_sovereignty, conventional).
narrative_ontology:cs_axiom('1a533136-3855-43ff-9e49-f20d158d1db7', foundational, relational_continuity_ontological_priority).
narrative_ontology:cs_axiom_status(relational_continuity_ontological_priority, holdable).
narrative_ontology:cs_axiom_grounding('1a533136-3855-43ff-9e49-f20d158d1db7', relational_continuity_ontological_priority, deontological).
narrative_ontology:cs_reference_frame('1a533136-3855-43ff-9e49-f20d158d1db7', ancestral_relational_authority).
narrative_ontology:cs_drift_state('1a533136-3855-43ff-9e49-f20d158d1db7', contemporary_repatriation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a533136-3855-43ff-9e49-f20d158d1db7', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_community_authority).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_community_members).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, credentialed_research_community).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, scriptural_religious_bodies).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, state_museum_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the recognized right to determine access to ancestral remains and sacred sites, to validate or reject external research proposals, and to assert oral tradition as the primary framework for interpreting the anthropological record. Authority is exercised through councils, elder boards, and legal instruments such as repatriation claims.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_community_authority, agenda_setter,
    organized, generational, identity_locked, regional).

% Receive cultural continuity through the transmission of language, ceremony, and ancestral relationship. Individual members do not set epistemic rules but live within the collective framework that maintains sacred geography and intergenerational narrative.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_community_members, beneficiary,
    moderate, generational, identity_locked, regional).

% Must obtain community consent to study remains or sites; publications and methodologies are subject to community oversight or veto. They bear the cost of restructured research programs, delayed access, and epistemic devaluation of material evidence when it conflicts with oral tradition.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, credentialed_research_community, payer,
    institutional, biographical, constrained, global).

% Find their creation narratives and scriptural timelines subordinated to community authority in museum and policy discourse. They lose a public ally against strict materialism and compete with indigenous origin stories for legitimacy in educational and political venues.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, scriptural_religious_bodies, payer,
    organized, generational, constrained, global).

% Hold physical collections of ancestral remains and artifacts that are subject to repatriation claims and community-controlled access protocols. They bear administrative and legal costs of inventorying, consulting, and returning items, and lose curatorial autonomy.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, state_museum_systems, payer,
    institutional, generational, constrained, national).

% Adjudicate repatriation and land-rights claims by evaluating community standing, oral tradition evidence, and statutory frameworks. They do not collect epistemic or material benefits but mediate between competing authority claims.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, national_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, indigenous_community_authority).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves intergenerational cultural continuity, sacred relationship to specific places and ancestors, and the transmission of language and ceremony through sustained oral tradition.
% TRANSFER_FUNCTION: Moves epistemic authority and physical control of ancestral remains from credentialed research institutions, state museums, and scriptural religious bodies to indigenous community authority.
% ABSENT_VOICES: Materialist scientists who regard oral tradition as non-empirical folklore; creationist groups who regard indigenous spiritual frameworks as doctrinally incompatible; potentially indigenous dissenters who might prefer collaborative material study but are excluded by the community authority structure.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, museums would regain full curatorial control over ancestral remains, scientific researchers would resume unilateral excavation and publication, scriptural institutions would re-enter public origin discourse as peers rather than subordinates, and the legal architecture of repatriation would collapse. Indigenous sacred geography would be administratively desacralized in governance, though community practice might persist sub rosa.
% FOUNDING_PROBLEM: Colonial expropriation of ancestral remains and systematic epistemic dismissal of indigenous knowledge systems by museums, universities, and settler governments.
% FOUNDING_PROBLEM_CORROBORATION: The colonial expropriation of remains is corroborated by international human rights bodies (UNDRIP) and independent post-colonial historians. However, the claim that the indigenous epistemology reading is the only or best remedy is contested by some anthropologists and museum scholars from outside the benefiting parties; legislative histories also show mixed motives.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint systematically transfers control of physical remains and interpretive authority from external institutions to community authority. Suppression (0.72) is higher because the constraint's persistence depends on active legal and social suppression of unilateral scientific excavation, unmediated museum curation, and scriptural claims to precedence. Theater ratio (0.25) is moderate-to-low: consultation protocols and repatriation ceremonies are largely functional, though some institutional performance of deference occurs. Accessibility collapse (0.45) is moderate because materialist and scriptural alternatives persist but are structurally subordinated, not eliminated. Resistance (0.70) is high because the scientific community, museums, and some religious bodies actively contest the loss of authority.
 *
 * PERSPECTIVAL GAP:
 *   The community authority seat experiences the constraint as cultural survival and the restoration of epistemic justice; the research and museum seats experience it as expropriation of collections and closure of inquiry; the scriptural seat experiences it as displacement from public origin discourse. The engine computes this divergence from the same structural data through directionality and scope modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous community authority and members are structural beneficiaries (low directionality): the constraint subsidizes their epistemic sovereignty and political standing. Credentialed researchers, state museums, and scriptural religious bodies are structural targets (high directionality): they bear the costs of subordinated access, lost collections, and diminished public legitimacy. National courts sit near the analytical middle, mediating without capturing the extraction or bearing its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled rope prevents mislabeling it as a pure snare by preserving its genuine coordination function: intergenerational knowledge transmission, sacred relationship to place, and identity continuity are real public goods that the constraint produces. Conversely, the tangled rope classification prevents mislabeling it as pure rope by requiring the declaration of victims and active enforcement, capturing the asymmetric subordination of external epistemic frameworks that would otherwise be invisible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oral_tradition_incommensurability,
    'Is oral tradition incommensurable with scientific epistemology, or can it be integrated as complementary evidence without subordination?',
    'Comparative analysis of hybrid research programs that integrate oral tradition and material evidence on equal epistemic footing.',
    'If incommensurable, the constraint''s suppression of scientific alternatives is structurally necessary for coordination; if integrable, the constraint is more extractive than coordinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_incommensurability, conceptual, 'Whether oral tradition and scientific method can coexist as peers or must be ranked').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal repatriation mandates, statutory consultation) or internalized (self-censorship by researchers due to political or moral pressure)?',
    'Survey of researchers on perceived constraints versus actual legal barriers; comparison of research behavior in jurisdictions with and without repatriation law.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and the constraint operates partly through cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_reading_stability,
    'Does the indigenous epistemology reading function as a stable epistemic framework, or as an instrumental political lever whose epistemic content shifts with strategic needs?',
    'Track consistency of oral tradition claims across different legal, political, and territorial contexts.',
    'If content shifts strategically, the reading''s coordination function is weaker and its extraction function stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, empirical, 'Epistemic stability versus strategic instrumentalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t10, anthropological_record__indigenous_epistemology_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__indigenous_epistemology_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(anth_tr_t30, anthropological_record__indigenous_epistemology_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__indigenous_epistemology_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(anth_tr_t50, anthropological_record__indigenous_epistemology_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anth_be_t10, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(anth_be_t20, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(anth_be_t30, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(anth_be_t40, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(anth_be_t50, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(anth_su_t10, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(anth_su_t20, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(anth_su_t30, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(anth_su_t40, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(anth_su_t50, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).

% DUAL FORMULATION NOTE:
% The anthropological_record kernel decomposes into three structurally distinct constraints because the label 'the record' conflates competing epistemic claims with different beneficiary structures, enforcement mechanisms, and epsilon values. This reading treats oral tradition as sovereign; the naturalist reading treats material evidence as sovereign; the creationist reading treats scriptural revelation as sovereign. Each is authored as a separate epsilon-invariant story and linked via network affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
