% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Reading of the Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The creationist reading of the anthropological record asserts that divine
 *   creation event(s) compatible with scriptural timeline or designed
 *   complexity are revealed in the empirical record. This reading operates as
 *   a constraint by requiring that any acceptable account of human origins
 *   accommodate divine causation as a necessary explanatory element, and by
 *   suppressing the materialist timeline (deep time, common descent, unguided
 *   evolution) within adopting communities. The constraint is actively
 *   enforced through institutional statements of faith, educational curricula
 *   (homeschool, Christian school, and political pressure on public
 *   standards), and community boundary maintenance. It coordinates religious
 *   communities around a shared epistemic commitment while extracting
 *   epistemic authority from credentialed science and excluding pluralist and
 *   indigenous alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.65).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.75).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, '438e4c58-7363-436d-a23d-576a82f967ce').
narrative_ontology:cs_kernel_codification('438e4c58-7363-436d-a23d-576a82f967ce', fixed_text).
narrative_ontology:cs_authority_grounding('438e4c58-7363-436d-a23d-576a82f967ce', lineage).
narrative_ontology:cs_interpretation_layer_present('438e4c58-7363-436d-a23d-576a82f967ce').
narrative_ontology:cs_reading_relation('438e4c58-7363-436d-a23d-576a82f967ce', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('438e4c58-7363-436d-a23d-576a82f967ce', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('438e4c58-7363-436d-a23d-576a82f967ce', foundational, divine_creation_required_by_record).
narrative_ontology:cs_axiom_status(divine_creation_required_by_record, holdable).
narrative_ontology:cs_axiom_grounding('438e4c58-7363-436d-a23d-576a82f967ce', divine_creation_required_by_record, theological).
narrative_ontology:cs_axiom('438e4c58-7363-436d-a23d-576a82f967ce', foundational, scriptural_timeline_epistemic_priority).
narrative_ontology:cs_axiom_status(scriptural_timeline_epistemic_priority, holdable).
narrative_ontology:cs_axiom_grounding('438e4c58-7363-436d-a23d-576a82f967ce', scriptural_timeline_epistemic_priority, theological).
narrative_ontology:cs_axiom('438e4c58-7363-436d-a23d-576a82f967ce', secondary, science_yields_adjudicative_authority_in_religious_domains).
narrative_ontology:cs_axiom_status(science_yields_adjudicative_authority_in_religious_domains, holdable).
narrative_ontology:cs_axiom_grounding('438e4c58-7363-436d-a23d-576a82f967ce', science_yields_adjudicative_authority_in_religious_domains, conventional).
narrative_ontology:cs_reference_frame('438e4c58-7363-436d-a23d-576a82f967ce', classical_scriptural_authority).
narrative_ontology:cs_drift_state('438e4c58-7363-436d-a23d-576a82f967ce', contemporary_secular_science_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('438e4c58-7363-436d-a23d-576a82f967ce', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_institutions).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, religious_communities_adopting_reading).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, credentialed_scientists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, science_educators).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, religious_pluralists).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, divine_creation_compatible_with_record).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, scriptural_timeline_epistemic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and enforce the creationist reading through seminaries, publishing houses, advocacy organizations, and educational institutions. Their institutional identity and funding depend on maintaining the reading's authority. Exit would mean institutional dissolution or radical transformation.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Gain coherent origin narrative, moral framework, and communal identity from the reading. The reading structures Sunday school curricula, homeschool materials, and community boundaries. Exit is constrained by social embeddedness — leaving the reading often means leaving the community.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_communities_adopting_reading, beneficiary,
    organized, biographical, constrained, national).

% Lose adjudicative authority over human origins in religious educational and public policy domains. Face pressure to either accommodate the reading or be excluded from discourse in adopting communities. Can exit to secular professional spaces but lose influence in contested domains.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, credentialed_scientists, payer,
    institutional, biographical, mobile, global).

% Constrained in curriculum by school board policies, state standards, and community pressure shaped by the reading. Must navigate legal boundaries (Edwards v. Aguillard, Kitzmiller v. Dover) while facing professional and social costs for resistance. Exit means leaving public education in affected regions.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, science_educators, payer,
    moderate, biographical, constrained, national).

% Hold both scientific and spiritual epistemologies (e.g., evolutionary creationists, theistic evolutionists). Marginalized within creationist-adopting communities as 'compromised' and in secular scientific spaces as 'insufficiently naturalist.' Their epistemic position is structurally excluded by the reading's demand for exclusive divine causation.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_pluralists, excluded,
    moderate, biographical, identity_locked, national).

% Carry relational origin traditions knowable via sustained oral practice. Neither creationist nor naturalist reading accommodates their epistemic structure — both impose external categories (scriptural timeline, materialist evolution) that displace place-based, ancestor-mediated knowledge. Exit is trapped by colonial history and ongoing epistemic marginalization.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, indigenous_epistemology_holders, excluded,
    moderate, generational, trapped, regional).

% Philosophical, anthropological, and historical analysts who track the kernel's readings as objects of study. No stake in any reading's victory; the constraint's operation is data for understanding epistemic pluralism, authority contestation, and the anthropology of knowledge.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared origin narrative that coordinates religious community identity, moral framework, institutional authority, and intergenerational transmission across diverse denominations and geographies.
% TRANSFER_FUNCTION: Moves epistemic authority over human origins from credentialed science to religious institutions; moves curricular control from science standards to religious/community standards; moves social capital from scientific literacy to doctrinal conformity within adopting communities.
% ABSENT_VOICES: Religious pluralists who hold both scientific and spiritual epistemologies (evolutionary creationists, theistic evolutionists); indigenous epistemology holders whose traditions are neither creationist nor naturalist; secular students in religious educational settings who cannot voice dissent without social cost; mainstream denominational bodies that have accommodated evolutionary science but are pressured toward the reading's boundaries.
% DISAPPEARANCE_RATIONALE: The reading actively structures educational policy (state standards, textbook adoption, school board mandates), institutional statements of faith (seminary hiring, denominational boundaries), and community identity (homeschool curricula, parachurch organizations). Its removal would trigger reorganization across these domains: science education would reclaim uncontested authority in public schools; religious institutions would need new coherence structures; legal/political battles over curriculum would shift from 'teach the controversy' to new fronts.
% FOUNDING_PROBLEM: Late 19th/early 20th century crisis of religious authority amid professionalizing science, biblical criticism (higher criticism), and the rise of evolutionary biology as a comprehensive explanatory framework. The reading was built to defend scriptural authority against materialist reductionism and to preserve a coherent Christian worldview in the face of epistemic displacement.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the fundamentalist-modernist controversy (Marsden 1980, Numbers 1992) from outside creationist institutions corroborates the defensive origin. Sociological studies of boundary-work (Gieryn 1983, Evans 2011) corroborate that the reading functions to demarcate 'true science' from 'secular ideology.' No corroborating source outside the benefiting parties attests that the founding problem remains live in its original form; several (e.g., BioLogos, ASA) attest it has mutated into identity maintenance.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.65) is substantial: the reading demands surrender of scientific adjudicative authority in its domain and imposes conformity costs on educators, pluralists, and indigenous holders. Suppression (0.75) is high: the constraint's persistence depends on active exclusion of materialist timelines from curricula, institutional statements, and communal discourse — not merely on participant preference. Theater ratio (0.4) is moderate: genuine belief and community cohesion coexist with performative adherence enforced by institutional gatekeeping. Accessibility collapse (0.6) reflects that alternatives exist but are structurally marginalized within adopting communities (pluralists labeled 'compromised,' indigenous epistemologies rendered invisible). Resistance (0.7) is high: legal challenges, scientific professional organizations, internal dissent, and competing readings all contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the creationist institution seat, the constraint is genuine coordination (rope-like): it solves the problem of maintaining scriptural authority and communal identity in a secularizing world. From the credentialed scientist seat, it is extraction (snare-like): it demands surrender of adjudicative authority won through empirical method. From the religious pluralist seat, it is a snare with identity-locked suppression: their epistemic synthesis is ruled out by the reading's binary logic. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures the coordination-extraction hybrid but the per-seat experience varies radically.
 *
 * DIRECTIONALITY LOGIC:
 *   Creationist institutions are structural beneficiaries (d near 0.0) — they set the agenda, collect institutional legitimacy and resources, and their identity is fused with the reading. Religious communities adopting the reading are beneficiaries (d ~ 0.2) — they gain coherence and identity but bear conformity costs. Credentialed scientists are targets (d ~ 0.8) — they lose authority in contested domains but retain mobile exit to secular professional spaces. Science educators are constrained targets (d ~ 0.75) — less mobile, directly regulated. Religious pluralists are identity-locked targets (d ~ 0.9) — their epistemic position is excluded by the reading's logic, and exit means identity rupture. Indigenous epistemology holders are trapped targets (d ~ 0.95) — colonial history and ongoing marginalization block exit. The analytical observer sits at d = 0.5 (symmetric) by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defending scriptural authority against materialist reductionism) is contested as live vs. mutated. If dead, the reading persists as mandate-atrophy: the coordination function (community cohesion) remains but the original epistemic threat has been met or transformed, and the constraint now primarily extracts conformity. The engine's mandatrophy_resolved flag would trigger on founding_problem_status=dead + disappearance_verdict=world_rearranges, indicating a zombie constraint. Current contested status means the constraint is in the indeterminate zone — some beneficiaries attest the threat persists (secularism, scientific naturalism), while external corroboration suggests the reading now creates the crisis it claims to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural-law-like reading of the record, or a constructed constraint that benefits identifiable institutions?',
    'Cross-kernel comparison: if the same empirical record yields mutually exclusive readings with different beneficiary structures, the constraint is reading-relative, not record-intrinsic. The engine''s false_summit_mountain signature would evaluate any mountain claim on this kernel.',
    'If reading-relative, the constraint is a tangled_rope (coordination + extraction) not a mountain. The ε value is indexed to this reading''s structural commitments, not to the record itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the creationist reading''s epistemic claims are intrinsic to the record or constituted by the reading''s commitments.').

omega_variable(
    sibling_reading_deltas,
    'What would the naturalist_reading and indigenous_epistemology_reading change structurally if adopted as the adjudicative frame?',
    'Author the sibling constraint stories and compare: naturalist_reading would have beneficiaries (scientific institutions), victims (creationist institutions), and claimed_type mountain (on its own metrics). Indigenous_epistemology_reading would have beneficiaries (indigenous communities), victims (colonial epistemic structures), and claimed_type rope or tangled_rope depending on enforcement.',
    'The three readings form a constraint family with different ε values, different beneficiary/victim structures, and different classification outcomes. Linking them via network.affects_constraints enables contamination analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_deltas, conceptual, 'Structural deltas between this reading and its siblings in the anthropological_record kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.75) primarily structural (institutional gatekeeping, legal pressure) or internalized (identity fusion, epistemic closure that persists after institutional pressure lifts)?',
    'Post-exit suppression trajectory: track religious pluralists who leave creationist communities — does epistemic suppression persist? If yes, reclassify as partially internalized. Compare with indigenous_epistemology_reading where suppression is structurally colonial.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries the suppression after exit. This affects χ computation for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious epistemic communities.').

omega_variable(
    coordination_extraction_boundary,
    'Is the community cohesion function (coordination) structurally separable from the epistemic suppression function (extraction)?',
    'Natural experiment: communities that retain creationist identity but drop epistemic exclusivity (e.g., evolutionary creationist congregations). If cohesion persists without suppression, functions are separable and the suppression is extractive overhead. If cohesion collapses, they are fused.',
    'If separable, the constraint is a tangled_rope with distinguishable components. If fused, the coordination story may be cover for extraction — pushing toward snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the reading''s coordination and extraction components are structurally separable or fused.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anthro_record_creationist_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anthro_record_creationist_tr_t25, anthropological_record__creationist_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(anthro_record_creationist_tr_t50, anthropological_record__creationist_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(anthro_record_creationist_tr_t75, anthropological_record__creationist_reading, theater_ratio, 75, 0.37).
narrative_ontology:measurement(anthro_record_creationist_tr_t100, anthropological_record__creationist_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(anthro_record_creationist_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(anthro_record_creationist_be_t25, anthropological_record__creationist_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(anthro_record_creationist_be_t50, anthropological_record__creationist_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(anthro_record_creationist_be_t75, anthropological_record__creationist_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement(anthro_record_creationist_be_t100, anthropological_record__creationist_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(anthro_record_creationist_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(anthro_record_creationist_su_t25, anthropological_record__creationist_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(anthro_record_creationist_su_t50, anthropological_record__creationist_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(anthro_record_creationist_su_t75, anthropological_record__creationist_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(anthro_record_creationist_su_t100, anthropological_record__creationist_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__creationist_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% Part of the anthropological_record constraint family (kernel_id: anthropological_record). This reading (creationist_reading) forecloses naturalist_reading on the question of divine causation (mutually exclusive core premises within any single framework) but coexists_with indigenous_epistemology_reading (different communities hold both; neither logically rules out the other). The ε values differ: creationist_reading has higher extraction (0.65) due to active suppression of alternatives; naturalist_reading claims mountain-like status for its empirical claims (extraction near 0); indigenous_epistemology_reading has moderate extraction from colonial epistemic structures. All three share the same referent (the anthropological/archaeological/paleontological record) but instantiate different constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__creationist_reading, moderate, 0.9).
constraint_indexing:directionality_override(anthropological_record__creationist_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
