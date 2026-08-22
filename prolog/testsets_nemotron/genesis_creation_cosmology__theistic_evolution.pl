% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__theistic_evolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__theistic_evolution, []).

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
 *   constraint_id: genesis_creation_cosmology__theistic_evolution
 *   human_readable: Theistic Evolution Reading of Genesis Creation
 *   domain: religious_studies/theology/philosophy_of_science
 *
 * SUMMARY:
 *   The theistic evolution reading of Genesis 1-2 holds that the creation
 *   narrative communicates theological truth — God as creator, humanity's
 *   purpose and fallenness — through Ancient Near Eastern literary forms
 *   (cosmological schema, temple inauguration liturgy, polemic against rival
 *   creation myths) rather than literal historical or scientific claims. This
 *   reading enables compatibility with evolutionary cosmology by restricting
 *   textual authority to the theological domain. It emerged as a major
 *   Protestant and Catholic position post-Darwin (1859), faced suppression
 *   during fundamentalist-modernist controversies (1920s Scopes era), and
 *   regained institutional legitimacy mid-century through figures like
 *   Teilhard de Chardin, Pope Pius XII (Humani Generis 1950), and the
 *   BioLogos movement. The constraint coordinates scientific engagement with
 *   theological fidelity; its extraction is low but non-zero due to
 *   cultural-institutional pressure on literalist alternatives.
 *
 * KEY AGENTS:
 *   - theistic_evolution_believers: Primary beneficiaries (institutional/mobile) — hold coherent theology-science integration
 *   - theological_seminaries_evolutionary: Beneficiaries (institutional/biographical) — train clergy in this framework
 *   - science_engaging_theologians: Beneficiaries (organized/mobile) — professional niche bridging domains
 *   - literalist_doctrine_adherents: Victims (organized/identity_locked) — bear epistemic marginalization costs
 *   - young_earth_creationist_communities: Victims (organized/identity_locked) — bear institutional exclusion costs
 *   - secular_scientific_establishment: Observer (institutional/analytical) — engages theistic evolution as dialogue partner
 *   - biblical_scholars_an: Observer (organized/analytical) — provide ANE literary context supporting non-literal reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__theistic_evolution, 0.22).
domain_priors:suppression_score(genesis_creation_cosmology__theistic_evolution, 0.35).
domain_priors:theater_ratio(genesis_creation_cosmology__theistic_evolution, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, extractiveness, 0.22).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__theistic_evolution, rope).
narrative_ontology:human_readable(genesis_creation_cosmology__theistic_evolution, "Theistic Evolution Reading of Genesis Creation").
narrative_ontology:topic_domain(genesis_creation_cosmology__theistic_evolution, "religious_studies/theology/philosophy_of_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__theistic_evolution, 'e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed').
narrative_ontology:cs_kernel_codification('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', fixed_text).
narrative_ontology:cs_authority_grounding('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', lineage).
narrative_ontology:cs_interpretation_layer_present('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed').
narrative_ontology:cs_reading_relation('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', genesis_creation_cosmology__literary_framework, coexists_with).
narrative_ontology:cs_reading_relation('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', genesis_creation_cosmology__young_earth_literal, forecloses).
narrative_ontology:cs_axiom('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', foundational, genesis_theological_truth_non_literal).
narrative_ontology:cs_axiom_status(genesis_theological_truth_non_literal, holdable).
narrative_ontology:cs_axiom_grounding('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', genesis_theological_truth_non_literal, deontological).
narrative_ontology:cs_axiom('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', foundational, evolutionary_cosmology_compatible_with_creator).
narrative_ontology:cs_axiom_status(evolutionary_cosmology_compatible_with_creator, holdable).
narrative_ontology:cs_axiom_grounding('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', evolutionary_cosmology_compatible_with_creator, instrumental).
narrative_ontology:cs_reference_frame('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', patristic_allegorical_tradition).
narrative_ontology:cs_drift_state('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', post_darwinian_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e0a6b73c-e4bc-485a-9eb3-e3fea11bb0ed', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, theistic_evolution_believers).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, theological_seminaries_evolutionary).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, science_engaging_theologians).
narrative_ontology:constraint_victim(genesis_creation_cosmology__theistic_evolution, literalist_doctrine_adherents).
narrative_ontology:constraint_victim(genesis_creation_cosmology__theistic_evolution, young_earth_creationist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a theology that affirms evolutionary science as God's creative method. Gain intellectual coherence, communal belonging in mainline denominations, and credibility in academic/scientific contexts. Can leave the framework without identity collapse — many migrate from literalist backgrounds.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, theistic_evolution_believers, beneficiary,
    institutional, biographical, mobile, global).

% Train clergy in theistic evolution framework (e.g., BioLogos-affiliated seminaries, mainline Protestant and Catholic institutions). Receive institutional legitimacy, accreditation, and funding streams tied to science engagement. Could pivot curricula but institutional inertia and donor expectations create soft lock-in.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, theological_seminaries_evolutionary, beneficiary,
    institutional, generational, arbitrage, global).

% Professional theologians and scientists (e.g., BioLogos, Faraday Institute, Vatican Observatory) who build careers bridging evolutionary science and Christian theology. Gain grant funding, speaking platforms, publication venues. Exit is mobile — can shift to secular philosophy of biology or pure theology.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, science_engaging_theologians, beneficiary,
    organized, biographical, mobile, global).

% Hold literalist hermeneutic as core to theological identity and communal boundary. Bear costs: marginalization in mainline academia, exclusion from denominational leadership in traditions that adopted theistic evolution, cultural stigma as 'anti-science'. Exit requires identity reconstruction — leaving literalism often means leaving community, family, and self-concept.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, literalist_doctrine_adherents, payer,
    organized, biographical, identity_locked, global).

% Maintain parallel institutions (Answers in Genesis, Creation Museum, Ark Encounter, homeschool curricula, Christian schools) that reject evolutionary cosmology. Bear costs: exclusion from mainstream scientific/educational institutions, legal battles over curriculum, cultural ridicule. Exit is identity_locked — young-earth cosmology is fused to their theological epistemology and communal survival narrative.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, young_earth_creationist_communities, payer,
    organized, generational, identity_locked, global).

% Scientific community (evolutionary biology, cosmology, geology) that engages theistic evolution as a dialogue partner reducing public conflict. Neither benefits nor pays — views the constraint as socially useful coordination. Exit is analytical: evaluates the constraint from outside any theological commitment.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, secular_scientific_establishment, observer,
    institutional, civilizational, analytical, universal).

% Scholars of Ancient Near Eastern literature and biblical studies who provide the literary-historical evidence for non-literal reading (cosmological schema, temple texts, polemical genre). Their work supports the coordination but they hold diverse personal theological positions. Exit is analytical — scholarly consensus shifts on evidence.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, biblical_scholars_an, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables Christians to affirm evolutionary science as God's creative method while maintaining theological commitments to creator, purpose, and human dignity — solves the faith/science crisis that fragmented Protestantism post-Darwin.
% TRANSFER_FUNCTION: Moves cultural legitimacy and institutional access from literalist communities to theistic evolution frameworks in mainline denominations, seminaries, and academia. Literalists lose credibility capital; theistic evolutionists gain it.
% ABSENT_VOICES: Secular scientists who reject any theological accommodation (e.g., New Atheist voices) — they would object that theistic evolution still smuggles in non-naturalistic claims. Mythicist biblical scholars who deny historical referent for any Genesis theology. Both are excluded from the coordination the constraint achieves.
% DISAPPEARANCE_RATIONALE: If theistic evolution vanished overnight, mainline denominations would lose their primary theology/science integration framework. Clergy training would revert to either literalism or secularized theology. The faith/science conflict would re-escalate in public education and culture war arenas. Literalist communities would gain cultural ground; secular science would lose a bridge community.
% FOUNDING_PROBLEM: Post-1859 crisis: Darwinian evolution shattered the literal Genesis cosmology that grounded Protestant theology and public culture. Theistic evolution was built to preserve Christian theological coherence without rejecting scientific consensus.
% FOUNDING_PROBLEM_CORROBORATION: Evolutionary biology remains scientific consensus (National Academies, Royal Society). Mainline denominations (PCUSA, ELCA, Episcopal, UMC, Catholic) officially affirm compatibility. BioLogos and Faraday Institute document ongoing need. Literalist communities (AIG, ICR) attest the problem is live from their side — they experience the constraint as ongoing pressure.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__theistic_evolution, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__theistic_evolution, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__theistic_evolution, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(genesis_creation_cosmology__theistic_evolution, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_cosmology__theistic_evolution, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__theistic_evolution_tests).
:- end_tests(genesis_creation_cosmology__theistic_evolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22) is low because the reading primarily coordinates — it solves the theology/science tension for believers without coercing adherence. Suppression (0.35) reflects historical episodes (Scopes, fundamentalist takeovers of institutions) where literalist alternatives were actively marginalized, but current suppression is cultural-institutional not legal. Theater (0.18) is low: the coordination function (faithful scientists, credible theology) is genuine. Accessibility collapse (0.42) is moderate: literalist alternatives persist robustly despite cultural pressure. Resistance (0.55) is significant: young-earth creationism maintains parallel institutions (Answers in Genesis, Creation Museum, homeschool curricula) demonstrating the constraint does not collapse alternatives. Claimed type: rope — genuine coordination with minimal coercive overhead, though victim declarations introduce tension with pure rope classification.
 *
 * PERSPECTIVAL GAP:
 *   From theistic evolution seat (beneficiary/institutional/mobile): constraint is pure coordination — it enables faithful scientific practice. From literalist seat (victim/identity_locked): constraint operates as extraction — cultural dominance of theistic evolution in mainline institutions marginalizes their hermeneutic, costing them credibility, educational access, and institutional resources. From secular science seat (observer): constraint is benign coordination — removes theology/science conflict from public sphere. The engine will compute per-seat types from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (theistic_evolution_believers, theological_seminaries_evolutionary, science_engaging_theologians) collect coordination gains: intellectual coherence, institutional legitimacy, professional viability. Their exit options are mobile/arbitrage — they can leave the framework without identity collapse. Victims (literalist_doctrine_adherents, young_earth_creationist_communities) bear costs: epistemic marginalization in academia, exclusion from mainline denominational leadership, cultural stigma. Their exit is identity_locked — literalist hermeneutic is fused to their theological identity and communal boundary maintenance. Excluded voices (secular scientists who reject any theology, mythicist scholars) are not structurally positioned by this constraint. The directionality derivation from beneficiary/victim + exit captures this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem (post-1859 theology/science crisis) is live — evolutionary cosmology remains the scientific consensus and theological integration remains needed. No mandatrophy: the constraint's coordination function matches its current operation. However, if scientific consensus shifted or theological liberalism collapsed, the constraint could become a piton (theatrical maintenance of a solved problem). Current theater_ratio (0.18) suggests genuine function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a single reading of the contested kernel ''genesis_creation_cosmology'', or does the label cover multiple structurally distinct claims?',
    'Decompose into separate constraint stories per ε-invariance principle: theistic_evolution, literary_framework, young_earth_literal each get their own ε, stakeholders, and classification linked by network.affects_constraints.',
    'If decomposed, each reading gets independent extraction profile; theistic_evolution would show low ε (coordination), young_earth_literal high ε (extraction via suppression of alternatives). Current single-story authoring risks conflation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the Genesis creation kernel should be modeled as one constraint with contested readings or multiple structurally distinct constraints').

omega_variable(
    literalist_victim_status,
    'Do literalist adherents bear costs from this reading''s operation, or are they merely non-participants in a different interpretive framework?',
    'Measure concrete costs: exclusion from institutional credibility, loss of educational access, social marginalization in scientific/academic contexts attributable to theistic evolution''s cultural dominance.',
    'If literalists bear asymmetric costs (epistemic marginalization, institutional exclusion), they are victims and constraint may be tangled_rope. If no structural extraction, they are excluded observers and constraint remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literalist_victim_status, empirical, 'Whether literalist doctrine adherents are structural victims of theistic evolution''s cultural-institutional prevalence').

omega_variable(
    scientific_cooption_risk,
    'Does theistic evolution''s compatibility claim extract legitimacy from science without reciprocal accountability?',
    'Track whether theistic evolution makes testable predictions that could falsify its theological claims, or whether it only accommodates post-hoc. Compare to NOMA (non-overlapping magisteria) boundary maintenance.',
    'If unidirectional legitimacy borrowing (science → theology without vulnerability), extraction is higher than authored. If genuine bidirectional engagement, current ε stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scientific_cooption_risk, conceptual, 'Whether compatibility functions as legitimacy extraction from scientific authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__theistic_evolution, 1859, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1859, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1859, 0.25).
narrative_ontology:measurement(gene_tr_t1925, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1925, 0.3).
narrative_ontology:measurement(gene_tr_t1950, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(gene_tr_t1980, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(gene_tr_t2005, genesis_creation_cosmology__theistic_evolution, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(gene_tr_t2024, genesis_creation_cosmology__theistic_evolution, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(gene_be_t1859, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1859, 0.35).
narrative_ontology:measurement(gene_be_t1925, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1925, 0.42).
narrative_ontology:measurement(gene_be_t1950, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(gene_be_t1980, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(gene_be_t2005, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(gene_be_t2024, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1859, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1859, 0.55).
narrative_ontology:measurement(gene_su_t1925, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1925, 0.65).
narrative_ontology:measurement(gene_su_t1950, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(gene_su_t1980, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(gene_su_t2005, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(gene_su_t2024, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__theistic_evolution, identity_coordination).
narrative_ontology:boltzmann_floor_override(genesis_creation_cosmology__theistic_evolution, 0.08).
narrative_ontology:affects_constraint(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology__literary_framework).
narrative_ontology:affects_constraint(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology__young_earth_literal).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of the genesis_creation_cosmology kernel. theistic_evolution (this story) = coordination via non-literal theological truth. literary_framework = minimalist coordination via literary form only. young_earth_literal = extraction via literalist suppression of alternatives. Linked by network.affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genesis_creation_cosmology__theistic_evolution, institutional, 0.15).
constraint_indexing:directionality_override(genesis_creation_cosmology__theistic_evolution, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
