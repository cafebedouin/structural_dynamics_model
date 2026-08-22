% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Animal Instrumental Use (Abolitionist Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The abolitionist reading claims that animals are moral subjects with
 *   inherent value, precluding all instrumental use. Under this reading, the
 *   standing arrangement of animal commodification is not a coordination
 *   problem to be managed (the welfare reading) or a property right to be
 *   exercised (the property reading), but a structural violation: the
 *   constraint asserts that animals are victims of a snare whose persistence
 *   depends on actively suppressing recognition of their subjectivity. The
 *   referent (what this reading examines) is the standing arrangement of
 *   instrumental use as currently practiced — agriculture, research,
 *   entertainment, product derivation — assessed by the reading's own lights
 *   as a system that extracts from entities explicitly claimed as moral
 *   subjects. The reading's endorsed alternative (a world where instrumental
 *   use is abolished) is NOT the referent; ε measures the current
 *   arrangement, not the imagined replacement.
 *
 * KEY AGENTS:
 *   - animals: moral subjects, bearers of interests, trapped in instrumental use arrangements without voice or recourse
 *   - animal_advocates: bear identity-locked costs of resistance, constituted through ethical refusal
 *   - extractive_industries: institutional beneficiaries collecting rents from commodification
 *   - consumer_state: benefits from low-cost animal products and research capacity
 *   - welfare_reformed_institutions: occupying a contested middle ground — accepting welfare improvement while defending commodity status
 *   - philosophical_naturalists: analytical observers examining the reading's structural consistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.0).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.92).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Instrumental Use (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '38a4dd66-8d72-4a10-bc2f-14ccf72d37db').
narrative_ontology:cs_kernel_codification('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', distributed).
narrative_ontology:cs_authority_grounding('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', distributed).
narrative_ontology:cs_reading_relation('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', animal_status__welfare_reading, influences).
narrative_ontology:cs_axiom('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', foundational, sentience_grounds_moral_status).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', sentience_grounds_moral_status, deontological).
narrative_ontology:cs_axiom('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', foundational, moral_status_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(moral_status_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', moral_status_precludes_instrumental_use, deontological).
narrative_ontology:cs_reference_frame('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', animal_rights_framework).
narrative_ontology:cs_drift_state('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', contemporary_2026, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('38a4dd66-8d72-4a10-bc2f-14ccf72d37db', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animal_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, extractive_industries).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumer_state).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, welfare_reformed_institutions).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, welfare_reformed_institutions).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, animal_moral_status).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, rights_bearer_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct cost of instrumental use — confinement, exploitation, slaughter, research subject status, entertainment commodification. Have no formal voice in the arrangement, no ability to refuse participation, no recourse through existing legal systems. The constraint claims they are moral subjects; actual practice denies them standing to contest their own treatment.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals, payer,
    powerless, biographical, trapped, global).

% Bear psychological and organizational costs of resistance: moral injury from witnessing systemic exploitation, resource drain from advocacy work against entrenched institutional interests, social marginalization and professional liability for challenging commodity status. Their identity is constituted through the refusal of instrumental use; exit would require abandoning the ethical framework itself.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_advocates, payer,
    moderate, generational, identity_locked, global).

% Collect rents from animal commodification: agriculture, pharmaceutical testing, entertainment, research, clothing production. Justify continued use through property-rights doctrine and welfare-compliance theater (humane certification, certified-humane slaughter, enrichment protocols). Their institutional power is defended by legal frameworks treating animals as objects, not subjects.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, extractive_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Benefits from cheap animal-derived goods and services: food production costs stay low, pharmaceutical innovation bypasses human trials through animal testing, research agendas set by institutional capacity rather than ethical constraint. Collects tax revenue from extractive industries and maintains political support from constituent consumers of animal products.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumer_state, beneficiary,
    institutional, generational, arbitrage, national).

% Adopt welfare compliance measures (cage-free certification, slaughter standards, research protocols) that reduce but do not eliminate instrumental use. They benefit from reduced resistance, improved market positioning, and maintained commodity status; they also bear the cost of welfare infrastructure and accept a narrower extraction margin. Their position is threatened by the abolitionist claim that welfare is false consciousness.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reformed_institutions, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, welfare_reformed_institutions, payer).

% Analyze the question from outside any institutional interest: what does the referent (the standing arrangement of animal instrumental use) look like when examined by the abolitionist reading's own lights? How does the rights-bearer claim compare empirically to the property and welfare readings? What structural evidence would differentiate the readings?
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, philosophical_naturalists, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(animal_status__abolitionist_reading, philosophical_naturalists).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, extractive_industries).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The reading rejects framing instrumental use as coordination at all — animals did not consent to the arrangement, their interests are not balanced, welfare measures do not resolve the fundamental conflict. The arrangement coordinates ONLY among human beneficiaries and the state around the exclusion of animal subjectivity.
% TRANSFER_FUNCTION: Moves labor, attention, and biological capital from animals (extracted at zero reciprocal benefit) to humans and institutions: extracted energy in agriculture, extracted biological material in pharmaceuticals and research, extracted performance in entertainment. The transfer is asymmetric because the reading denies the legitimacy of the entire extraction mechanism.
% ABSENT_VOICES: Animals themselves are structurally absent from adjudication — they cannot testify, cannot hire counsel, cannot participate in policy formation. Animal advocates are present but marginalized, denied standing in legal proceedings, and treated as special interests rather than genuine stake-holders. The reading asserts this structural absence IS the violation: a moral subject excluded from its own case.
% DISAPPEARANCE_RATIONALE: If this reading became legally binding (instrumental use precluded, animals recognized as rights-holders), the world reorganizes substantially: agriculture shifts to plant-based systems, pharmaceutical testing uses alternatives, research protocols exclude animal subjects, entertainment industries divest from animal performance. The extraction mechanism would be dismantled; benefiting institutions would lose directly.
% FOUNDING_PROBLEM: The founding problem, from the abolitionist seat, is metaphysical: animals ARE moral subjects with interests of their own, yet institutional arrangements treat them as objects. The problem is NOT animal welfare (what do we owe animals once we own them) but animal status (do we have any right to own them at all).
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the abolitionist advocacy community: comparative animal cognition research documents sophisticated preferences, social bonds, and pain aversion; evolutionary biology confirms continuity of nervous systems across species; philosophical analysis in secular and religious traditions recognizes animal subjectivity. Extractive industries and the consumer state do not attest the problem — their denial is exactly what the reading claims is false consciousness. Welfare-reformed institutions acknowledge animal sentience but deny it entails rights; that disagreement is the domain boundary.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).
:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The abolitionist reading produces ε = 0.0 because the reading's core claim is that instrumental use violates rights — the moment the constraint is recognized as what it claims to be (violation of moral subjectivity), it has ZERO extractive content (extraction presumes legitimacy; violated rights are not extraction, they are violation). Suppression is extremely high (0.92) because the arrangement's persistence DEPENDS on suppressing recognition of animal moral status. The theater ratio (0.68) reflects the dominance of welfare-compliance theater: most enforcement activity since ~1980 defends the narrative of 'humane use' rather than the fact of use itself. The measurement series tracks rising theater from 1970–2026 (welfare certifications, slaughter reforms, research protocols) and rising suppression (more elaborate justification systems needed as the moral case for animals has strengthened). The accessibility_collapse (0.78) is moderately high: once the reading is understood, alternatives (plant-based agriculture, non-animal testing, observational research) are already technically available; the collapse comes from institutional path-dependence and economic interest, not from natural limits. Resistance (0.71) is substantial: advocacy movements, legislative efforts, academic ethics challenges all mount active resistance — the constraint persists despite real counter-pressure.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between the abolitionist reading and its siblings (welfare and property readings) is STRUCTURAL, not empirical disagreement about facts: all three readings can accept identical cognitive and sentience data about animals. The property reading says: sentience is present, but does not entail moral rights (instrumental use is permissible). The welfare reading says: sentience grounds interests, interests ground constraints on use, but use remains permissible if welfare standards are met. The abolitionist reading says: sentience grounds interests, interests ground rights, rights preclude instrumental use. Each reading instantiates a different constraint because each assigns different beneficiary/victim structure and different permission-status to the same instrumental-use practice. The claim/metric independence principle applies across readings: ε = 0.0 for the abolitionist reading should diverge sharply from the welfare reading's ε (likely 0.35–0.55, measuring extraction from welfare constraints) and the property reading's ε (likely 0.1–0.25, measuring only the costs of property administration). That divergence IS THE MEASUREMENT THE CORPUS EXISTS TO TAKE.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals carry d = 1.0 (full target): they are trapped without exit, powerless, bearing direct costs of the arrangement with zero participation in its governance. Animal advocates carry d ≈ 0.85–0.95 (near-target): they are identity_locked (exit would require abandoning the ethical framework), moderate power, bearing substantial psychological and organizational costs with no corresponding benefit from the arrangement itself. Extractive_industries carry d ≈ 0.0 (full beneficiary): they set the rules, collect rents, have arbitrage-level exit (they choose to participate). Consumer_state carries d ≈ 0.1–0.2 (mostly beneficiary): institutional power, generational time horizon, but constrained by constituency pressure and rising welfare expectations — not full arbitrage. Welfare_reformed_institutions carry d ≈ 0.35–0.45 (ambiguous): they collect rents from the constraint while bearing the cost of welfare infrastructure and accepting moral-status vulnerability. The directionality derivation need not be perfect; the reading's coherence rests on the structural data being internally consistent, not on the d values landing in any particular range.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading does not face a mandatrophy question in the classical sense (founding problem persists, suppression is live, no zombie-constraint problem). However, the reading DOES face a conceptual mandatrophy risk: if the moral status of animals were to be fully accepted (recognized by law, enforced in practice), the constraint would become moot. But this is not the constraint's fate under threat — it is the reading's theory of what justice would require. The classification remains snare-coded because persistence DOES depend on active suppression, and the arrangement's operation DOES suppress alternative framings. No zombie-ness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_criterion_ambiguity,
    'What criterion grounds moral status? Sentience (capacity to suffer)? Cognition (self-awareness, planning)? Language capacity? Relational presence (ability to enter into covenant)? The abolitionist reading claims sentience suffices; property reading denies sentience entails status; welfare reading accepts sentience but denies it entails rights.',
    'Philosophical analysis of the logical entailment from sentience to rights; empirical clarification of which animals meet which criteria; mapping of institutional positions onto criterion choice.',
    'Different criteria ground different victim sets and different ε values. If sentience suffices, ε = 0.0 (violation of rights). If something more is required (language, self-awareness), ε rises toward welfare territory. The reading hinges on the criterion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_criterion_ambiguity, conceptual, 'What property of animals grounds their moral status?').

omega_variable(
    reading_identity_and_institutional_capture,
    'Is the abolitionist reading a genuine alternative moral framework, or has it been institutionally captured by reformist movements that use ''abolition'' as a rhetorical brand while defending welfare-compliant use?',
    'Document institutional adoption of the reading; analyze whether stated commitments to abolition translate to policy opposition to ALL instrumental use or only to the worst practices; track whether advocates maintain independence from welfare-reform infrastructure.',
    'If captured, the reading''s suppression rises (its authentic voice becomes confused with welfare theater). If independent, suppression measures the gap between the reading''s vision and institutional practice. The classification remains snare in either case (suppression is high), but the REASON for suppression changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_and_institutional_capture, empirical, 'Whether the abolitionist reading remains institutionally independent from welfare reform movements.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.92) primarily structural (legal barriers, economic cost, institutional exclusion) or internalized (the public has genuinely come to believe animals are property and welfare is sufficient)? If animals were legally recognized as rights-holders tomorrow, would the suppression persist at behavioral level?',
    'Post-recognition behavioral tracking: do people cease instrumental use after legal recognition, or does suppression persist? Survey data on belief vs. practice divergence.',
    'If suppression is structural, legal change could shift the constraint rapidly. If internalized, even legal recognition would leave the constraint''s psychological persistence intact. The theater-ratio rise suggests internalization is significant: welfare theater reproduces belief in legitimate use even as suffering persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether the suppression is structural (external barriers) or internalized (belief that the arrangement is legitimate).').

omega_variable(
    animal_advocate_identity_lock_mechanism,
    'The animal_advocate seat is coded as identity_locked (exit means abandoning the ethical framework). Is this identity lock: professional (career path), relational (identity constituted through relationships with animals), ideological (worldview-level commitment), or institutional (organizational belonging)? What would actually break the identity lock?',
    'Qualitative analysis of advocate decision points: what circumstances might lead advocates to exit? Historical analysis of identity-lock dissolution in analogous movements (abolition of human slavery, women''s suffrage).',
    'Understanding the lock mechanism informs whether the reading''s suppression can be overcome by changing material conditions (removing identity-lock mechanism) or whether the reading is genuinely constitutive of advocate identity. If institutional identity-lock (advocate careers depend on the movement), changing institutions changes the reading''s constituency. If ideological lock, only conversion changes it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_advocate_identity_lock_mechanism, empirical, 'What kind of identity lock binds animal advocates to the abolitionist reading?').

omega_variable(
    kernel_reading_foreclosure,
    'Do the abolitionist and property readings genuinely foreclose each other, or do they coexist as held by different institutional seats? That is: can a party hold both (rights exist AND property is legitimate), or does holding abolitionist premises logically rule out property premises?',
    'Analyze the core premises: if ''animals have moral rights to bodily autonomy'' is true, can ''humans have unlimited ownership rights over animals'' also be true in the same framework? The question is logical compatibility, not factual agreement.',
    'If foreclosed, the readings are in hard contradiction and the constraint family exhibits a rare `forecloses` edge. If coexistent, they represent competing institutional interests and exhibit `coexists_with`. The choice affects how the engine models the readings'' relationship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the abolitionist and property readings logically foreclose each other or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_theater_1970, animal_status__abolitionist_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement_basis(animal_theater_1970, observed).
narrative_ontology:measurement(animal_theater_1985, animal_status__abolitionist_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement_basis(animal_theater_1985, observed).
narrative_ontology:measurement(animal_theater_2000, animal_status__abolitionist_reading, theater_ratio, 2000, 0.51).
narrative_ontology:measurement_basis(animal_theater_2000, observed).
narrative_ontology:measurement(animal_theater_2013, animal_status__abolitionist_reading, theater_ratio, 2013, 0.62).
narrative_ontology:measurement_basis(animal_theater_2013, observed).
narrative_ontology:measurement(animal_theater_2026, animal_status__abolitionist_reading, theater_ratio, 2026, 0.68).
narrative_ontology:measurement_basis(animal_theater_2026, observed).

% Extraction over time
narrative_ontology:measurement(animal_extractiveness_1970, animal_status__abolitionist_reading, base_extractiveness, 1970, 0.0).
narrative_ontology:measurement_basis(animal_extractiveness_1970, observed).
narrative_ontology:measurement(animal_extractiveness_1985, animal_status__abolitionist_reading, base_extractiveness, 1985, 0.0).
narrative_ontology:measurement_basis(animal_extractiveness_1985, observed).
narrative_ontology:measurement(animal_extractiveness_2000, animal_status__abolitionist_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(animal_extractiveness_2000, observed).
narrative_ontology:measurement(animal_extractiveness_2013, animal_status__abolitionist_reading, base_extractiveness, 2013, 0.0).
narrative_ontology:measurement_basis(animal_extractiveness_2013, observed).
narrative_ontology:measurement(animal_extractiveness_2026, animal_status__abolitionist_reading, base_extractiveness, 2026, 0.0).
narrative_ontology:measurement_basis(animal_extractiveness_2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(animal_suppression_1970, animal_status__abolitionist_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement_basis(animal_suppression_1970, observed).
narrative_ontology:measurement(animal_suppression_1985, animal_status__abolitionist_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement_basis(animal_suppression_1985, observed).
narrative_ontology:measurement(animal_suppression_2000, animal_status__abolitionist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(animal_suppression_2000, observed).
narrative_ontology:measurement(animal_suppression_2013, animal_status__abolitionist_reading, suppression_requirement, 2013, 0.81).
narrative_ontology:measurement_basis(animal_suppression_2013, observed).
narrative_ontology:measurement(animal_suppression_2026, animal_status__abolitionist_reading, suppression_requirement, 2026, 0.92).
narrative_ontology:measurement_basis(animal_suppression_2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status__abolitionist_reading, 0.12).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three constraint stories: property_reading (animals as legal objects, ε ≈ 0.1–0.25), welfare_reading (animals as sentient beings with interests constraining use, ε ≈ 0.35–0.55), and abolitionist_reading (animals as moral subjects precluding instrumental use, ε = 0.0). Each reading instantiates a different constraint from the same domain: they share a referent (the standing arrangement of animal commodification) but assess it under different moral frameworks, producing different ε values and different victim sets. The abolitionist reading's ε = 0.0 is not a disagreement about facts but a structural claim: under this reading's framework, instrumental use violates rights and has zero legitimate extraction content. The readings are linked because institutional adoption of one reading affects the operational conditions of the others: welfare reforms defend property doctrine and undermine abolitionist claims; abolitionist success would collapse the welfare and property readings entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
