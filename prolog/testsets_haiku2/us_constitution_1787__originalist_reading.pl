% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Constitutional Meaning Fixed at Ratification (Originalist Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the originalist reading of the
 *   contested kernel 'us_constitution_1787'—the claim that constitutional
 *   meaning is fixed at ratification and framers' intent is binding. The
 *   originalist reading anchors constitutional interpretation to historical
 *   fact: the public meaning of text as understood in 1787 and the specific
 *   intentions of those who drafted and ratified it. This reading presents
 *   itself as a coordination mechanism (stabilizing interpretation,
 *   preventing arbitrary judicial expansion) while operating as an enforced
 *   extraction from modern rights advocates and historically marginalized
 *   communities whose claims lack 1787 historical grounding. The constraint
 *   is classified as tangled_rope because it solves a genuine coordination
 *   problem (interpretive stability) while simultaneously extracting from
 *   those whose social claims fall outside the narrow boundary it sets.
 *   Active enforcement is required because rival readings
 *   (living-constitution and positivist interpretations) remain alive in
 *   competing parts of the judiciary and academy; originalism persists by
 *   continuously defending its boundaries against alternative framings.
 *
 * KEY AGENTS:
 *   - Originalist judiciary: Federal judges enforcing the 1787-meaning boundary through opinions and jurisprudence
 *   - Constitutional federalists: Political factions benefiting from narrow readings that protect state sovereignty
 *   - Property rights holders: Institutional and individual beneficiaries protected from modern regulatory expansions
 *   - Modern rights advocates: Organized movements seeking constitutional recognition of unenumerated rights
 *   - Marginalized communities: Powerless victims whose exclusion from 1787 ratification means systematic invisibility in originalist history
 *   - Historical evidence gatekeepers: Originalist historians and legal scholars whose expertise becomes binding through this reading
 *   - Living-constitution judiciary: Excluded alternative that would expand constitutional meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Constitutional Meaning Fixed at Ratification (Originalist Reading)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '72089a38-b383-4ca0-9c7b-31321dc620f7').
narrative_ontology:cs_kernel_codification('72089a38-b383-4ca0-9c7b-31321dc620f7', fixed_text).
narrative_ontology:cs_authority_grounding('72089a38-b383-4ca0-9c7b-31321dc620f7', lineage).
narrative_ontology:cs_interpretation_layer_present('72089a38-b383-4ca0-9c7b-31321dc620f7').
narrative_ontology:cs_reading_relation('72089a38-b383-4ca0-9c7b-31321dc620f7', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_reading_relation('72089a38-b383-4ca0-9c7b-31321dc620f7', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('72089a38-b383-4ca0-9c7b-31321dc620f7', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('72089a38-b383-4ca0-9c7b-31321dc620f7', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('72089a38-b383-4ca0-9c7b-31321dc620f7', foundational, framers_intent_is_binding).
narrative_ontology:cs_axiom_status(framers_intent_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('72089a38-b383-4ca0-9c7b-31321dc620f7', framers_intent_is_binding, empirically_contingent).
narrative_ontology:cs_axiom('72089a38-b383-4ca0-9c7b-31321dc620f7', secondary, historical_evidence_is_determinative).
narrative_ontology:cs_axiom_status(historical_evidence_is_determinative, holdable).
narrative_ontology:cs_axiom_grounding('72089a38-b383-4ca0-9c7b-31321dc620f7', historical_evidence_is_determinative, empirically_contingent).
narrative_ontology:cs_reference_frame('72089a38-b383-4ca0-9c7b-31321dc620f7', fixed_meaning_at_ratification).
narrative_ontology:cs_drift_state('72089a38-b383-4ca0-9c7b-31321dc620f7', contemporary_constitutional_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('72089a38-b383-4ca0-9c7b-31321dc620f7', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, constitutional_federalists).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, property_rights_holders).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, marginalized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, historical_evidence_gatekeepers).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_academic_network).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges committed to originalist methodology who interpret the Constitution by reference to its public meaning at ratification. They set the boundary of legitimate constitutional claims by requiring historical evidence of framers' intent and widespread original public meaning. They enforce this interpretive constraint through opinions, dissents, and jurisprudential influence that shapes the admissible range of constitutional arguments in courts nationwide.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Political and intellectual factions (originalist scholars, conservative foundations, federalist think tanks, legislatures in originalist-controlled states) who benefit from the constraint because it narrows the field of legitimate constitutional claims against their preferred arrangements. They defend state sovereignty, property rights, and traditional institutional boundaries by invoking originalist readings that treat modern expansions of federal power as illegitimate novelties.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_federalists, beneficiary,
    institutional, generational, mobile, national).

% Individuals and corporations whose property and commercial interests are protected by originalist readings that reject modern regulatory and social welfare expansions. The constraint keeps certain redistributive, environmental, and labor-safety claims outside the constitutional boundary, protecting their discretion from judicial enforcement of unenumerated rights.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, property_rights_holders, beneficiary,
    powerful, biographical, mobile, national).

% Constitutional scholars, civil rights organizations, and political movements seeking to vindicate claims of privacy, dignity, equality, and social provision that do not appear explicitly in 1787 text. The constraint subjects their arguments to an epistemic demand—proof of framers' intent or original public meaning—that is difficult to satisfy for novel social claims. They must either excavate historical evidence that was not preserved or abandon their claims as unconstitutional.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, modern_rights_advocates, payer,
    organized, biographical, constrained, national).

% Communities whose historical exclusion from ratification (enslaved people, women, Native Americans, non-property-owners) means their dignity interests are systematically invisible in originalist history. The constraint's requirement for historical originalism excludes their claims by construction—no framers' intent exists for rights they were not considered to have. They bear the cost of having their constitutional status determined by evidence of what a homogeneous, property-owning male class intended in 1787.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, marginalized_communities, payer,
    powerless, biographical, trapped, national).

% Judges and scholars committed to evolutionary constitutional interpretation would argue for a different framework—one in which meaning evolves with society and constitutional text functions as an aspirational foundation for growth. They are systematically excluded from the originalist reading's framework; their interpretive methodology is deemed illegitimate by the constraint itself.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitution_judiciary, excluded,
    institutional, generational, constrained, national).

% Originalist historians, legal historians, and historical consultants whose expertise becomes determinative of constitutional meaning under this reading. The constraint elevates historical inquiry to the status of binding constitutional law, creating a professionalized gatekeeping role. These experts benefit from the epistemic power this reading grants them, though they are also constrained by the historical record's gaps and ambiguities.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, historical_evidence_gatekeepers, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, historical_evidence_gatekeepers, beneficiary).

% Law schools, legal scholarship networks, and intellectual institutions organized around originalist methodology. They benefit from the constraint because it makes originalism a dominant interpretive framework, channeling resources, hiring, publication, and intellectual prestige toward originalist scholars and maintaining originalism as the default constitutional language of educated legal discourse.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_academic_network, beneficiary,
    organized, generational, mobile, global).

% Elected majorities whose legislative judgments about modern constitutional needs are excluded from the constraint's legitimate space. If they pass social welfare, civil rights, or environmental legislation based on reasoning that the Constitution should evolve to address modern conditions, originalist constraint excludes that reasoning as non-constitutional. They must legislate without invoking the Constitution as a living framework or see their laws invalidated.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, legislative_majorites, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, originalist_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional interpretation by anchoring it to a fixed, discoverable historical reference point (framers' intent and 1787 public meaning), preventing arbitrary reinterpretation and providing a common interpretive standard across judges and jurisdictions. Coordinates the judiciary around a shared epistemic method.
% TRANSFER_FUNCTION: Transfers interpretive power from elected representatives and contemporary constituencies to historical actors and originalist experts who determine what the framers intended. Moves the burden of constitutional change from interpretation (which is judicially manageable under living readings) to the formal amendment process (which is institutionally difficult). Transfers the validation of social claims from moral-reasoning space to historical-evidence space, where certain communities' interests are systematically invisible.
% ABSENT_VOICES: Living-constitution advocates and scholars are excluded from the framework itself—their methodology is deemed illegitimate rather than merely contested. Modern social movements and marginalized communities whose concerns (dignity rights, equality expansions, social provision) lack 1787 framers' intent are systematically absent from the constraint's epistemic space. Democratic majorities seeking to resolve modern questions through constitutional reasoning are excluded unless they can prove their answers were intended in 1787.
% DISAPPEARANCE_RATIONALE: If the originalist constraint vanished overnight, constitutional interpretation would immediately expand to include living-constitution readings; social movements would have access to expansive constitutional arguments that are currently foreclosed; judges would invoke contemporary values and social needs in constitutional reasoning; and the scope of enforceable constitutional rights would shift dramatically toward modern social claims. The political equilibrium would reorganize around different interpretive frameworks.
% FOUNDING_PROBLEM: Early post-ratification constitutional interpretation was unstable and contested, with Federalists and anti-Federalists offering incompatible readings. The Constitution's meaning needed to be fixed by reference to something durable: the intent of those who made it, the original public meaning of the text at the moment of ratification. Originalism emerged as a proposal to constrain interpretive arbitrariness by anchoring meaning to historical fact rather than contemporary politics.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative jurists attest the founding problem remains live—they invoke interpretive instability and Lochner-era excess as evidence that original meaning is necessary to constrain judges. Living-constitution scholars, civil rights advocates, and legislative historians argue the founding problem is substantially solved; modern jurisprudence has settled on interpretive traditions (even if evolving ones); the constraint now functions to lock in particular outcomes rather than to stabilize interpretation. Academic historians have documented that 'original public meaning' is itself a constructed, contingent historical claim, not an objective fact, and that different historical evidence supports different readings—the constraint is not solving the stability problem it claims to solve.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.42) in early periods when originalism is a contested methodology among co-equal interpretive schools, rises steadily to 0.68 as originalist jurists achieve institutional dominance (Federalist Society influence, Federal Judicial Center changes, appointment patterns shift judiciary composition), and plateaus as the constraint becomes entrenched in a now-majority originalist Supreme Court. Theater ratio rises from 0.22 to 0.41 as originalism increasingly wraps institutional power-grabs in historical-evidence language—the constraint's justification shifts from 'this is the correct methodology among competing methods' to 'historical evidence is the only legitimate voice in constitutional debates,' a performative move that excludes alternatives by fiat. Suppression is high and stable (0.58→0.72) because the constraint's enforcement depends continuously on blocking rival interpretive frameworks—living-constitution readings must be actively suppressed in law school curricula, judicial appointments, and mainstream legal rhetoric, or the constraint collapses. The measurement series shares one time grid so all metrics are authored at the same six time points (0, 8, 16, 24, 32, 40), enabling temporal integration and drift analysis.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judiciary and federalist beneficiaries perceive this constraint as pure coordination: a shared methodology that stabilizes interpretation and prevents arbitrary judges from imposing contemporary values. From their seat, the constraint is a rope—a genuine solution to a real interpretive stability problem. Modern rights advocates and marginalized communities perceive the same constraint as extractive domination: an epistemic gatekeeping mechanism that forecloses their claims by construction. The methodological claim (historical evidence is binding) is experienced as substantive extraction (your claim is illegitimate because the people who wrote this document did not intend it). The engine computes this divergence from the structural positions: beneficiary seats derive low d → perceived as coordination; victim seats derive high d → perceived as extraction. The claim/metric gap is intentional: originalism CLAIMS to be pure coordination (rope) while operating with substantial extraction that requires active suppression of rival methodologies to persist. That gap is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judiciary and constitutional federalists are structural beneficiaries (d approaches 0.0): they set the agenda, enforce the boundaries, and benefit from a narrow constraint that protects their preferred institutional arrangements. Property rights holders are clear beneficiaries (d low): they escape modern regulatory expansions by invoking originalist readings that exclude social-welfare and environmental claims. Modern rights advocates face extraction (d toward 1.0): they must prove framers' intent for claims their societies now need, an evidentiary burden uniquely imposed on their arguments; their alternatives (living-constitution reasoning, contemporary-values interpretation) are foreclosed. Marginalized communities face maximum extraction (d → 1.0): they are trapped by an epistemic standard (historical originalism) that is structurally impossible for them to satisfy, because their exclusion from 1787 means no historical evidence of their intended rights exists. The living-constitution judiciary is excluded rather than target: they are not extracted from directly but are systematically prevented from participating in the constraint's interpretive framework. Historical evidence gatekeepers occupy a dual position: they benefit from the constraint (their expertise becomes binding) while being partially constrained by the historical record's gaps and ambiguities. The directionality chain is transparent: beneficiary/victim declarations → low d for beneficiaries → high d for victims → high effective extraction χ for payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interpretive instability in post-ratification debates) is classically identified with living-constitution advocates and originalist scholars alike—both invoke it to defend their reading. The originalist reading claims the problem is LIVE: modern judges still face Lochner-era temptations and must be constrained by historical fact. Living-constitution scholars and legislative historians attest the problem is DEAD or SUBSTANTIALLY TRANSFORMED: modern jurisprudence has settled into interpretive traditions (even if evolving ones); originalism itself has become a contested doctrine generating its own instability; the constraint now functions to lock in particular outcomes rather than to enable stable interpretation (historicists have shown that different historical evidence supports different originalist conclusions; there is no single 'original public meaning,' only competing historical readings defended by experts with different methodologies and political commitments). The mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) signals mandatrophy: the constraint's foundational justification (we need historical anchoring to prevent arbitrary reinterpretation) has outlived its function (modern interpretation is stable enough, but the constraint now prevents the world from addressing the needs for which living readings were developed). The constraint persists not because the founding problem is live but because institutional actors benefit from maintaining it. Classification as tangled_rope rather than piton reflects the real coordination function (interpretive standards, shared methodology) that persists alongside the extraction; pitons have atrophied coordination. But the theater_ratio rise and extraction accumulation signal that theatrical maintenance of the founding-problem justification is increasing as its actual function declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_determinacy,
    'Is ''original public meaning at ratification'' a determinate historical fact discoverable by research, or is it a constructed interpretive claim that varies depending on which historical evidence is weighted, which experts are consulted, and which methodologies are applied?',
    'Examine cases where originalist scholars offer conflicting historical interpretations of the same constitutional provision (e.g., Second Amendment, Commerce Clause, Fourteenth Amendment). If multiple originalist historians reach incompatible conclusions from the same evidence base, original meaning is not a constraint on interpretation but rather a site of contestation that substitutes one form of indeterminacy for another.',
    'If original meaning is indeterminate, the constraint''s epistemic power is illusory—it does not stabilize interpretation but relocates indeterminacy to historical scholarship and expert gatekeeping. This would reclassify the constraint from tangled_rope (coordination + extraction) to snare (pure extraction wrapped in coordination framing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_public_meaning_determinacy, empirical, 'Whether original public meaning is a determinate historical fact or a contestable interpretive construction.').

omega_variable(
    structural_invisibility_of_excluded_communities,
    'Does the originalist methodology structurally preclude the constitutional recognition of rights for communities that were excluded from ratification (enslaved people, women, Native Americans, non-property-owners), and if so, is this a feature or a bug of the constraint?',
    'Compare originalist and living-constitution approaches to the constitutional status of slavery, women''s rights, and indigenous sovereignty. Document whether originalist readings can generate constitutional claims on behalf of historically excluded communities, or whether the methodology systematically forecloses such claims.',
    'If originalism systematically forecloses rights for excluded communities by construction, the constraint is not merely extractive toward modern rights advocates but is fundamentally a mechanism for locking in the exclusions of 1787. This would elevate the constraint''s extraction profile and strengthen classification as snare rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_invisibility_of_excluded_communities, empirical, 'Whether originalism can generate constitutional rights for historically excluded communities or forecloses them by methodology.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (interpretive instability in post-ratification debates) still the organizing problem that justifies the originalist constraint, or has modern constitutional practice settled into stable interpretive traditions such that the constraint now functions primarily to lock in particular outcomes rather than to solve an active coordination problem?',
    'Historical analysis of judicial consistency and doctrinal stability over time. Measure whether modern constitutional interpretation (under competing readings) exhibits more or less instability than it would under originalism alone. Consult legislative historians about whether modern democracies face interpretive problems that originalism solves or merely prevents them from addressing.',
    'If the founding problem is obsolete, the constraint crosses into mandatrophy territory: it solves a problem that no longer exists and persists primarily due to institutional inertia and beneficiary capture. This would support a reclassification toward piton (performance-heavy, benefit-thin) and flag the constraint for structural dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem justifying originalism remains live or has been substantially resolved.').

omega_variable(
    kernel_reading_identity,
    'How does the originalist reading of us_constitution_1787 maintain its coherence as a single reading, given that ''originalism'' itself is not monolithic—originalists disagree sharply about methodology (original public meaning vs. framers'' intent vs. original law), scope (the Fourteenth Amendment as a modifier of original meaning), and application across domains (originalist results differ on Second Amendment, Commerce Clause, substantive due process)?',
    'Document the range of ''originalist'' positions held by self-identified originalist scholars and judges. Where the range becomes so wide that different originalists reach incompatible constitutional conclusions, determine whether they are still instantiating the same reading or whether methodological variation has fractured the reading into multiple, competing readings.',
    'If originalism has fractured into multiple incompatible readings (e.g., private-meaning originalism vs. public-meaning originalism vs. law-of-the-time originalism), this constraint story may be over-inclusive, describing a family of readings rather than a single, coherent reading. Reclassification would require decomposition into multiple constraint stories, each capturing a specific originalist methodology with its own beneficiary structure and extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether originalism is a single coherent reading or a fractured family of competing methodologies.').

omega_variable(
    coexistence_vs_foreclosure_with_living_reading,
    'Does the originalist reading logically foreclose the living-constitution reading (one core premise directly contradicts the other within the same framework), or do they coexist as competing positions that different parties hold simultaneously?',
    'Examine whether the originalist thesis (''meaning is fixed at ratification'') logically entails the rejection of the living-constitution thesis (''meaning evolves with society''). These appear to be direct contradictions, but test whether they could coexist in a pluralist framework where meaning is fixed for some purposes and evoling for others, or whether accepting one truly requires rejecting the other.',
    'If originalism forecloses living-constitution readings, the reading_relations should specify ''forecloses'' rather than ''coexists_with''. If they coexist as different parties'' live positions, ''coexists_with'' is correct. The classification affects how the engine models the kernel contest and which readings it flags as mutually exclusive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_vs_foreclosure_with_living_reading, conceptual, 'Whether originalism and living-constitution readings are logically exclusive or can coexist as competing positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(us_c_tr_t8, us_constitution_1787__originalist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(us_c_tr_t16, us_constitution_1787__originalist_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_1787__originalist_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(us_c_tr_t32, us_constitution_1787__originalist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__originalist_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(us_c_be_t8, us_constitution_1787__originalist_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(us_c_be_t16, us_constitution_1787__originalist_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(us_c_be_t24, us_constitution_1787__originalist_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(us_c_be_t32, us_constitution_1787__originalist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__originalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(us_c_su_t8, us_constitution_1787__originalist_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(us_c_su_t16, us_constitution_1787__originalist_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(us_c_su_t24, us_constitution_1787__originalist_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(us_c_su_t32, us_constitution_1787__originalist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__originalist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, fourteenth_amendment_equal_protection_original_meaning).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, commerce_clause_original_scope).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'us_constitution_1787'. Sibling readings 'us_constitution_1787__living_reading' and 'us_constitution_1787__positivist_reading' instantiate alternative constraints on the same kernel, each with different ε values, beneficiary structures, and classification results. The three stories form a constraint family linked through the kernel; each is structurally independent but epistemically related. Upstream from this reading: the kernel 'us_constitution_1787' itself (the fixed text over which readings contest). Downstream from this reading: more specific originalist applications to particular constitutional provisions (commerce clause, fourteenth amendment, etc.). The network enables tracking how originalist readings of the whole constitution propagate constraints on specific amendments and clauses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
