% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra Normative Authority under the Reformist Contextual Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The standing arrangement under contest, as this reading holds it: the
 *   Dharmasastra corpus remains a live normative authority for a large
 *   devotional community, but its prescriptions are filtered through the
 *   separability thesis — dharma as righteous conduct endures, while
 *   birth-ranked social prescriptions belonged to their historical moment and
 *   are retired from binding force. The interpretive establishment teaches
 *   this filtering, certifies orthopraxy within it, and collects the
 *   deference and material support that follow from controlling the
 *   core/context boundary; upper-caste laity retain symbolic precedence
 *   without enforcing it; dalit and lower-caste communities bear the residual
 *   of the old ranking inside a fold that now promises them equality it
 *   delivers at the interpreters' chosen pace. KEY AGENTS (by structural
 *   relationship): - reformist_theological_establishment: Agenda-setting
 *   interpreter (institutional/identity_locked) — draws the
 *   core-versus-context boundary, administers the settlement, and collects
 *   its receipts - upper_caste_devout_laity: Primary symbolic beneficiary
 *   (powerful/identity_locked) — inherited precedence priced in ceremony and
 *   marriage - dalit_and_lower_caste_communities: Primary target
 *   (organized/constrained) — bears residual hierarchy and presses for
 *   recognition at the establishment's pace - hindu_devout_laity:
 *   Near-symmetric participant (moderate/constrained) — receives ethical
 *   coordination, pays deference and obligation -
 *   ambedkarite_abolitionist_activists: Excluded critic (organized/mobile) —
 *   rejects the frame from outside and exercises exits the establishment does
 *   not control - indian_constitutional_courts: Observer
 *   (institutional/analytical) — reshapes what remains legally enforceable
 *   without claiming a doctrinal seat Constraint-family note: the colloquial
 *   label 'the authority of Dharmasastra' decomposes into three
 *   epsilon-distinct arrangements linked by network.affects_constraints —
 *   orthodox_literalist (eternal prescriptions, literal enforcement, maximal
 *   victim set), this reformist_contextual settlement (conditioned texts,
 *   separable core, medium extraction, reduced victim set), and
 *   abolitionist_rejection (constitutive oppression, no legitimate authority,
 *   no beneficiary seats). This story authors epsilon only for the reformist
 *   arrangement itself, assessed by that reading's own lights; sibling
 *   readings are other files, not hedges folded into this one.
 *
 * KEY AGENTS:
 *   - reformist_theological_establishment: agenda_setter (institutional/identity_locked) — administers the separability settlement and collects deference, offerings, and endowments
 *   - upper_caste_devout_laity: beneficiary (powerful/identity_locked) — retains ceremonial and matrimonial precedence without enforcing the old exclusions
 *   - dalit_and_lower_caste_communities: payer (organized/constrained) — bears residual symbolic and material hierarchy, mobilizes politically, presses the establishment for recognition
 *   - hindu_devout_laity: beneficiary with secondary_role payer (moderate/constrained) — receives ethical instruction and lifecycle rites, pays deference and inherits softening obligations
 *   - ambedkarite_abolitionist_activists: excluded (organized/mobile) — argues the hierarchy was constitutive, not incidental, and exercises conversion and secular-law exits
 *   - indian_constitutional_courts: observer (institutional/analytical) — adjudicates collisions between prescription and constitutional equality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.58).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.45).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.58).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra Normative Authority under the Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f').
narrative_ontology:cs_kernel_codification('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', fixed_text).
narrative_ontology:cs_authority_grounding('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', lineage).
narrative_ontology:cs_interpretation_layer_present('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f').
narrative_ontology:cs_reading_relation('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', foundational, dharma_core_separable_from_timebound_prescriptions).
narrative_ontology:cs_axiom_status(dharma_core_separable_from_timebound_prescriptions, holdable).
narrative_ontology:cs_axiom_grounding('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', dharma_core_separable_from_timebound_prescriptions, empirically_contingent).
narrative_ontology:cs_axiom('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', foundational, textual_lineage_retains_normative_authority).
narrative_ontology:cs_axiom_status(textual_lineage_retains_normative_authority, holdable).
narrative_ontology:cs_axiom_grounding('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', textual_lineage_retains_normative_authority, conventional).
narrative_ontology:cs_axiom('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', secondary, varna_reads_as_guna_karma_not_birth).
narrative_ontology:cs_axiom_status(varna_reads_as_guna_karma_not_birth, holdable).
narrative_ontology:cs_axiom_grounding('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', varna_reads_as_guna_karma_not_birth, theological).
narrative_ontology:cs_reference_frame('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', perennial_core_conditioned_transmission).
narrative_ontology:cs_drift_state('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', contemporary_constitutional_ambedkarite_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f3388b59-52dd-4b17-9baa-8a7b8a4ecc3f', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_theological_establishment).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, upper_caste_devout_laity).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, dalit_and_lower_caste_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, hindu_devout_laity).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, hindu_devout_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches, publishes commentaries, trains successors in mathas and seminaries, and adjudicates which prescriptions express enduring dharma and which belonged to their historical moment. Certifies orthopraxy within the reformist line and answers both traditionalists and rejectionists on behalf of the tradition. Deference, offerings, and endowments flow to its institutions, and the authority to define the boundary between core and context flows to its senior members. Leaving the role would mean dissolving the lineage identity that constitutes its members' vocation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_theological_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, reformist_theological_establishment, beneficiary).

% Descendants of families the old order ranked highly. They no longer enforce ritual exclusions and frequently fund reform charities, yet ceremonial precedence, surname prestige, and matrimonial markets continue to price their ancestry favorably. What they give up under the reformist settlement is enforcement duty, not standing. Exiting would mean relinquishing inherited status within the community that remains their social home.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, upper_caste_devout_laity, beneficiary,
    powerful, generational, identity_locked, continental).

% Carry the residual of the old ranking: marriage pools still largely closed, village temples and wells contested within living memory, labor stigma attached to ancestral trades, and everyday deference expectations that outlived their legal enforcement. Many have mobilized politically and through conversion movements, giving the class real collective capacity; staying inside the fold means petitioning interpreters who control the pace and depth of reform, while converting out carries legal and familial cost in several jurisdictions.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, dalit_and_lower_caste_communities, payer,
    organized, generational, constrained, continental).

% Receive ethical instruction, lifecycle rites, festival structure, and a usable moral vocabulary connecting daily conduct to a revered canon. They pay deference to teachers, sustain institutions financially, and inherit household duties whose gendered and hierarchical traces soften slowly. Realistic alternatives — secular ethics, another tradition, indifferent exit — carry family estrangement and identity costs that keep most inside while they negotiate terms.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, hindu_devout_laity, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, hindu_devout_laity, payer).

% Campaign from outside the interpretive conversation for wholesale abandonment of textual authority, arguing the hierarchy was constitutive rather than incidental to the corpus. Conversion, secular law, and independent political organization are open to them, and they exercise these exits — which is precisely why the establishment can keep them out of doctrinal deliberation without losing their labor or their dues. Their objection defines the outer boundary of the reformist conversation without being admitted into it.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, ambedkarite_abolitionist_activists, excluded,
    organized, generational, mobile, continental).

% Adjudicate collisions between the corpus's social prescriptions and constitutional equality: temple entry, priesthood eligibility, personal-law reform, protection of converts. Each ruling shrinks what remains legally enforceable while leaving the tradition's internal authority formally its own affair. They claim no doctrinal seat, take testimony from the other seats, and their remedies alter the arrangement's enforcement surface without touching its interpretive core.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, indian_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, reformist_theological_establishment).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, continuous ethical framework — dharma as righteous conduct — that coordinates conduct, lifecycle rites, festival rhythm, and communal identity across enormous regional, linguistic, and generational diversity, resolving disputes about right action by appeal to an interpreted canon rather than ad hoc authority.
% TRANSFER_FUNCTION: Moves interpretive authority and material support (deference, offerings, endowments, studentship) from lay devotees to the scholarly establishment; moves symbolic status upward along the residual hierarchy, as deference expectations and matrimonial valuation continue to run from lower-ranked to higher-ranked ancestry.
% ABSENT_VOICES: Dalit voices inside the fold are incorporated as objects of reform rather than co-authors of doctrine — consulted, cited, and accommodated at the establishment's discretion, with no seat in deciding where the core/context boundary falls. Abolitionist rejection is defined out of the conversation as leaving the tradition altogether, and women's interpretive voices remain marginal in the seminary structures that certify doctrine. They are outside the deliberative rooms: in activist networks, universities, courts, and conversion movements.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, religious instruction, ceremony, and the ethical vocabulary of hundreds of millions would lose their coordinating frame; the establishment's institutions would dissolve for want of mandate; upper-caste ceremonial precedence would lose its legitimating reference; and dalit communities would lose both a site of petition and a structure whose residual burdens they currently fight from within — marriages, temples, and communal identity would all reorganize around whatever replaced the canon.
% FOUNDING_PROBLEM: Codify righteous conduct — ritual, legal, domestic, and royal duty — for a society explicitly ordered by birth-ranked stations, specifying what each station owed and received so that social hierarchy and cosmic order were maintained together.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists but is split along the very fault this reading claims to transcend, and it comes from outside the benefiting parties. Academic indology (diachronic textual history of the smritis — stratification, redaction, dated variation across Manusmriti and successors) attests from a scholarly seat that the prescriptions were historically conditioned, supporting the reformist genealogy. Ambedkarite scholarship attests from outside the beneficiary set that the caste prescriptions were constitutive of the corpus rather than incidental to it, directly challenging the separability thesis. Indian constitutional jurisprudence attests the prescriptions' obsolescence as enforceable law. No single external source attests the reformist synthesis itself — the reading's claim that core and context come apart cleanly rests on its own interpretive act.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 is medium by design of this reading: the literal-enforcement regime's heavy extraction has been dismantled (the series opens at 0.82 under panchayat-enforced hierarchy), but the settlement re-consolidates a smaller extraction around interpretive rents and symbolic precedence — the series declines for a century and then ticks back up (0.54 to 0.58) as the reformist settlement institutionalizes and its administrators' mandate entrenches. Suppression 0.45 reflects a dismantled legal machinery with persistent social and identity pressure: exit is constrained, not sealed. Theater_ratio 0.46 is the signature drift of this arrangement — as enforcement gave way to gesture, a growing share of activity became performative (inclusion ceremonies, heritage framing, symbolic repudiations of caste alongside retained ceremonial precedence), while genuine teaching and coordination continue. Accessibility_collapse 0.38: alternatives are visible and real (secular law, conversion traditions, reform movements), so understanding the arrangement does not collapse alternatives; the tradition retains gravitational pull through identity rather than through sealed exits. Resistance 0.6: the settlement is pressed from both flanks — orthodox traditionalists resisting the demotion of eternal prescriptions, abolitionists resisting any residual authority — and from below by organized dalit mobilization, which supplies coalition capacity that a purely powerless victim set would lack. Temporal design note: one shared eight-point grid serves all three series; suppression_requirement is authored (rather than left to the static scalar) because the story specifically tracks enforcement-capacity decay — the falling trajectory models the dismantling of panchayat jurisdiction, ostracism enforcement, and ritual policing, flattening at a low plateau of social-pressure maintenance. The mild post-100 re-rise in base_extractiveness alongside monotonically rising theater_ratio is the arrangement's characteristic late-life shape: extraction stabilizing while its justification grows more gestural.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural asymmetry is deliberate. From the establishment's seat the arrangement is a rescue operation: it preserved a revered corpus through an age that had outlawed its social core, and its members experience the boundary-work as scholarship and pastoral care. From the upper-caste laity's seat it is a benign inheritance: nobody enforces anything anymore, and what remains reads as heritage. From the dalit communities' seat the same structure is a managed hierarchy — equality promised, paced, and conditioned on deference to the very interpreters who set the pace. From the courts' seat it is a recurring constitutional collision: each ruling shrinks the enforceable residue while leaving doctrinal authority formally intact. The identity-lock on the establishment seat is institutional fusion: lineage, vocation, and the corpus's continuing authority constitute the members' selves, so the settlement cannot be surrendered without dissolving the institution that embodies it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: reformist_theological_establishment (declared beneficiary, institutional power, identity_locked exit, and the receipt seat for deference and endowments) derives a strongly beneficiary-side d; upper_caste_devout_laity (declared beneficiary, powerful, identity_locked) likewise sits near the subsidized end; dalit_and_lower_caste_communities (declared victim, organized, constrained exit) derives a strongly target-side d, amplified by exit constraint. One override is declared: for the moderate-power atom, d is set to 0.48. The derivation reading hindu_devout_laity off the beneficiary declaration alone would understate their position — they are the story's genuinely dual seat, receiving real coordination (instruction, rites, moral vocabulary) while paying deference, funding institutions, and bearing obligations whose hierarchical traces fade slowly; 0.48 places them at near-symmetry, which is the honest structural read. Scope amplification applies modestly at continental scale: verification of what actually happens in villages and households is harder than what synods decree, so effective extraction scales up somewhat for the payer seat relative to its base rate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was to codify righteous conduct for a society explicitly ordered by birth-ranked stations: half of that mandate (answering how one should live) is plainly live; half (calibrating duty to inherited rank) is dead in law and contested in practice. Hence founding_problem_status: contested rather than dead — the parties genuinely dispute whether anything of the original charge survives or whether the arrangement now administers a mandate it has redefined. Because the mandate is reinterpreted rather than completed or abandoned, no sunset clause is declared and none would be honest: the reformist settlement presents itself as permanent stewardship, not transitional scaffolding. The mandatrophy machinery earns its keep here in both directions: it blocks the abolitionist temptation to read the entire arrangement as pure extraction by keeping the real coordination function (an ethical framework coordinating hundreds of millions across generations and a diaspora) visible in the classification; and it blocks the establishment's temptation to read the settlement as finished purification by surfacing the receipt surface — deference, offerings, and boundary-authority flowing continuously to the seat that controls the pace of reform. The mismatch consumer finds no zombie flag under the current authorship (contested status x world_rearranges verdict), but the core_context_boundary_elasticity omega is the tripwire: if the boundary proves administratively elastic and the founding problem resolves to dead, this story should be re-read as a managed perpetuation. On the cost side, fixing is prohibitive for whoever could fix it: the establishment would have to surrender the boundary-authority that constitutes it; the courts would face constitutional and political upheaval in disestablishing a tradition's internal authority outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the reformist_contextual reading of the dharmasastra_corpus kernel; what would the sibling readings (orthodox_literalist, abolitionist_rejection) change structurally if instantiated instead?',
    'Compile and compare the sibling stories: restoring eternal-prescription enforcement (orthodox_literalist) re-expands the victim set and drives epsilon toward its historical high; abolishing textual authority outright (abolitionist_rejection) removes every beneficiary seat and leaves no coordination defense.',
    'The orthodox instantiation yields a high-extraction enforcement regime with a maximal victim set; the abolitionist instantiation yields a pure extraction arrangement with no surviving coordination function. This story''s medium epsilon and reduced victim set are properties of the reformist settlement only, not of the label ''Dharmasastra authority''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one reading of dharmasastra_corpus among three, with structural deltas documented per sibling.').

omega_variable(
    separability_disagreement_location,
    'Where do the three readings locate their disagreement: in the epistemic status of the varna/jati prescriptions (eternal revealed truth versus historically conditioned custom), or additionally in whether any residual textual authority legitimately remains?',
    'Structural comparison of the three readings'' axiom sets and beneficiary/victim declarations: orthodox asserts eternal revelation and literal observance; reformist asserts conditioned texts carrying a separable ethical core; abolitionist asserts constitutive oppression with no legitimate authority remaining.',
    'If the dispute reduces to prescription status alone, reformist and orthodox differ chiefly on enforcement intensity; if residual authority is a second axis, the reformist reading sits strictly between its siblings and inherits opposition from both flanks, which the elevated resistance metric already reflects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_disagreement_location, conceptual, 'Locating the axis on which the kernel''s readings actually diverge.').

omega_variable(
    core_context_boundary_elasticity,
    'Is the boundary between ''enduring ethical core'' and ''time-bound prescription'' a stable textual-philological property, or is it drawn and redrawn by the interpretive class under social and political pressure?',
    'Track reclassification decisions across the interval: which practices migrated from ''eternal duty'' to ''historical artifact'', and whether migration correlates with external legal and political pressure (constitutional prohibition, legislation, mass movements) rather than with new philological evidence.',
    'If the boundary is elastic, the separability doctrine functions as discretionary interpretive authority: extraction concentrates in the power to redraw the boundary itself, and the arrangement drifts from hybrid coordination-plus-extraction toward pure extraction wearing hermeneutic garb. If stable, the measured medium extraction reflects genuine residual hierarchy rather than interpretive arbitrage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_context_boundary_elasticity, empirical, 'Whether the reformist settlement''s central distinction is principled or administered opportunistically.').

omega_variable(
    symbolic_hierarchy_material_reproduction,
    'Does the surviving symbolic caste hierarchy actively reproduce material disadvantage (marriage-market closure, occupational stigma, ritual gatekeeping), or has it become inert residue?',
    'Longitudinal socioeconomic data on caste-correlated outcomes conditional on continued religious participation; audit studies of matrimonial and ritual gatekeeping within reformist congregations.',
    'If materially reproductive, the payer seat remains a real victim set and the medium extractiveness is understated; if inert, the arrangement approaches coordination-with-residue and the payer burden is largely historical memory carried forward by identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_hierarchy_material_reproduction, empirical, 'Whether softened hierarchy still bites or merely memorializes.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the suppression that remains after legal emancipation structural (community sanction, matrimonial closure, an anti-conversion legal climate at the exit margin) or internalized (duty-and-fate framings absorbed by subordinated communities)?',
    'Post-exit suppression trajectory: compare converts and long-term secular emigrants with stayers on reported obligation, status anxiety, and continued deference patterns; persistence of suppression after the mechanism is removed indicates partial internalization.',
    'If substantially internalized, effective suppression exceeds the structural measure and travels with the payer after exit, slowing coalition formation and making measured suppression an underestimate of lived constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized remainder of the old enforcement order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 175).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dhar_tr_t25, dharmasastra_corpus__reformist_contextual, theater_ratio, 25, 0.14).
narrative_ontology:measurement(dhar_tr_t50, dharmasastra_corpus__reformist_contextual, theater_ratio, 50, 0.22).
narrative_ontology:measurement(dhar_tr_t75, dharmasastra_corpus__reformist_contextual, theater_ratio, 75, 0.29).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__reformist_contextual, theater_ratio, 100, 0.34).
narrative_ontology:measurement(dhar_tr_t125, dharmasastra_corpus__reformist_contextual, theater_ratio, 125, 0.4).
narrative_ontology:measurement(dhar_tr_t150, dharmasastra_corpus__reformist_contextual, theater_ratio, 150, 0.43).
narrative_ontology:measurement(dhar_tr_t175, dharmasastra_corpus__reformist_contextual, theater_ratio, 175, 0.46).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(dhar_be_t25, dharmasastra_corpus__reformist_contextual, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(dhar_be_t50, dharmasastra_corpus__reformist_contextual, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(dhar_be_t75, dharmasastra_corpus__reformist_contextual, base_extractiveness, 75, 0.62).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__reformist_contextual, base_extractiveness, 100, 0.57).
narrative_ontology:measurement(dhar_be_t125, dharmasastra_corpus__reformist_contextual, base_extractiveness, 125, 0.54).
narrative_ontology:measurement(dhar_be_t150, dharmasastra_corpus__reformist_contextual, base_extractiveness, 150, 0.56).
narrative_ontology:measurement(dhar_be_t175, dharmasastra_corpus__reformist_contextual, base_extractiveness, 175, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(dhar_su_t25, dharmasastra_corpus__reformist_contextual, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(dhar_su_t50, dharmasastra_corpus__reformist_contextual, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(dhar_su_t75, dharmasastra_corpus__reformist_contextual, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__reformist_contextual, suppression_requirement, 100, 0.51).
narrative_ontology:measurement(dhar_su_t125, dharmasastra_corpus__reformist_contextual, suppression_requirement, 125, 0.47).
narrative_ontology:measurement(dhar_su_t150, dharmasastra_corpus__reformist_contextual, suppression_requirement, 150, 0.45).
narrative_ontology:measurement(dhar_su_t175, dharmasastra_corpus__reformist_contextual, suppression_requirement, 175, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, abolitionist_rejection).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel dharmasastra_corpus. The colloquial label 'the authority of Dharmasastra' conflates three epsilon-distinct arrangements: orthodox_literalist (upstream historically — its eternal-prescription claims are what reformism responds to, with a maximal victim set under literal enforcement), reformist_contextual (this story — conditioned texts, separable core, medium extraction, reduced victim set), and abolitionist_rejection (downstream polemic target and rival — constitutive oppression, no beneficiaries, no coordination defense). Each member links the others via affects_constraints; epsilon is invariant within each file because each is a distinct arrangement, not one constraint viewed from angles. The upstream/downstream gradient runs opposite to legitimacy: the oldest claims carry the highest extraction, and each successive reading reduces the victim set while defending a shrinking residue of textual authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
