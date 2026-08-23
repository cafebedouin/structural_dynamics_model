% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing-Press Inevitability Causal Claim (Technological-Determinist Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism reading of the
 *   kernel press_reformation_causality: the claim that the printing press, as
 *   an autonomous enabling technology, made the spread of vernacular
 *   scripture and the success of the Reformation inevitable, with human
 *   actors — printers, reformers, princes, authorities — cast as downstream
 *   responders to a technological prime mover. The standing arrangement under
 *   contest, and the referent for every metric below, is that print-mediated
 *   causal sequence as this reading constitutes it; the reading's endorsed
 *   alternative is the sequence itself, not any rival arrangement. Per the
 *   epsilon-invariance principle, the colloquial label 'printing caused the
 *   Reformation' is decomposed into a three-story constraint family: this
 *   determinist reading, press_reformation_causality__strategic_deployment,
 *   and press_reformation_causality__co_constitution, all linked via
 *   network.affects_constraints. The claim/metric gap is deliberate and is
 *   the measurement this story exists to take: the reading CLAIMS mountain —
 *   an unchangeable, law-like media regularity needing no defender and
 *   collecting no rents — while the authored structural data declares the
 *   beneficiaries and payers the frame obscures (expected delta: 'beneficiary
 *   structure obscured'), so the false-summit signature and per-seat
 *   computation can register what the inevitability narrative conceals.
 *   Declaring beneficiaries on a mountain is intentional FSM authoring,
 *   documented by the omega naturality_vs_constructed_arrangement. KEY AGENTS
 *   (by structural relationship): - commercial_printers: Primary historical
 *   beneficiary (organized/mobile) — converts controversy demand into
 *   concentrated profit; the frame casts them as downstream conduits -
 *   protestant_reformers: Movement beneficiary (organized/identity_locked) —
 *   gains irreversible momentum the frame attributes to the technology -
 *   secularizing_princes: Beneficiary and agenda-setter
 *   (institutional/arbitrage) — converts print-accelerated realignment into
 *   confiscated property and territorial sovereignty -
 *   manuscript_scribes_and_scriptoria: Primary target (powerless/trapped) —
 *   bears the transition the frame renders inevitable -
 *   catholic_doctrinal_authorities: Target (institutional/constrained) —
 *   loses the interpretive monopoly; cast as doomed incumbent -
 *   whig_historians_and_tech_futurists: Framing beneficiary
 *   (moderate/identity_locked) — collects narrative authority from
 *   inevitability - illiterate_rural_parishioners: Excluded voice
 *   (powerless/trapped) — lives the confessional outcomes, absent from the
 *   archive - contemporary_revisionist_historians: Analytical observer
 *   (analytical/analytical) — tests the inevitability claim against the
 *   record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.34).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.44).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.34).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing-Press Inevitability Causal Claim (Technological-Determinist Reading)").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '8ca5023d-49eb-4cd7-9169-20da3ea1f0b1').
narrative_ontology:cs_kernel_codification('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', distributed).
narrative_ontology:cs_authority_grounding('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', expertise).
narrative_ontology:cs_interpretation_layer_present('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1').
narrative_ontology:cs_reading_relation('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', foundational, technology_autonomy_prime_mover).
narrative_ontology:cs_axiom_status(technology_autonomy_prime_mover, holdable).
narrative_ontology:cs_axiom_grounding('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', technology_autonomy_prime_mover, empirically_contingent).
narrative_ontology:cs_axiom('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', secondary, reformation_success_invariant_to_agency).
narrative_ontology:cs_axiom_status(reformation_success_invariant_to_agency, holdable).
narrative_ontology:cs_axiom_grounding('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', reformation_success_invariant_to_agency, empirically_contingent).
narrative_ontology:cs_reference_frame('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', autonomous_technology_prime_mover).
narrative_ontology:cs_drift_state('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', post_revisionist_print_history, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8ca5023d-49eb-4cd7-9169-20da3ea1f0b1', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, commercial_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, secularizing_princes).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, whig_historians_and_tech_futurists).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, manuscript_scribes_and_scriptoria).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, catholic_doctrinal_authorities).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_determinism_doctrine).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, media_autonomy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run print shops in centers such as Basel, Wittenberg, Strasbourg, and Frankfurt. Print whatever sells: Latin Bibles, Lutheran pamphlets, anti-papal caricature, Catholic rebuttal, indulgence-controversy broadsheets. Controversy multiplies demand and reprint cycles concentrate fortunes in a few houses. When a title loses heat they pivot genres or relocate; several houses moved cities or switched confessional allegiance to keep presses running. The frame casts them as passive conduits of the technology's logic.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, commercial_printers, beneficiary,
    organized, biographical, mobile, continental).

% Lead a movement whose pamphlets and scripture translations circulate faster than authorities can reply. Their self-understanding fuses vocation with providential success: the movement's spread is read as God's doing — and, in modern retellings, the technology's doing — rather than as the product of their own publishing strategy, patron management, and timing. Leaving the movement means abandoning the calling that constitutes them.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, protestant_reformers, beneficiary,
    organized, generational, identity_locked, continental).

% Territorial rulers who convert print-accelerated religious realignment into jurisdictional control: sponsor territorial churches, seize monastic property, appoint clergy, and legislate confession under the principle that the ruler's confession is the territory's. Gains are concrete — land, revenue, sovereignty over religious life. They can and do switch confessional alignments when advantageous.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, secularizing_princes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__technological_determinism, secularizing_princes, agenda_setter).

% Copyists, illuminators, and monastery workshops whose trade is reproducing texts by hand. Demand for their core product collapses within a generation in commercial centers; wages fall, workshops close, and the skill offers no bridge to machine-paced production. Many are monastics whose vocation and craft are the same thing, so losing the trade means losing the identity that ordered their lives.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, manuscript_scribes_and_scriptoria, payer,
    powerless, biographical, trapped, continental).

% Clergy, universities, and curial offices that previously controlled what was copied, expounded, and sold. Once the press decentralizes reproduction, they fight rearguard actions: index lists of prohibited books, licensing regimes, seminary reform, new preaching orders. They cannot abandon their office or their doctrine; their response capacity is bounded by the very institutions they are defending.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_doctrinal_authorities, payer,
    institutional, civilizational, constrained, continental).

% Historians, media theorists, and technology commentators who carry the inevitability narrative forward: survey textbooks, Gutenberg-celebration anniversaries, McLuhan-lineage media theory, and internet-era essays casting the web as the new printing press. The narrative supplies a ready-made template — technology as protagonist, institutions as scenery — that transfers to each new medium. Their professional identities are invested in the progressivist frame; revising it means revising their own canon.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, whig_historians_and_tech_futurists, beneficiary,
    moderate, generational, identity_locked, global).

% Peasant households who never read a pamphlet but live inside the outcomes: which catechism their children memorize, which images survive visitation, which liturgy their priest says. Confessionalization reaches them through visitations, schools, and pulpits. They appear in the record only as visitation responses and court cases, never as participants in the print-struggle story the causal claim tells.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, illiterate_rural_parishioners, excluded,
    powerless, biographical, trapped, regional).

% Scholars who test the inevitability claim against evidence: print-shop economics showing failure rates and pivots, regions where abundant printing produced no reformation, the persistence of scribal publication alongside print, and documentation of print's instability through errata and variant editions. They publish critiques and syntheses; their seat sits outside the confessional and celebratory economies the claim feeds.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, contemporary_revisionist_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__technological_determinism, whig_historians_and_tech_futurists).
narrative_ontology:fixing_cost_class(press_reformation_causality__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the material layer, the print economy solved the problem of reproducing and distributing identical doctrinal texts to dispersed audiences faster than opponents could respond; at the interpretive layer, the causal claim coordinates explanation by supplying a single sufficient cause for the Reformation's spread.
% TRANSFER_FUNCTION: Materially: controversy-driven profits to print houses; ecclesiastical land and revenue to secularizing princes; interpretive authority from clerical gatekeepers to vernacular readers and publishers. Interpretively: explanatory credit transferred from human agents to the technology itself.
% ABSENT_VOICES: Illiterate rural parishioners lived inside the confessional outcomes the print struggle produced but are absent from the literate archive the claim rests on; displaced scribes left no corporate testimony; Catholic voices enter the triumphal narrative only as doomed incumbents. They sit outside the book-fair ledgers, pamphlet corpora, and denominational histories that carry the claim — present in visitation records and court cases, absent from the causal story.
% DISAPPEARANCE_RATIONALE: If the inevitability claim vanished overnight, the arrangements built on it would rearrange: survey curricula would reorganize around contingency and mediation, media-theory canons would lose their founding template, and the internet-era analogy industry that licenses the claim forward to each new technology would lose its charter. The historical record itself would not change; what rearranges is the explanatory and celebratory economy the claim carries.
% FOUNDING_PROBLEM: Explaining why the Reformation succeeded where earlier vernacular dissent (Wycliffe, Hus) disseminating religious material in manuscript did not — and supplying progressive historiography with a mechanical, technology-first cause for religious change.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set on the question, contested on the answer: confessional and secular historians alike built research programs on explaining differential spread, so the founding problem is attested by the entire field including its critics. The inevitability answer is corroborated by no external source — regional-comparison work (print-rich Italy and Spain produced no durable reformation), bibliographic evidence of print's variance, scribal-publication studies, and commercial histories of print shops all attest against it from seats outside the framing's beneficiaries. Stated plainly: no one outside the beneficiary set attests inevitability itself.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored as descriptively true of the arrangement's actual operation, independently of the mountain claim. Extractiveness is modest (0.34) because the reading's own accounting concedes only transition costs — scribe displacement framed as progressive friction — while the declared beneficiary structure documents where gains actually pooled without the reading pricing them as extraction; the omega beneficiary_structure_visibility records that this modesty may itself be the obscuring the reading performs. Suppression (0.44) is the frame's rhetorical hold on alternative causal accounts; it is a raw structural property, unscaled by power or scope — only extractiveness is scaled by directionality and scope in the engine's computation. Theater_ratio (0.40) reflects maintenance by ritual assertion: anniversary celebrations, textbook boilerplate, and internet-era analogy essays repeating the claim against counterevidence the tradition declines to engage. Accessibility_collapse (0.78) is high because the frame, once accepted, closes contingency questions almost completely; resistance (0.45) is the revisionist program it meets. Coordination type information_standard is declared because the arrangement's primary coordination function is identical-text reproduction at scale — a standardization function with minimal inherent overhead (type floor 0.02, no override warranted).
 *   
 *   The temporal series run on one shared grid (1450/1520/1600/1700/1800/1900/2020) with every tracked metric authored at every point; trajectories are monotonic rather than cyclical — the frame hardened rather than oscillated, so no intermittent-reinforcement reading applies. suppression_requirement is authored deliberately: the story's traced dynamic is enforcement-capacity change, specifically the growth of rhetorical enforcement (Whig orthodoxy policing teleology, then active dismissal of revisionism) as counterevidence accumulated; a static picture would understate the ratchet. Endpoint values equal the base_properties scalars by construction of the grid (0.34 / 0.40 / 0.44).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the payer seats, the law-like sequence is experienced as dispossession without agency: scribes watch a trade die and are told the death was inevitable; doctrinal authorities fight rearguard actions inside institutions the frame has already written off as scenery. From the beneficiary seats, the same sequence is vindication: printers collect controversy rents, princes convert realignment into sovereignty, and the framing intelligentsia inherits a reusable template. From the reading's own analytic seat, the sequence is neither — it is natural process, which is precisely the perceptual gap the false-summit machinery is built to measure. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. commercial_printers: declared beneficiary with mobile, arbitrage-grade exit (genre pivots, relocation, confessional switching) — derivation places them nearest the beneficiary end. secularizing_princes: beneficiary with arbitrage exit (confessional realignment) — likewise near-beneficiary, with the agenda_setter secondary role marking that they also administer the territorial-church machinery the sequence produced. protestant_reformers: beneficiary, but identity_locked exit (vocation fused with the movement) pulls them slightly off the floor — subsidized, yet unable to arbitrage their position. whig_historians_and_tech_futurists: beneficiary of the framing itself, identity_locked into the progressivist canon — low d with no material exit. manuscript_scribes_and_scriptoria: declared victim, trapped (craft-specific skill, monastic vocation) — derivation places them near the full-target end. catholic_doctrinal_authorities: declared victim, institutional power but constrained exit (office and doctrine are not abandonable) — high d; institutional power damps but does not invert targeting. illiterate_rural_parishioners carry no declaration; their exclusion from the conversation is structural, and their exposure to confessional outcomes is noted for qualitative review rather than forced into the derivation. No directionality_overrides are authored: the derivation chain produces accurate d for every declared seat from beneficiary/victim data plus exit options, and the one genuinely ambiguous seat is better handled by the excluded-role treatment than by a fabricated override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining why the Reformation succeeded where Wycliffe's and Hus's movements, disseminating vernacular religious material in manuscript, did not — is live but contested: the question still organizes research programs, while the inevitability answer this reading supplies is the part under revisionist assault. Mandatrophy resolution matters in both directions here. Read naively, the mountain claim would immunize a beneficiary-laden arrangement as natural law — the false-summit signature exists precisely to catch that, and the declared beneficiaries (printers, princes, reformers, framing intellectuals) give it the evidence it needs. Read cynically, the same structure could be mislabeled pure extraction, erasing the arrangement's genuine coordination function: identical-text reproduction at scale really did solve a collective-action problem — doctrinal content reaching dispersed audiences faster than authorities could respond. The classification keeps both errors visible: coordination function credited, asymmetry priced, inevitability claim tested rather than assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_constructed_arrangement,
    'Is the print-driven causal sequence a law-like regularity of media technology (mountain), or a contingent historical configuration whose gains accrued to identifiable actors?',
    'Comparative analysis across print-access gradients: regions with equal or greater press density that produced no durable reformation (Italy, Spain), cases where print served Counter-Reformation mobilization equally well, and manuscript-persistence zones. If outcomes track political and commercial choices rather than press presence, the sequence is constructed.',
    'Resolution toward constructed dissolves the mountain claim, activates the declared beneficiary structure, and shifts classification toward hybrid coordination/extraction; confirmation of law-likeness would certify the mountain profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_constructed_arrangement, empirical, 'Whether the causal claim describes natural law or a constructed, beneficiary-bearing arrangement.').

omega_variable(
    beneficiary_structure_visibility,
    'Does the determinist frame obscure a real beneficiary structure — printer profits, princely confiscations, reformer momentum — that the sibling readings would surface?',
    'Archival reconstruction of print-house ledgers, confiscation inventories, and publisher-strategy correspondence; compare recorded gains against what the inevitability narrative credits to the technology itself.',
    'If the frame launders concentrated gains as technological necessity, effective extraction exceeds the reading''s own accounting and the false-summit signature strengthens; if gains were genuinely diffuse windfalls, the reading''s low-extraction profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_visibility, empirical, 'Whether the reading''s modest extraction estimate reflects reality or obscures concentrated gains.').

omega_variable(
    counterfactual_inevitability_test,
    'Was Reformation success invariant to human choices — Luther''s survival, Frederick''s protection, printer credit decisions, imperial politics — or contingent on them?',
    'Structured counterfactual analysis anchored in the Hussite precedent (widespread vernacular religious material in manuscript without durable reformation success) and in documented decision points of 1518-1521.',
    'Demonstrated sensitivity to choices invalidates the inevitability axiom and collapses the reading into the strategic-deployment or co-constitution framings; demonstrated insensitivity would substantiate the mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_inevitability_test, empirical, 'Test of the reading''s core invariance premise against counterfactual evidence.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (technological_determinism) of the kernel press_reformation_causality; what would the sibling readings (strategic_deployment, co_constitution) change structurally, and where is the disagreement located?',
    'Adopting a sibling reading reassigns causal credit: strategic_deployment moves printers and reformers from downstream responders to agenda-setting agents; co_constitution dissolves the technology/agency boundary into print-economy/controversy feedback loops. The disagreement is located in causal topology — autonomous sufficiency versus mediated strategy versus co-constitution — not in the historical record itself.',
    'Any sibling adoption dissolves the mountain claim and the downstream-responder role assignment; the beneficiary structure becomes foreground rather than obscured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega recording kernel membership, reading identity, sibling deltas, and disagreement location.').

omega_variable(
    determinist_frame_internalization,
    'Is the frame''s hold on historiographical and popular discourse structural (canon formation, curricula, publishing pipelines) or internalized (trained intuitions that treat technology-first explanation as the default)?',
    'Post-canonical trajectory: whether newer cohorts trained on revisionist syntheses revert to determinist defaults when writing for popular audiences. If the reflex persists after canonical pressure is removed, it is internalized.',
    'An internalized hold raises effective suppression above the structural measure — the frame travels with its carriers after the canon revises; a purely structural hold would decay with curricular change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinist_frame_internalization, conceptual, 'Structural versus internalized maintenance of the inevitability frame.').

omega_variable(
    print_fixity_assumption,
    'Does the causal claim''s mechanism depend on print''s standardizing fixity — and is that fixity real, given documented errata, variant editions, and unauthorized abridgments?',
    'Bibliographic evidence: collation of surviving editions for variance rates across the early modern corpus.',
    'Low fixity undermines the autonomy mechanism itself — the press did not deliver what the claim says it delivered — weakening both the mountain claim and the coordination-function attribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(print_fixity_assumption, empirical, 'Whether print''s standardization, the claim''s causal mechanism, was as stable as the frame assumes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_det_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.1).
narrative_ontology:measurement_basis(press_det_tr_t1450, observed).
narrative_ontology:measurement(press_det_tr_t1520, press_reformation_causality__technological_determinism, theater_ratio, 1520, 0.14).
narrative_ontology:measurement_basis(press_det_tr_t1520, observed).
narrative_ontology:measurement(press_det_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.18).
narrative_ontology:measurement_basis(press_det_tr_t1600, observed).
narrative_ontology:measurement(press_det_tr_t1700, press_reformation_causality__technological_determinism, theater_ratio, 1700, 0.22).
narrative_ontology:measurement_basis(press_det_tr_t1700, observed).
narrative_ontology:measurement(press_det_tr_t1800, press_reformation_causality__technological_determinism, theater_ratio, 1800, 0.3).
narrative_ontology:measurement_basis(press_det_tr_t1800, observed).
narrative_ontology:measurement(press_det_tr_t1900, press_reformation_causality__technological_determinism, theater_ratio, 1900, 0.36).
narrative_ontology:measurement_basis(press_det_tr_t1900, observed).
narrative_ontology:measurement(press_det_tr_t2020, press_reformation_causality__technological_determinism, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(press_det_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(press_det_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.18).
narrative_ontology:measurement_basis(press_det_be_t1450, observed).
narrative_ontology:measurement(press_det_be_t1520, press_reformation_causality__technological_determinism, base_extractiveness, 1520, 0.26).
narrative_ontology:measurement_basis(press_det_be_t1520, observed).
narrative_ontology:measurement(press_det_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.28).
narrative_ontology:measurement_basis(press_det_be_t1600, observed).
narrative_ontology:measurement(press_det_be_t1700, press_reformation_causality__technological_determinism, base_extractiveness, 1700, 0.27).
narrative_ontology:measurement_basis(press_det_be_t1700, observed).
narrative_ontology:measurement(press_det_be_t1800, press_reformation_causality__technological_determinism, base_extractiveness, 1800, 0.31).
narrative_ontology:measurement_basis(press_det_be_t1800, observed).
narrative_ontology:measurement(press_det_be_t1900, press_reformation_causality__technological_determinism, base_extractiveness, 1900, 0.33).
narrative_ontology:measurement_basis(press_det_be_t1900, observed).
narrative_ontology:measurement(press_det_be_t2020, press_reformation_causality__technological_determinism, base_extractiveness, 2020, 0.34).
narrative_ontology:measurement_basis(press_det_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(press_det_su_t1450, press_reformation_causality__technological_determinism, suppression_requirement, 1450, 0.15).
narrative_ontology:measurement_basis(press_det_su_t1450, observed).
narrative_ontology:measurement(press_det_su_t1520, press_reformation_causality__technological_determinism, suppression_requirement, 1520, 0.2).
narrative_ontology:measurement_basis(press_det_su_t1520, observed).
narrative_ontology:measurement(press_det_su_t1600, press_reformation_causality__technological_determinism, suppression_requirement, 1600, 0.24).
narrative_ontology:measurement_basis(press_det_su_t1600, observed).
narrative_ontology:measurement(press_det_su_t1700, press_reformation_causality__technological_determinism, suppression_requirement, 1700, 0.26).
narrative_ontology:measurement_basis(press_det_su_t1700, observed).
narrative_ontology:measurement(press_det_su_t1800, press_reformation_causality__technological_determinism, suppression_requirement, 1800, 0.34).
narrative_ontology:measurement_basis(press_det_su_t1800, observed).
narrative_ontology:measurement(press_det_su_t1900, press_reformation_causality__technological_determinism, suppression_requirement, 1900, 0.38).
narrative_ontology:measurement_basis(press_det_su_t1900, observed).
narrative_ontology:measurement(press_det_su_t2020, press_reformation_causality__technological_determinism, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement_basis(press_det_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'printing caused the Reformation' decomposes per the epsilon-invariance principle into three structurally distinct causal claims with different epsilon values and beneficiary structures. This story is the technological_determinism member (technology as autonomous sufficient cause; mountain claim; beneficiary structure obscured). press_reformation_causality__strategic_deployment (actors weaponizing the press toward religious and commercial goals) and press_reformation_causality__co_constitution (technology and agency co-constituted through print-economy/controversy feedback loops) are siblings. Influence runs upstream-to-downstream: the determinist narrative dominated first and remains the popular citation environment the corrective readings argue against; each sibling story links back to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
