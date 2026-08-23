% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Allegorical Ancient Near Eastern Reading of Genesis 1-2
 *   domain: religious studies / biblical hermeneutics / science-religion interface
 *
 * SUMMARY:
 *   This story authors the allegorical_ancient_near_east reading of the
 *   Genesis 1-2 creation narrative: the interpretive settlement under which
 *   the text is read as ancient Near Eastern mythopoetic literature making no
 *   historical-scientific claims, with complete decoupling from cosmology and
 *   biology and with the dominion language demoted from normative mandate to
 *   ancient rhetorical furniture. The constraint is the hermeneutical norm
 *   itself as it operates in mainline denominations, seminaries, and the
 *   academic biblical-studies guild: it coordinates faith communities and
 *   scientific institutions around a shared genre classification, and it is
 *   actively enforced — through credentialing, peer review, and doctrinal
 *   discipline — against readings it classifies as category errors. The
 *   epsilon referent is this allegorical-ANE interpretive arrangement itself,
 *   assessed by the reading's own lights; it is not the literal arrangement
 *   the reading contests, and it is not the reading's endorsed alternative.
 *   KEY AGENTS (by structural relationship): mainline_denominations —
 *   agenda-setter and collector (institutional/constrained) — administers the
 *   settlement through teaching offices and curricula;
 *   academic_biblical_scholars — beneficiary and co-agenda-setter
 *   (organized/mobile) — produces the philological grounding and staffs the
 *   enforcement surface; young_earth_creationists — primary payer
 *   (organized/identity_locked) — bears delegitimation of their reading;
 *   traditionalist_dominion_theologians — secondary payer
 *   (moderate/identity_locked) — bear the loss of the dominion mandate's
 *   normative force; science_friendly_believers — beneficiary
 *   (moderate/constrained); scientific_institutions — pure beneficiary
 *   (institutional/arbitrage); secular_text_critics — excluded voice
 *   (moderate/mobile); comparative_religion_historians — analytical observer
 *   (analytical/analytical).
 *
 * KEY AGENTS:
 *   - mainline_denominations: agenda-setter and beneficiary (institutional/constrained/generational/global) — runs the seminaries, lectionaries, and doctrinal machinery that teach and police the reading; absorbs donor flight and internal division when the settlement is challenged; exit would mean dismantling a century of institutional adaptation
 *   - academic_biblical_scholars: beneficiary and co-agenda-setter (organized/mobile/biographical/global) — produce the comparative and philological work the reading stands on; careers and journals ride on the critical paradigm; mobile between confessional and secular institutions
 *   - young_earth_creationists: primary payer (organized/identity_locked/generational/national) — communities whose reading of the text as factual chronicle structures schools, media networks, and self-understanding; under the settlement their reading is classified as genre error rather than engaged as a rival; they respond by building parallel institutions rather than exiting
 *   - traditionalist_dominion_theologians: secondary payer (moderate/identity_locked/biographical/national) — teachers and writers for whom the dominion mandate carries direct ethical weight; the settlement recasts that language as ancient royal ideology without prescriptive force; their objection is heard as residue, not as a competing claim
 *   - science_friendly_believers: beneficiary (moderate/constrained/biographical/national) — scientifically liter laity and professionals who can remain in their traditions because the text no longer competes with their fields; they populate and fund the mainline pews
 *   - scientific_institutions: pure beneficiary (institutional/arbitrage/generational/global) — universities, academies, museums, and science-education bodies operating free of scriptural adjudication over geology, cosmology, and biology; abundant costless alternatives
 *   - secular_text_critics: excluded voice (moderate/mobile/biographical/global) — argue the settlement launders the text's cultural authority by stripping its claims while retaining its prestige; publish outside the confessional-academic conversation and hold no seat in the communities the reading governs
 *   - comparative_religion_historians: analytical observer (analytical/analytical/civilizational/global) — Assyriologists and historians of religion who attest the genre question from outside every benefiting party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.35).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.52).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.35).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Allegorical Ancient Near Eastern Reading of Genesis 1-2").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious studies / biblical hermeneutics / science-religion interface").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, 'c2a50f44-572a-43d6-ad5b-bec4aaf79738').
narrative_ontology:cs_kernel_codification('c2a50f44-572a-43d6-ad5b-bec4aaf79738', fixed_text).
narrative_ontology:cs_authority_grounding('c2a50f44-572a-43d6-ad5b-bec4aaf79738', expertise).
narrative_ontology:cs_interpretation_layer_present('c2a50f44-572a-43d6-ad5b-bec4aaf79738').
narrative_ontology:cs_reading_relation('c2a50f44-572a-43d6-ad5b-bec4aaf79738', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('c2a50f44-572a-43d6-ad5b-bec4aaf79738', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('c2a50f44-572a-43d6-ad5b-bec4aaf79738', foundational, text_makes_no_historical_scientific_claims).
narrative_ontology:cs_axiom_status(text_makes_no_historical_scientific_claims, holdable).
narrative_ontology:cs_axiom_grounding('c2a50f44-572a-43d6-ad5b-bec4aaf79738', text_makes_no_historical_scientific_claims, empirically_contingent).
narrative_ontology:cs_axiom('c2a50f44-572a-43d6-ad5b-bec4aaf79738', secondary, dominion_metaphor_carries_no_normative_force).
narrative_ontology:cs_axiom_status(dominion_metaphor_carries_no_normative_force, holdable).
narrative_ontology:cs_axiom_grounding('c2a50f44-572a-43d6-ad5b-bec4aaf79738', dominion_metaphor_carries_no_normative_force, deontological).
narrative_ontology:cs_reference_frame('c2a50f44-572a-43d6-ad5b-bec4aaf79738', ane_mythopoetic_genre_frame).
narrative_ontology:cs_drift_state('c2a50f44-572a-43d6-ad5b-bec4aaf79738', contemporary_critical_consensus, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c2a50f44-572a-43d6-ad5b-bec4aaf79738', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, mainline_denominations).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_friendly_believers).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, scientific_institutions).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_creationists).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, traditionalist_dominion_theologians).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, ane_mythopoetic_genre_classification).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, divine_accommodation_hermeneutics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the seminaries, lectionaries, hymnals, and doctrinal statements that teach Genesis 1-2 as ancient Near Eastern literature, and run the clergy-credentialing and doctrinal-discipline machinery that keeps the teaching uniform. They collect institutional legitimacy and educated membership from the settlement, and they absorb donor flight, congregational secessions, and internal division whenever the settlement is publicly challenged. Leaving the settlement would mean unwinding a century of curricular and institutional adaptation and reopening the schisms the settlement closed.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, mainline_denominations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, mainline_denominations, beneficiary).

% Produce the comparative philology, ANE parallel studies, and genre analysis on which the reading stands, and staff the graduate programs, journals, and review processes through which interpretive work is admitted or rejected. Careers, grants, and publication venues depend on the critical paradigm's standing. They can move between confessional and secular institutions, which makes their position unusually mobile for participants in a confessional dispute.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_scholars, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_scholars, agenda_setter).

% Communities whose reading of the text as a factual chronicle organizes their schools, media networks, museums, and self-understanding across generations. Under the settlement their reading is classified as a genre mistake rather than engaged as a rival interpretation, and admission to accredited institutions and academic publication effectively requires abandoning it. Leaving the literal framework would cost them their community, their institutions, and much of their interpretive world, so they pay the standing costs instead and build parallel structures alongside the settlement.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_creationists, payer,
    organized, generational, identity_locked, national).

% Teachers, pastors, and writers for whom the dominion mandate of Genesis 1:26-28 carries direct ethical weight as a statement of human vocation toward the created order. The settlement recasts that language as ancient Near Eastern royal ideology without prescriptive force, which removes the foundation beneath bodies of teaching they have built careers and identities on. Their objection is recorded as residue of an outgrown reading rather than seated as a competing claim, and their professional standing inside the governing communities depends on not pressing it.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, traditionalist_dominion_theologians, payer,
    moderate, biographical, identity_locked, national).

% Scientifically literate laity and working scientists who remain inside their traditions because the text no longer competes with their professional knowledge. They populate the pews, fund the institutions, and supply the settlement's constituency of proof that faith and science are compatible. Their realistic alternative — joining literalist communities — carries social and intellectual costs most of them will not pay, so their exit is constrained even though their satisfaction with the settlement is genuine.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_friendly_believers, beneficiary,
    moderate, biographical, constrained, national).

% Research universities, scientific academies, museums, and science-education bodies that operate entirely free of scriptural adjudication over cosmology, geology, and biology. They engage the text only as a cultural artifact and bear none of the settlement's costs; their alternatives are abundant and costless, since their authority never depended on the text in the first place. The settlement's decoupling dividend flows to them without reciprocal obligation.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, scientific_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Philosophers and secular critics who argue that the settlement launders the text's cultural authority — stripping its factual claims while retaining its prestige in law, politics, and public rhetoric. They publish outside the confessional-academic conversation and hold no seat in the communities whose practice the settlement governs, so their objection circulates in adjacent discourse without entering the interpretive process it criticizes.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, secular_text_critics, excluded,
    moderate, biographical, mobile, global).

% Assyriologists, Egyptologists, and historians of religion who track the genre question across traditions and centuries — the Enuma Elish and Atrahisis parallels, compositional dating, comparative cosmogony. They take no side in the confessional dispute, collect nothing from the settlement, and attest the evidentiary basis of the genre classification from a seat outside every benefiting party.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, comparative_religion_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, mainline_denominations).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared genre classification for Genesis 1-2 — ancient Near Eastern mythopoetic literature — so that confessional communities, the academic guild, and scientific institutions each operate without the text adjudicating another domain's claims. The classification is produced once, centrally, by the scholarly community instead of being renegotiated in every classroom, pulpit, curriculum committee, and school-board fight.
% TRANSFER_FUNCTION: Moves interpretive authority over origins questions away from the text's literalist custodians and toward the critical-mainline establishment, while ceding empirical adjudication over cosmology, geology, and biology entirely to scientific institutions. The costs of the settlement move onto the constituencies whose readings lose standing: young-earth communities bear delegitimation, and dominion-traditionalists bear the loss of a doctrine's normative force.
% ABSENT_VOICES: Young-earth creationists appear in the settlement's own proceedings only as objects — their strongest counterclaim, that the reading contradicts the text's self-presentation and the historic concordist tradition of the community, is recorded as data about them rather than seated as a rival reading with standing. Secular critics who would dissolve the text's residual cultural authority altogether likewise hold no seat in the communities the settlement governs. Both voices object from outside the rooms where the reading is administered.
% DISAPPEARANCE_RATIONALE: If the allegorical-ANE settlement vanished overnight, mainline seminaries and curricula would face the pre-Darwinian crisis again, the academic field would lose its organizing genre paradigm, and literalist and critical constituencies would renegotiate the text's authority congregation by congregation and journal by journal. The existing division of labor among pulpit, seminar room, and laboratory — and the school-system truces built on it — would reorganize around whatever settlement emerged next.
% FOUNDING_PROBLEM: The collision, sharpened by Enlightenment historical criticism and Darwinian biology, between a text long read as factual cosmogony and the emerging scientific account of origins — a collision that threatened both the credibility of confessional communities before their educated members and civic peace over public education.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science document the nineteenth-century collision and the settlement's reconciling function from outside the confessional parties; Assyriologists and comparative philologists attest the genre classification on textual evidence independent of any benefiting community; and the century-long persistence of organized creationist counter-institutions attests, from the paying side, that the founding problem remains live for a large population. No party outside the benefiting set attests that the problem is dead.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).
:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.35: real but bounded. The settlement imposes genuine costs — literalist constituencies have their reading reclassified as error, and dominion-traditionalists lose a doctrine's normative force — but most governed participants are net beneficiaries, and the heaviest historical costs fell during the settlement's insurgent phase, not its consolidated phase. Suppression is 0.52 and is a raw structural property, unscaled by power or scope: enforcement is jurisdictionally dense (heresy trials in the 1860s-80s, the fundamentalist-modernist purges of the 1920s, then normalized credentialing and peer-review gatekeeping) and jurisdictionally absent outside mainline and academic territory, where creationist parallel institutions operate freely. Theater ratio is 0.28 and rising slowly: the genre scholarship is real, but an increasing share of settlement activity is ritualized affirmation of 'faith-science harmony' that papers over live disputes. Accessibility collapse is low (0.35) because alternatives remain fully available in parallel institutions — the settlement collapses alternatives only within its own jurisdictions. Resistance is high (0.6): an organized, well-funded counter-movement has persisted for a century, which is precisely why suppression stays jurisdictional rather than total — the coalition potential of the payers was realized, not suppressed. The measurement series run on one shared eight-point grid (all three metrics at every point). The enforcement trajectory is rise-then-plateau tied to controversy cycles (Essays and Reviews, the modernist controversies), not intermittent reinforcement; the oscillation is a side effect of external controversy waves, not itself the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From the mainline-denominational and scholarly seats, the settlement is coordination they built and maintain: a settled genre framework that ended a century of crisis and lets pulpit, seminar room, and laboratory divide labor peacefully. From the identity-locked literalist seat, the same settlement operates as enforced exclusion — their reading is not refuted but ruled out of category, and the enforcement surface (credentialing, review, doctrinal discipline) is experienced as suppression of a faithful reading. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Scientific institutions sit nearest the beneficiary pole: they receive the entire decoupling dividend with arbitrage-grade exit and bear nothing. Academic_biblical_scholars and mainline_denominations sit low on the target axis — they collect disciplinary and institutional legitimacy — though the denominations also absorb real coordination costs (donor flight, internal division), which tempers but does not reverse their beneficiary position. Science_friendly_believers are low-to-moderate: genuine benefit, diffuse indirect costs. Young_earth_creationists and traditionalist_dominion_theologians sit near the full-target end, and their identity_locked exit amplifies effective extraction: leaving the literal framework would cost them their community and much of their interpretive world, so they pay rather than exit. Comparative_religion_historians hold the analytical seat with no directional stake. Global scope of the settlement modestly amplifies effective extraction for targets by making verification of compliance harder and exit across jurisdictions costlier.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. From its own seat the reading presents as pure liberation — a rope that merely removed a false burden — and a naive reading of its self-description would certify exactly that; the declared victims and the enforcement record prevent the mislabel. From the literalist seat the same settlement is described as pure suppression — a snare that silences faithful reading — and the genuine, load-bearing coordination function (a shared genre framework that resolved a real collective-action problem between confessional and scientific institutions) prevents that mislabel. The hybrid structure — real coordination and real asymmetric extraction through the same enforced norm — is what the tangled_rope claim records. No mandatrophy resolution is declared: the founding problem (the science-faith collision) remains live for a large population, the settlement's function is not atrophied, and the R5 status-by-verdict pair (live, world_rearranges) raises no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading (allegorical_ancient_near_east) of the genesis_creation_narrative kernel; the sibling readings (literal_young_earth, theistic_evolutionary) instantiate structurally different constraints with different epsilon values and different victim sets. Does any cross-reading comparison improperly average over readings?',
    'Each reading is authored as its own constraint story with its own epsilon, beneficiaries, and victims; the engine computes per-reading classifications and comparisons proceed story-to-story, never within a blended constraint.',
    'If readings were merged, the resulting constraint would have no stable epsilon: the literal reading assigns the text adjudicative authority over cosmology and biology (high extraction from scientific practice and from believers'' public credibility), while this reading assigns none. Classification is indexical to the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: classification is valid per-reading, not per-kernel.').

omega_variable(
    genre_classification_evidential_status,
    'How secure is the ancient Near Eastern mythopoetic genre classification itself — the parallel material (Enuma Elish, Atrahisis, cosmic-temple composition) on which this reading''s entire coordination function rests?',
    'Ongoing Assyriological publication: new tablet finds, refined dating of compositional layers, and comparative genre analysis either reinforce or complicate the classification.',
    'If the genre classification were substantially weakened, this reading''s coordination function erodes, its enforcement loses justification, and the interpretive field shifts back toward the sibling readings'' terms — raising measured extraction as constituencies renegotiate the text''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_classification_evidential_status, empirical, 'Empirical security of the ANE genre classification underpinning the reading.').

omega_variable(
    dominion_normativity_residual,
    'Does the dominion language of Genesis 1:26-28 lose ALL normative force under this reading, or does it retain theological-ethical (non-scientific) force as stewardship framing?',
    'Track eco-theological and denominational ethics literature: if communities governed by this reading continue deriving ecological obligations from the dominion verses, the metaphor retains normative force in practice regardless of the reading''s official decoupling.',
    'If normative force is fully lost, the victim set includes traditionalist_dominion_theologians at full weight; if stewardship force survives, part of the claimed loss is not extraction but ordinary theological development, and epsilon drops slightly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominion_normativity_residual, conceptual, 'Whether the expected structural delta (dominion loses normative force) is complete or partial.').

omega_variable(
    jurisdictional_suppression_ambiguity,
    'Enforcement of this reading is jurisdictional — dense inside mainline denominations and the academic guild (credentialing, peer review, doctrinal discipline), absent outside them, where literalist parallel institutions flourish. Does the single suppression scalar misstate the structure?',
    'Seat-resolved analysis: compute suppression separately for agents inside versus outside the enforcing jurisdictions; compare against the scalar.',
    'For identity-locked payers inside the jurisdiction, effective suppression exceeds the scalar; for the organized creationist counter-institutions outside it, the scalar overstates their constraint. Per-seat classification diverges from the story-level number.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_suppression_ambiguity, empirical, 'Jurisdictional structure of enforcement versus the scalar suppression measure.').

omega_variable(
    residual_authority_disclaimer_completeness,
    'Secular critics charge that this reading strips the text of adjudicative claims while quietly retaining its cultural and liturgical authority — borrowing prestige the reading officially disclaims grounding for. Is the decoupling complete?',
    'Examine whether communities governed by this reading still cite the text in ways that carry evidential weight beyond the literary-theological register (public reasoning, bioethics, education policy).',
    'If the disclaimer is incomplete, a hidden extraction channel persists — the reading collects authority while denying the obligations that authority would carry — pushing the classification toward the snare end of the hybrid range.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_authority_disclaimer_completeness, conceptual, 'Completeness of the decoupling between retained authority and disclaimed adjudication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 1859, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genesis_ane_allegorical_tr_t1859, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1859, 0.1).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t1880, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1880, 0.14).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t1900, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1900, 0.17).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t1925, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1925, 0.2).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t1950, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t1975, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1975, 0.24).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t2000, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t2025, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(genesis_ane_allegorical_be_t1859, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1859, 0.42).
narrative_ontology:measurement(genesis_ane_allegorical_be_t1880, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1880, 0.46).
narrative_ontology:measurement(genesis_ane_allegorical_be_t1900, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1900, 0.44).
narrative_ontology:measurement(genesis_ane_allegorical_be_t1925, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1925, 0.4).
narrative_ontology:measurement(genesis_ane_allegorical_be_t1950, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1950, 0.36).
narrative_ontology:measurement(genesis_ane_allegorical_be_t1975, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1975, 0.34).
narrative_ontology:measurement(genesis_ane_allegorical_be_t2000, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(genesis_ane_allegorical_be_t2025, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(genesis_ane_allegorical_su_t1859, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1859, 0.3).
narrative_ontology:measurement(genesis_ane_allegorical_su_t1880, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1880, 0.48).
narrative_ontology:measurement(genesis_ane_allegorical_su_t1900, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1900, 0.46).
narrative_ontology:measurement(genesis_ane_allegorical_su_t1925, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1925, 0.53).
narrative_ontology:measurement(genesis_ane_allegorical_su_t1950, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1950, 0.51).
narrative_ontology:measurement(genesis_ane_allegorical_su_t1975, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1975, 0.48).
narrative_ontology:measurement(genesis_ane_allegorical_su_t2000, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2000, 0.49).
narrative_ontology:measurement(genesis_ane_allegorical_su_t2025, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% The colloquial label 'what Genesis 1-2 is' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per reading of the genesis_creation_narrative kernel — because measuring the text's authority through different hermeneutical observables yields different, stable epsilon values: the literal_young_earth reading assigns the text adjudicative authority over cosmology and biology (high extraction, imposed epistemic costs, collision with scientific institutions); this allegorical_ancient_near_east reading assigns none (bounded extraction, jurisdictional enforcement); theistic_evolutionary sits between (partial concordist authority retained). Each is authored as its own story with its own beneficiaries, victims, and claimed type; this file links to both siblings via affects_constraints. The upstream/downstream structure runs from this reading toward theistic_evolutionary: the genre classification consolidated here is increasingly cited as the ground on which TE compatibilist readings stand, so this reading's evidentiary condition shapes the sibling's legitimacy environment without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
