% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Textualist Paradox Reading of the Equality Clause — Performative Contradiction Exposure
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The kernel is the equality clause of the American Declaration of
 *   Independence — 'all men are created equal' — a fixed text that three
 *   readings instantiate as three different constraints. This story authors
 *   the textualist_paradox_reading: the constraint that arises when the
 *   text's universal language is held irreconcilable with its restricted
 *   historical application, such that asserting the text's authority while
 *   bounding its scope is a performative contradiction. The standing
 *   arrangement under contest — and the referent of epsilon — is the
 *   operative interpretive economy in which founder-intent-bounded scope is
 *   asserted, defended, and challenged: by this reading's own lights, that
 *   economy charges originalist authority claims a standing coherence cost
 *   and routes the harvested legitimacy to challengers who invoke the words
 *   against the word-users' practice. The colloquial label 'all men are
 *   created equal' decomposes into three structurally distinct constraints
 *   (originalist, textualist-paradox, universalist); this file authors only
 *   the paradox reading, linked to its siblings through
 *   network.affects_constraints. Claimed type and metrics are authored
 *   independently: the constraint is claimed as tangled_rope — genuine
 *   coordination function plus asymmetric extraction through the same
 *   structure — while the metrics describe its observed operation without
 *   being tuned to any computed verdict.
 *
 * KEY AGENTS:
 *   - - originalist_interpreters: Primary target (institutional/identity_locked) — jurists and scholars whose authority claims are debited by each deployment of the paradox
 *   - - originalist_interpretive_framework: Non-agent bearer — the doctrine-structure that absorbs delegitimation; its costs land on its human adherents
 *   - - expansionist_reform_movements: Primary beneficiary (organized/constrained) — reform coalitions unified by the text-grounded critical standard
 *   - - excluded_groups_claiming_inclusion: Beneficiary (powerless/trapped) — groups outside the bounded scope who wield the universal words as their principal internal lever
 *   - - critical_constitutional_scholars: Beneficiary and receipt seat (moderate/arbitrage) — the interpreters who bank attention and standing from each deployment
 *   - - legal_positivist_skeptics: Excluded voice (moderate/analytical) — deny the text's normative force; kept out of a contest that presupposes it
 *   - - constitutional_courts: Observer (institutional/analytical) — register the debate as background theory and decline to let the Declaration do operative work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.55).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.3).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist Paradox Reading of the Equality Clause — Performative Contradiction Exposure").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '0bb51862-b5c8-4075-8435-5c07fc177b35').
narrative_ontology:cs_kernel_codification('0bb51862-b5c8-4075-8435-5c07fc177b35', fixed_text).
narrative_ontology:cs_authority_grounding('0bb51862-b5c8-4075-8435-5c07fc177b35', diffuse_epistemic).
narrative_ontology:cs_reading_relation('0bb51862-b5c8-4075-8435-5c07fc177b35', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0bb51862-b5c8-4075-8435-5c07fc177b35', all_men_created_equal__universalist_reading, influences).
narrative_ontology:cs_axiom('0bb51862-b5c8-4075-8435-5c07fc177b35', foundational, restricted_application_performatively_self_refuting).
narrative_ontology:cs_axiom_status(restricted_application_performatively_self_refuting, holdable).
narrative_ontology:cs_axiom_grounding('0bb51862-b5c8-4075-8435-5c07fc177b35', restricted_application_performatively_self_refuting, deontological).
narrative_ontology:cs_axiom('0bb51862-b5c8-4075-8435-5c07fc177b35', secondary, textual_fidelity_trumps_founder_practice).
narrative_ontology:cs_axiom_status(textual_fidelity_trumps_founder_practice, holdable).
narrative_ontology:cs_axiom_grounding('0bb51862-b5c8-4075-8435-5c07fc177b35', textual_fidelity_trumps_founder_practice, deontological).
narrative_ontology:cs_reference_frame('0bb51862-b5c8-4075-8435-5c07fc177b35', performative_universal_commitment).
narrative_ontology:cs_drift_state('0bb51862-b5c8-4075-8435-5c07fc177b35', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0bb51862-b5c8-4075-8435-5c07fc177b35', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, expansionist_reform_movements).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, excluded_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, critical_constitutional_scholars).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpreters).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, performative_contradiction_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, textual_universality_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jurists, judges, and academics who hold that the equality clause's reach is fixed by the founding generation's understanding. Each prominent invocation of the text's universality against restricted practice forces them to spend scholarly and rhetorical capital defending the coherence of intent-bounded scope; their standing in their interpretive community rests on the lineage they defend, so abandoning that defense would cost them the authority the lineage confers.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpreters, payer,
    institutional, generational, identity_locked, national).

% The doctrine-structure holding that the Declaration's equality language operates within an eighteenth-century social taxonomy rather than as open-ended universal command. It absorbs delegitimation whenever the universal words are juxtaposed with the founders' restricted practice; being a doctrine rather than a person, it collects nothing and decides nothing, and its burdens land entirely on its human adherents.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework).

% Organized campaigns — abolitionist societies, suffrage organizations, civil-rights coalitions — pressing for expanded inclusion. The text-grounded critical channel gives them a shared standard that lets them attack restricted application while professing fidelity to the founding document, protecting them from dismissal as enemies of the founding. Leaving the channel would mean falling back on purely moral or foreign-theoretical grounds that opponents can paint as alien.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, expansionist_reform_movements, beneficiary,
    organized, generational, constrained, national).

% People outside whatever scope the bounded reading draws — enslaved persons in the antebellum period, disenfranchised groups since — who invoke the words 'all men are created equal' to claim membership. The universal language is often the only lever available to them inside a legal order they cannot leave; they are governed by the text whether or not the text is read to include them.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, excluded_groups_claiming_inclusion, beneficiary,
    powerless, biographical, trapped, national).

% Academics, public intellectuals, and writers who articulate the contradiction between the text's universality and its restricted application. Publications, lectures, and editorial platforms built on the contradiction confer professional standing and attention; each anniversary or news cycle reviving the hypocrisy theme renews demand for their work. They could pivot to other scholarly puzzles at real but survivable career cost.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, critical_constitutional_scholars, beneficiary,
    moderate, biographical, arbitrage, national).

% Jurisprudential thinkers who hold that the Declaration, whatever its rhetorical force, is not law and binds no one. The entire contest between the paradox reading and its originalist target presupposes that the text carries normative authority worth fighting over, so the positivist objection — that there is nothing to contradict because nothing binds — never gets a seat at the table.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, legal_positivist_skeptics, excluded,
    moderate, biographical, analytical, national).

% Courts adjudicating disputes that touch the equality clause's reach. They register the contradiction debate as background political theory and have consistently declined to let the Declaration's universal language do direct operative work, treating the Constitution's provisions as the enforceable locus of equality.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, critical_constitutional_scholars).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single text-grounded standard that lets dispersed challenges to bounded equality — abolition, suffrage, civil rights, modern expansion claims — proceed as fidelity to the founding text rather than rejection of it. It solves the collective-action problem that document-rejection could not: Garrisonian burning of the Constitution fragmented reform coalitions, while standing on the Declaration's own words unified them.
% TRANSFER_FUNCTION: Moves interpretive authority and moral standing from holders of founder-intent-bounded scope to challengers who invoke the text's universality; each deployment debits originalist authority claims and credits the deployer with textual fidelity. Secondarily, it moves attention and professional capital to the scholars and writers who articulate the contradiction.
% ABSENT_VOICES: The founding generation itself — slaveholding signers and ratifiers — would object that their personal inconsistency did not cede the text's meaning to their critics; they are permanently absent, so the paradox reading answers for them. Legal positivists who deny the Declaration any normative force are structurally excluded because the whole contest presupposes the text's authority. Enslaved people and excluded groups, the parties the paradox claims to speak for, were absent from the founding conversation that produced the contradiction.
% DISAPPEARANCE_RATIONALE: Originalist interpreters contend the argumentative landscape would barely change — on their view the paradox is rhetoric layered over a settled question of intent, and its loss would remove one debating device. Expansionist movements and critical scholars contend a load-bearing channel would collapse: two centuries of text-grounded critique (Douglass, the Lincoln-Douglas exchanges, Reconstruction-era argument, the civil-rights era's promissory-note framing) would lose its founding-document anchor, forcing challengers onto purely extrinsic moral grounds that opponents can dismiss as alien to the text. The parties genuinely dispute whether arrangements depend on the constraint.
% FOUNDING_PROBLEM: How can challengers attack the restricted application of the equality clause without repudiating the founding text — standing on the founders' words against the founders' practice? The problem crystallized in the antebellum crisis, when reformers had to choose between Garrisonian document-rejection and deploying the Declaration's own universality against its restrictors.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legal historians of antislavery constitutionalism document the strategic choice between document-rejection and text-grounded critique as real and consequential; originalist scholars themselves attest the contradiction's existence while disputing its consequence, conceding that the founders' practice diverged from their declarations; judicial opinions across eras acknowledge the tension even while refusing it operative force. No party denies the founding problem existed; the live dispute is over whether it remains unresolved.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, contested).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55): the paradox reading performs real critical work — it unified two centuries of dispersed challenges to bounded equality under a single text-grounded standard — but the same structure asymmetrically charges one interpretive community a standing coherence cost and delivers attention, standing, and career capital to the scholars who wield it (receipt seat: critical_constitutional_scholars). Suppression is low (0.30): the constraint coerces no one; originalists keep publishing, courts keep declining to enforce the Declaration, and three live alternatives persist (originalist demotion of the text, positivist non-law, universalist bypass). Theater is elevated (0.45): anniversary denunciations of founder hypocrisy form a durable ritual genre whose output rarely changes an interpretive outcome, running alongside functional deployments in litigation and scholarship. Accessibility collapse is partial (0.55): within the paradox frame the 'coherent bounded reading' alternative collapses almost entirely, but the frame itself is optional. Resistance is substantial (0.60): an organized originalist counter-literature and an institutionally ascendant originalist judiciary actively deflate the paradox. The temporal series run on one shared nine-point grid (1776-2026) and show a cyclical rather than monotone profile: the paradox's force surges during reform waves (abolition crisis, Reconstruction, the civil-rights era) and decays into ritual between them. The oscillation is partly an extraction mechanism in itself — intermittent reinforcement keeps the originalist seat paying defensive attention costs across cycles — and partly a side effect of external movement cycles. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the originalist interpreter's seat (institutional power, identity-locked exit), the constraint is an ambush: rhetoric dressed as interpretation that taxes their authority claims without engaging their premises. From the critical scholar's seat (moderate power, arbitrage exit), it is an instrument and a livelihood. From the excluded group's seat (powerless, trapped in the polity), it is a lifeline — often the only lever inside a legal order they cannot leave. From the positivist's seat (excluded), the whole contest is a category error. The engine derives these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: expansionist_reform_movements, excluded_groups_claiming_inclusion, and critical_constitutional_scholars sit at the beneficiary end — the constraint subsidizes them with a shared standard, a legal-order-internal lever, and a professional franchise respectively. Victims: originalist_interpreters bear the transfer — every deployment debits their authority claims — and their identity-locked exit (their standing is constituted by the lineage they defend) places them near the full-target end. The originalist_interpretive_framework is authored as a non-agent stakeholder per the expected structural delta ('victim is the framework itself'); as agent:false it feeds no derivation — the human bearers, originalist_interpreters, carry the directional weight. One override: excluded_groups_claiming_inclusion are trapped-in-the-polity beneficiaries, and a trapped-exit heuristic would wrongly push them toward the target end; the override sets d=0.15 to reflect that the constraint subsidizes them even though they cannot exit the system it critiques. Receipt: among the seats, gains demonstrably accrue to critical_constitutional_scholars (attention, standing, career capital); movements convert the instrument into goals rather than receiving the extraction itself, so the receipt seat is named rather than marked diffuse.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this reading as tangled_rope guards against two opposite mislabels. Read as pure critique (coordination-like truth-telling), the analysis would miss the asymmetric extraction: legitimacy flows out of one interpretive community into another, and an identifiable seat banks the proceeds. Read as pure attack (an extraction machine with no coordination content), it would miss the genuine coordination function: the paradox solved a real collective-action problem that document-rejection could not — Garrisonian burning of the Constitution fragmented reform coalitions, while text-grounded critique unified them. The founding problem (attacking restricted application without repudiating the text) remains live — bounded readings persist in jurisprudence and politics — so no mandatrophy is declared; the R5 interview records status=live with corroboration from legal historians and from originalist scholars themselves, who concede the historical inconsistency while disputing its consequence. The constraint is not a scaffold: it declares no transition endpoint and does not announce its own retirement once expansion completes. Nor is it a degraded shell: its deployments still bite, and its function has not atrophied into pure performance — theater is elevated but the functional share remains real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel all_men_created_equal (reading: textualist_paradox_reading). What would the sibling readings change structurally if adopted as the operative frame?',
    'Comparative classification across the three sibling stories: classify all_men_created_equal__originalist_reading and all_men_created_equal__universalist_reading on their own structural data and compare victim sets, extraction profiles, and persistence conditions.',
    'Under the originalist sibling, this reading''s victim set dissolves — bounded scope is coherent because intent governs, and the paradox reduces to a historical curiosity. Under the universalist sibling, this reading becomes transitional instrumentation whose critical force can be retired once expansion is complete.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of a contested kernel; sibling adoption changes the victim set and persistence conditions of this constraint.').

omega_variable(
    kernel_instability_or_signer_hypocrisy,
    'Does the performative contradiction expose genuine instability in the kernel itself (the text cannot bear determinate content), or merely document the signers'' personal inconsistency (text intact, word-users flawed)?',
    'Reception history and semantic analysis: determine whether the text''s first audience understood the universality as binding commitment or as aspirational rhetoric; trace whether the contradiction tracks the words'' semantics or the word-users'' conduct.',
    'Genuine instability means no reading of the kernel is stable and this reading''s negative case succeeds against all rivals; mere inconsistency leaves the originalist sibling fully intact and reduces this reading to a moral-psychological observation about the founders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_instability_or_signer_hypocrisy, conceptual, 'Whether the contradiction indicts the text''s semantics or only the founders'' practice — the reading''s central unresolved premise.').

omega_variable(
    framework_victim_agency,
    'The declared victim is an interpretive framework rather than persons: does delegitimation of a framework harm anyone, or does it merely redistribute authority among interpretive elites?',
    'Trace material consequences downstream of originalist authority losses: whether weakened intent-bounded scope correlates with materially improved standing for excluded groups, or only with turnover among jurists and scholars.',
    'If effects are purely elite redistribution, effective extraction from persons is lower than the framework-level measure suggests and the constraint drifts toward a coordination-dominant profile; if excluded groups materially gain, the extraction has identifiable human beneficiaries and victims on both sides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_victim_agency, conceptual, 'Whether a framework-level victim translates into person-level harm.').

omega_variable(
    semantic_universality_counterfactual,
    'Is the paradox''s coercive force intrinsic to the text''s universal semantics, or contingent on the historical accident of inconsistent signers — would a founding generation that practiced what it proclaimed have left the paradox toothless?',
    'Counterfactual founding analysis and comparison with other universal declarations whose authors practiced their principles: if consistent practice defuses the contradiction, the force is contingent; if the words alone generate it, the force is semantic.',
    'Contingent force means originalists can deflate the constraint by severing text-meaning from signer-conduct (their actual strategy), capping its extractiveness; intrinsic force means the constraint persists regardless of rebuttal and its victim set is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_universality_counterfactual, empirical, 'Whether the contradiction''s force is semantic or biographical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1776, 0.05).
narrative_ontology:measurement_basis(all__tr_t1776, observed).
narrative_ontology:measurement(all__tr_t1820, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1820, 0.15).
narrative_ontology:measurement_basis(all__tr_t1820, observed).
narrative_ontology:measurement(all__tr_t1852, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1852, 0.3).
narrative_ontology:measurement_basis(all__tr_t1852, observed).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1865, 0.25).
narrative_ontology:measurement_basis(all__tr_t1865, observed).
narrative_ontology:measurement(all__tr_t1896, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1896, 0.55).
narrative_ontology:measurement_basis(all__tr_t1896, observed).
narrative_ontology:measurement(all__tr_t1954, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1954, 0.4).
narrative_ontology:measurement_basis(all__tr_t1954, observed).
narrative_ontology:measurement(all__tr_t1963, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1963, 0.3).
narrative_ontology:measurement_basis(all__tr_t1963, observed).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement_basis(all__tr_t2000, observed).
narrative_ontology:measurement(all__tr_t2026, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2026, 0.45).
narrative_ontology:measurement_basis(all__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement_basis(all__be_t1776, observed).
narrative_ontology:measurement(all__be_t1820, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1820, 0.25).
narrative_ontology:measurement_basis(all__be_t1820, observed).
narrative_ontology:measurement(all__be_t1852, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1852, 0.62).
narrative_ontology:measurement_basis(all__be_t1852, observed).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1865, 0.7).
narrative_ontology:measurement_basis(all__be_t1865, observed).
narrative_ontology:measurement(all__be_t1896, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1896, 0.35).
narrative_ontology:measurement_basis(all__be_t1896, observed).
narrative_ontology:measurement(all__be_t1954, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1954, 0.55).
narrative_ontology:measurement_basis(all__be_t1954, observed).
narrative_ontology:measurement(all__be_t1963, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1963, 0.68).
narrative_ontology:measurement_basis(all__be_t1963, observed).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement_basis(all__be_t2000, observed).
narrative_ontology:measurement(all__be_t2026, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2026, 0.55).
narrative_ontology:measurement_basis(all__be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(all_men_created_equal__textualist_paradox_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, information_standard).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'all men are created equal' covers three structurally distinct claims with different epsilon values, victim sets, and failure modes. The originalist_reading (scope fixed by founding-generation understanding) is upstream — both rival readings define themselves against it, and its institutional strength shapes their operating environment. The textualist_paradox_reading (this file) is the negative-critical member: it attacks the originalist obstacle from inside the text's own words. The universalist_reading is the positive-program member: it programs expansion without needing the paradox. Each story carries its own stable epsilon; this file's moderate value reflects the paradox reading's own operation (legitimacy harvested from originalist authority claims), not the bounded-equality arrangement its siblings contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
