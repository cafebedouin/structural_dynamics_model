% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Textualist Paradox Reading: Universal Equality Language vs. Restricted Application
 *   domain: constitutional law/political philosophy/American studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel sentence 'all men are
 *   created equal': the textualist paradox reading, on which the sentence's
 *   universal language is irreconcilable with restricted application, and
 *   that irreconcilability operates as a standing liability on anyone who
 *   claims founding-text authority while bounding the sentence's scope. The
 *   standing arrangement under contest - the epsilon referent, assessed by
 *   this reading's own lights - is the originalist authority claim over the
 *   sentence's meaning; from this reading's position that arrangement carries
 *   an unresolved performative contradiction it cannot discharge, which taxes
 *   every invocation of founding-text authority. The originalist_reading and
 *   universalist_reading are SEPARATE constraints (separate files, separate
 *   epsilon, separate stakeholder surfaces); nothing about them is averaged
 *   into this story. The claim/metrics gap is deliberate: claimed_type states
 *   what I believe is structurally true (a genuine coordination standard with
 *   asymmetric legitimacy costs and active deployment requirements), while
 *   the metrics state what I believe descriptively true of the arrangement's
 *   actual operation - the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computation is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - originalist_scholars_and_judges: Primary target (powerful/constrained) - bear the delegitimation toll each time founding-text authority is invoked; exit paths (demote the Declaration, or abandon founding-text authority) are real but professionally costly
 *   - - originalist_interpretive_framework: Non-agent bearer (institutional/trapped) - the doctrine itself cannot amend the text it claims; its exposures are carried by its human holders
 *   - - abolitionist_reformers: Historical beneficiary (organized/mobile) - converted the founders' own words into an indictment of slaveholding practice
 *   - - civil_rights_litigators: Mid-century beneficiary (organized/constrained) - cast segregation as a broken pledge in litigation
 *   - - universalist_interpreters: Standing beneficiary (powerful/arbitrage) - hold the reading the parity objection naturally supports; can invoke or bypass it at will
 *   - - founding_era_disenfranchised: Excluded party (powerless/trapped) - governed by the restricted application, absent from the conversation that fixed it
 *   - - constitutional_theorists: Analytical observer (analytical/analytical) - maps the three-way reading dispute without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.56).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.34).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist Paradox Reading: Universal Equality Language vs. Restricted Application").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional law/political philosophy/American studies").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '705f74a5-ae10-4550-9b61-a1398c5789e2').
narrative_ontology:cs_kernel_codification('705f74a5-ae10-4550-9b61-a1398c5789e2', fixed_text).
narrative_ontology:cs_authority_grounding('705f74a5-ae10-4550-9b61-a1398c5789e2', distributed).
narrative_ontology:cs_reading_relation('705f74a5-ae10-4550-9b61-a1398c5789e2', all_men_created_equal__originalist_reading, influences).
narrative_ontology:cs_reading_relation('705f74a5-ae10-4550-9b61-a1398c5789e2', all_men_created_equal__universalist_reading, influences).
narrative_ontology:cs_axiom('705f74a5-ae10-4550-9b61-a1398c5789e2', foundational, textual_universality_binds_claimants).
narrative_ontology:cs_axiom_status(textual_universality_binds_claimants, holdable).
narrative_ontology:cs_axiom_grounding('705f74a5-ae10-4550-9b61-a1398c5789e2', textual_universality_binds_claimants, deontological).
narrative_ontology:cs_axiom('705f74a5-ae10-4550-9b61-a1398c5789e2', secondary, public_meaning_over_signer_conduct).
narrative_ontology:cs_axiom_status(public_meaning_over_signer_conduct, holdable).
narrative_ontology:cs_axiom_grounding('705f74a5-ae10-4550-9b61-a1398c5789e2', public_meaning_over_signer_conduct, conventional).
narrative_ontology:cs_reference_frame('705f74a5-ae10-4550-9b61-a1398c5789e2', universal_self_authenticating_text).
narrative_ontology:cs_drift_state('705f74a5-ae10-4550-9b61-a1398c5789e2', contemporary_originalist_revival, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('705f74a5-ae10-4550-9b61-a1398c5789e2', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, abolitionist_reformers).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, civil_rights_litigators).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, universalist_interpreters).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_scholars_and_judges).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, textual_universality_premise).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, parity_between_language_and_application).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A doctrine holding that the scope of the 1776 sentence is fixed by the founding generation's understanding, bounded by that era's social taxonomy. The doctrine cannot revise the text it claims: the sentence's wording is fixed, so every invocation of it as authority exposes the gap between its universal language and historically restricted application. The doctrine itself bears these exposures; the costs are carried by the jurists and scholars who advance it.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework).

% Build careers, methods, and institutional networks around grounding constitutional authority in founding texts. When they lean on the Declaration's equality language, they invite the rejoinder that its universality condemns bounded applications; when they avoid it, their method loses its most civically resonant document. Available retreats - relying on the constitutional text alone, or arguing the Declaration carries no legal force - are workable but cost the method prestige, and leaving the founding-text framework entirely would dissolve the professional identity built on it.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_scholars_and_judges, payer,
    powerful, generational, constrained, national).

% Nineteenth-century critics of slavery who demanded consistency with the founders' own words rather than repudiating the founders - Douglass's 1852 address is the canonical deployment. The gap between universal language and slaveholding practice gave them standing to indict the nation on its own terms. They held other strategies (wholesale repudiation of the founders) and moved among them freely.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, abolitionist_reformers, beneficiary,
    organized, biographical, mobile, national).

% Twentieth-century lawyers who paired the Fourteenth Amendment with the Declaration's promise in briefs and argument, casting segregation as a broken pledge rather than a new claim. Their use of the parity argument was channeled through litigation strategy and court access, which bounded how and when they could deploy it.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, civil_rights_litigators, beneficiary,
    organized, biographical, constrained, national).

% Living-constitution and moral-reading theorists for whom the sentence's scope tracks its language rather than the founding taxonomy. They invoke the parity objection when it serves and route around it through the Fourteenth Amendment when it does not. The leverage their arguments generate disperses across successive reform coalitions rather than accumulating to any single camp.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_interpreters, beneficiary,
    powerful, generational, arbitrage, continental).

% Enslaved people, women, and non-propertied men at the founding - the people the restricted application actually governed. None were present in the rooms where the sentence's operative scope was settled; their objection to bounded application was structurally impossible to register at the time and reaches the conversation today only through descendants and historians.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, founding_era_disenfranchised, excluded,
    powerless, biographical, trapped, national).

% Scholars who map the dispute over the sentence's meaning - including originalists who accept the universal reading (Barnett) and universalists who doubt the text's legal force at all. They adjudicate coherence between text, history, and doctrine without collecting or paying anything under the arrangement.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_theorists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, diffuse).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, theory-neutral critical standard: factions that disagree about everything else in interpretation can agree to measure any bounded founding-authority claim against the sentence's own universal wording. It coordinates dispersed critics - abolitionists, suffragists, civil rights lawyers, liberal academics - without requiring a common positive theory.
% TRANSFER_FUNCTION: Moves interpretive legitimacy and civic authority from holders of restricted-application readings to whichever expansive-reading coalition is currently invoking the parity objection, each time the gap between the sentence's language and its application is pressed in courts, classrooms, or public rhetoric.
% ABSENT_VOICES: The founders themselves are absent and cannot say whether the sentence was aspiration or description; both rival camps speak for the silent signers. Founding-era enslaved people, women, and non-propertied men - those the restricted application governed - were excluded from the conversation that fixed its scope and appear only through descendants and historians. Neither absence can be closed; both are load-bearing for the dispute.
% DISAPPEARANCE_RATIONALE: If the parity objection became unavailable overnight - if no one could press the gap between universal language and restricted application - reform traditions would lose their founding-document warrant and have to argue from other sources; originalist authority claims over the sentence would regain largely uncontested footing; civic pedagogy would lose its central reconciliation script of promise-versus-practice. The text itself would be unchanged; the discursive settlement around it would rearrange.
% FOUNDING_PROBLEM: How can critics of slavery and exclusion challenge defenders who claim fidelity to the founding texts without repudiating the founders outright? The paradox reading answered: demand consistency with the founders' own universal words, turning the text against restricted practice instead of attacking the text's authority.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists corroborate the problem's liveness from outside the beneficiary set: Taney's Dred Scott opinion attacked the proposition head-on, Calhoun denied its self-evidence, and modern originalist doctrine insists the Declaration carries no legal force - defensive architecture that only makes sense if the parity objection still bites. Historians of the Declaration's reception independently document the recurring cycle of invocation and rebuttal.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.56 at interval end) rather than high because the toll is real but escapable: originalists retain substantial institutional power and have functioning retreats (Declaration-has-no-legal-force doctrine, reliance on the constitutional text alone). Suppression is low-moderate (0.34) and is authored as a raw structural property - the engine scales only extractiveness by directionality and scope. The constraint does not imprison anyone; it taxes claims, and targets can stop making them. Theater ratio (0.28) is low-moderate: most deployment does real analytical work, but a growing share is ceremonial (Fourth-of-July rhetoric, commemorative invocation) rather than load-bearing. Accessibility collapse (0.40) is low because alternatives visibly persist: targets escaped via Calhounian rejection of the proposition, Taney's denial that it bound the signers' practice, and modern demotion of the Declaration to preamble-status. Resistance (0.62) is substantial for the same reason - this constraint has been fought continuously for 170 years, which is itself evidence it is a construct with stakes rather than an accepted background regularity. CYCLICAL PATTERN: the extractiveness series oscillates rather than drifting - peaks at 1863 (Gettysburg consecration) and 1954-1968 (Brown through King), troughs at 1909 (Plessy-era marginalization) and 2000 (originalist consolidation). The oscillation is driven by EXTERNAL movement cycles - reform mobilization raises deployment intensity, reaction periods starve it - and is NOT an intermittent-reinforcement mechanism operated by the constraint itself; the constraint has no operator cycling it. Base_properties scalars reflect the interval-end (2025) phase: post-originalist-revival, mid-recovery, which is why epsilon sits at 0.56 rather than the 0.75 civil-rights-era peak. All temporal series run on one shared nine-point grid; suppression_requirement is intentionally not tracked because the story traces deployment intensity, not enforcement-capacity construction or decay - the static enforcement picture is carried by the suppression scalar.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seat, the parity objection is a hostile instrument that punishes fidelity to the founding texts; from the beneficiary seats it is liberating truth-telling that finally holds the nation to its word; from the observer seat it is a symmetric logical observation about a fixed text. Same sentence, same history - three incompatible experiences, computed by the engine from the structural data rather than adjudicated by this claim. IDENTITY-LOCK DYNAMICS: the binding on originalist scholars and judges is substantially professional-ideological fusion - the founding-text method is their career credential, institutional network, and public brand simultaneously, so the exit that would evade the toll (demoting the Declaration) damages the asset their identity is fused to. If that identity frame broke, the seat would split: some holders would take the Lincoln-Douglass move (embrace the universal text, converting the toll into endorsement) and others the Calhoun-Scalia move (demote the text, exiting the kernel's authority entirely). Either break collapses the payer seat's extraction profile.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. originalist_scholars_and_judges: declared victim, powerful but with constrained exit (retreats cost professional prestige) - derived d sits near the full-target end, so effective extraction is amplified. originalist_interpretive_framework: authored with agent=false precisely so this non-agent entity does NOT feed d->chi derivation; it is narrative completeness, not a directional seat. abolitionist_reformers and civil_rights_litigators: declared beneficiaries with mobile/constrained exits - d near the beneficiary end, extraction damped. universalist_interpreters: beneficiary with arbitrage-grade exit (invoke or bypass at will) - sits nearest the beneficiary end of all seats. founding_era_disenfranchised: role=excluded, neither collecting nor paying under the current arrangement; their structural position motivates the universalist reading rather than feeding this constraint's arithmetic. No directionality overrides are authored: beneficiary/victim declarations plus exit options produce the correct qualitative d for every seated agent, so the derivation chain stands without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading this as a SNARE would be wrong: exits are open and used (targets demoted the Declaration or abandoned founding-text authority entirely), no machinery suppresses those exits, and the 'victim' can end the toll unilaterally by changing what it claims - extraction that evaporates on exit is not a snare's signature. Reading it as a MOUNTAIN would also be wrong: although the underlying logical point (an assertion contradicted by the assertor's practice) is audience-independent, the DELEGITIMATING FORCE is conventional - it operates only on audiences who prize both textual fidelity and consistency, and it lapses when those valuations lapse (Plessy-era America largely ignored it). The genuine coordination function - a theory-neutral standard that let factions who agree on nothing else coordinate critique - is real and load-bearing, which puts this in the rope family; the asymmetric incidence (originalist authority claims pay, expansive coalitions collect) plus the active deployment requirement (the objection must be continuously pressed or it decays) makes it a tangled rope rather than a rope. MANDATROPHY WATCH: the founding problem (challenging bounded readings without repudiating the founders) is still live, but if the universalist reading ever fully absorbs the culture, the parity objection becomes redundant - already-preached truth - and this constraint would decay toward ceremonial invocation, the piton signature. The theater_ratio series is the early-warning indicator for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (textualist_paradox_reading) of the kernel ''all men are created equal''; the sibling readings originalist_reading and universalist_reading instantiate different constraints with different beneficiary/victim structures - which reading''s institutional fortunes determine whether this constraint''s extraction target persists?',
    'Track doctrinal adoption and citation practice: if originalist institutions consolidate a Declaration-has-no-legal-force settlement, this constraint''s toll vanishes; if the universalist reading becomes hegemonic, the paradox becomes redundant and decays toward ceremony.',
    'Determines whether epsilon stays moderate (contested kernel), collapses toward zero (target demotes the text), or converts into piton-style theatrical maintenance (reading absorbed by a triumphant universalism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Sibling-reading competition governs this constraint''s persistence and severity.').

omega_variable(
    founder_intent_indeterminacy,
    'Did the founding generation understand the sentence as an aspirational-universal proposition or as a descriptive claim bounded by their own taxonomy?',
    'Historiographic synthesis of the drafting record, ratification-era usage, and signer behavior (slaveholder signatories, Jefferson''s condemnations, the 1787 Northwest Ordinance) - acknowledging that the archive may remain genuinely ambiguous.',
    'If descriptive-bounded, the contradiction dissolves into ordinary value-change and epsilon falls sharply; if aspirational, the paradox strengthens and epsilon rises toward the civil-rights-era peak.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_intent_indeterminacy, empirical, 'Whether the historical record supports a contradiction or merely changed values.').

omega_variable(
    contradiction_type_ambiguity,
    'Is the gap between universal language and restricted application a performative contradiction (the authority claim self-undermined by the claimant''s own practice) or mere hypocrisy or value-change with no self-undermining structure?',
    'Conceptual analysis of what the authority claim presupposes: if invoking the text presupposes its universality, the restriction is self-refuting; if invoking it only presupposes historical pedigree, the gap is ordinary inconsistency.',
    'A performative structure taxes ALL bounded readings that cite the text; a hypocrisy structure taxes only claimants who also affirm universality - materially changing the victim set and the constraint''s reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contradiction_type_ambiguity, conceptual, 'Which logic of inconsistency the delegitimating force actually runs on.').

omega_variable(
    framework_victim_abstraction,
    'The declared victim is an interpretive framework borne by human holders rather than persons directly harmed - does framework-level delegitimation count as extraction comparable to person-level harm for severity calibration?',
    'Compare the toll''s realized costs on holders (career, doctrinal, institutional losses) against extraction benchmarks from constraints with direct person-victims; decide whether the payer seat is severity-comparable or should be weighted down.',
    'If framework-level harm is discounted, effective extraction falls and the constraint drifts toward rope; if credited fully, the moderate-high range is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framework_victim_abstraction, conceptual, 'Whether a doctrine can be a victim in the same sense a person can.').

omega_variable(
    rhetorical_force_naturalness,
    'Is the delegitimating force a logical invariant (a contradiction is a contradiction regardless of audience) or a conventional rhetorical norm that requires audiences who prize textual fidelity and consistency?',
    'Comparative analysis of periods and publics where the parity objection failed to bite (Plessy-era jurisprudence, publics indifferent to founding-text authority) versus where it dominated - if force tracks audience valuation, it is conventional.',
    'If conventional, the constraint can lapse into theatrical maintenance (piton risk) when civic valuations shift; if logical, it persists wherever the text is cited, independent of cultural phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_force_naturalness, conceptual, 'Whether the constraint''s force is audience-independent or convention-dependent - the persistence question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 1852, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tmp_paradox_reading_tr_t1852, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1852, 0.15).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t1852, observed).
narrative_ontology:measurement(tmp_paradox_reading_tr_t1863, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1863, 0.16).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t1863, observed).
narrative_ontology:measurement(tmp_paradox_reading_tr_t1877, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1877, 0.22).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t1877, observed).
narrative_ontology:measurement(tmp_paradox_reading_tr_t1909, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1909, 0.26).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t1909, observed).
narrative_ontology:measurement(tmp_paradox_reading_tr_t1932, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1932, 0.24).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t1932, observed).
narrative_ontology:measurement(tmp_paradox_reading_tr_t1954, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1954, 0.17).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t1954, observed).
narrative_ontology:measurement(tmp_paradox_reading_tr_t1968, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t1968, observed).
narrative_ontology:measurement(tmp_paradox_reading_tr_t2000, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2000, 0.29).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t2000, observed).
narrative_ontology:measurement(tmp_paradox_reading_tr_t2025, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(tmp_paradox_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tmp_paradox_reading_be_t1852, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1852, 0.5).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t1852, observed).
narrative_ontology:measurement(tmp_paradox_reading_be_t1863, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1863, 0.66).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t1863, observed).
narrative_ontology:measurement(tmp_paradox_reading_be_t1877, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1877, 0.6).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t1877, observed).
narrative_ontology:measurement(tmp_paradox_reading_be_t1909, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1909, 0.3).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t1909, observed).
narrative_ontology:measurement(tmp_paradox_reading_be_t1932, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1932, 0.38).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t1932, observed).
narrative_ontology:measurement(tmp_paradox_reading_be_t1954, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1954, 0.72).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t1954, observed).
narrative_ontology:measurement(tmp_paradox_reading_be_t1968, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1968, 0.75).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t1968, observed).
narrative_ontology:measurement(tmp_paradox_reading_be_t2000, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2000, 0.47).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t2000, observed).
narrative_ontology:measurement(tmp_paradox_reading_be_t2025, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2025, 0.56).
narrative_ontology:measurement_basis(tmp_paradox_reading_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(all_men_created_equal__textualist_paradox_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, information_standard).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the meaning of all men are created equal' conflates three structurally distinct constraints: the originalist reading (scope fixed by founding-generation understanding), this textualist paradox reading (universal language irreconcilable with restricted application delegitimates bounded authority claims), and the universalist reading (equality as universal principle requiring iterative expansion). Each has its own epsilon, its own victims, its own classification; this story carries only the paradox reading. Network edges run FROM this reading TO both siblings because it exerts asymmetric pressure: downward on originalist legitimacy conditions (every bounded claim incurs the parity toll) and upward on universalist legitimacy conditions (supplies universalism's argumentative engine) - without foreclosing either, since originalism survives by demoting the text's authority and universalism predates and exceeds this argument. Sibling files should link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
