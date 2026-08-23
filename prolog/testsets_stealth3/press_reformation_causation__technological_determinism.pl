% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Print Capacity as Upstream Determinant of Confessional Outcome (Technological-Determinist Reading)
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism reading of the
 *   kernel press_reformation_causation: print capacity as an exogenous,
 *   self-executing constraint that made censorship impossible and vernacular
 *   scripture inevitable, with reformers as downstream beneficiaries of
 *   capacity they neither built nor maintained, and ecclesiastical
 *   counter-measures as structurally futile. The constraint is CLAIMED as
 *   mountain — the reading's entire thesis is that this is natural law
 *   operating through reproduction mechanics, requiring no administrator and
 *   no enforcement — and emerges_naturally is asserted accordingly. The
 *   metrics are authored from the same reading-indexed seat but describe the
 *   arrangement's operation as the reading's own sources record it:
 *   negligible extraction (nothing operates the constraint, so nothing
 *   collects through it), near-zero enforcement requirement (self-execution
 *   is the claim), total option-closure for the censorship establishment, and
 *   — deliberately — very high resistance met, because the reading's own
 *   chronicle is full of indices, burnings, and licensing regimes. A mountain
 *   claim carrying declared beneficiaries is intentional false-summit
 *   authoring: the omega natural_law_vs_constructed_attribution documents
 *   exactly the ambiguity the schema requires, and the engine's evaluation of
 *   that signature is the measurement this story exists to take. KEY AGENTS
 *   (by structural relationship): magisterial_reformers — principal
 *   beneficiary (organized/constrained), subsidized without maintaining;
 *   vernacular_bible_printers — beneficiary with cost exposure
 *   (moderate/mobile), the tangible money flow; roman_censorship_authorities
 *   — principal target (institutional/constrained), expenditure into
 *   futility; sixteenth_century_reading_public — diffuse beneficiary-payer
 *   (powerless/trapped); catholic_press_operators and
 *   humanist_mediator_scholars — excluded voices whose existence strains the
 *   frame; modern_historiographers — analytical observer seeing the full
 *   structure including the claim's own later career. Interval semantics: 0
 *   corresponds to roughly 1450 (Mainz), 200 to roughly 1650 (confessional
 *   settlement consolidated).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.22).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.15).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.22).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Print Capacity as Upstream Determinant of Confessional Outcome (Technological-Determinist Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history of technology / religious history / media studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'ac1f0574-2685-4ecf-822d-96e407d798a9').
narrative_ontology:cs_kernel_codification('ac1f0574-2685-4ecf-822d-96e407d798a9', formalized).
narrative_ontology:cs_authority_grounding('ac1f0574-2685-4ecf-822d-96e407d798a9', lineage).
narrative_ontology:cs_interpretation_layer_present('ac1f0574-2685-4ecf-822d-96e407d798a9').
narrative_ontology:cs_reading_relation('ac1f0574-2685-4ecf-822d-96e407d798a9', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('ac1f0574-2685-4ecf-822d-96e407d798a9', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('ac1f0574-2685-4ecf-822d-96e407d798a9', foundational, print_capacity_fixes_confessional_outcome).
narrative_ontology:cs_axiom_status(print_capacity_fixes_confessional_outcome, holdable).
narrative_ontology:cs_axiom_grounding('ac1f0574-2685-4ecf-822d-96e407d798a9', print_capacity_fixes_confessional_outcome, empirically_contingent).
narrative_ontology:cs_axiom('ac1f0574-2685-4ecf-822d-96e407d798a9', foundational, post_print_censorship_impossible).
narrative_ontology:cs_axiom_status(post_print_censorship_impossible, holdable).
narrative_ontology:cs_axiom_grounding('ac1f0574-2685-4ecf-822d-96e407d798a9', post_print_censorship_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('ac1f0574-2685-4ecf-822d-96e407d798a9', print_capacity_as_upstream_determinant).
narrative_ontology:cs_drift_state('ac1f0574-2685-4ecf-822d-96e407d798a9', contemporary_revisionist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac1f0574-2685-4ecf-822d-96e407d798a9', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, magisterial_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_bible_printers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, roman_censorship_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, sixteenth_century_reading_public).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, vernacular_bible_printers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, sixteenth_century_reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wittenberg-aligned reformers issued vernacular treatises, pamphlets, and scripture portions through sympathetic shops. The reproductive capacity of print carried their message faster than authorities could respond, without the reformers having built or maintained that capacity. Once committed, recantation was doctrinally unthinkable and physically dangerous, so their position inside the arrangement was locked even as its benefits flowed to them.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, magisterial_reformers, beneficiary,
    organized, generational, constrained, continental).

% Commercial print shops collected direct revenue from vernacular Bibles, testaments, and polemical sheets — the most tangible money flow the arrangement generated. Some paid severely when enforcement reached them: imprisonment, destruction of stock, and in recorded cases execution for producing proscribed vernacular editions. Many shops switched confessional stock with the market, so exit was real but costly and never total.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_bible_printers, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__technological_determinism, vernacular_bible_printers, payer).

% Congregations, university faculties, and imperial chanceries funded licensing regimes, confiscation campaigns, index-making, and prosecution against vernacular diffusion. On this reading their expenditure purchased delay, never prevention: every enforcement instrument they built was eventually swamped by reproductive capacity they did not control and could not shut down. Exiting meant conceding the field entirely, so spending continued against diminishing returns.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, roman_censorship_authorities, payer,
    institutional, generational, constrained, continental).

% Urban literate households gained access to affordable vernacular scripture and controversy for the first time. They paid through prices set by risk premiums and through periodic exposure to proscription and search. They could not choose the informational environment they inherited; they simply lived inside the new supply of text.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, sixteenth_century_reading_public, beneficiary,
    powerless, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__technological_determinism, sixteenth_century_reading_public, payer).

% The Antwerp, Louvain, Cologne, and Roman houses ran large, profitable, officially sanctioned Catholic print operations throughout the period, supplying breviaries, catechisms, controversial theology, and vernacular devotionals at scale. Their commercial success sits awkwardly beside any claim that the medium determined confession, yet the deterministic account gives them no speaking part: they are omitted rather than answered.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_press_operators, excluded,
    powerful, generational, trapped, continental).

% The Erasmian program sought reform through philology and moderated publication — print treated as a negotiable instrument under scholarly judgment. A framing in which vernacular rupture was inevitable renders mediation a category mistake, so this entire program drops out of the causal conversation rather than being argued against.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, humanist_mediator_scholars, excluded,
    organized, biographical, constrained, continental).

% The comparative book-history field tests the causal attribution against enforcement archives, printing-house account books, and cross-regional comparison. It neither collects from nor pays into the arrangement; its seat is analytical, seeing both the historical process and the later career of the claim about it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, modern_historiographers, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized mechanical reproduction of text solved, without any coordinator, the problem of delivering identical doctrinal content to dispersed literate publics faster than authorities could formulate responses. The reading treats this standardization as the operative coordination: replication fidelity plus speed, achieved by machinery rather than agreement.
% TRANSFER_FUNCTION: Moves reproducible doctrinal content, and the interpretive authority attached to controlling it, from clerical gatekeepers to vernacular readerships via commercial intermediaries. Simultaneously moves enforcement expenditure from the censorship establishment into purchases of delay that never convert into prevention.
% ABSENT_VOICES: Catholic press operators and humanist mediator scholars would object if seated: the former by demonstrating that print was a contested commercial resource on which both confessions thrived, the latter by insisting publication choices remained matters of judgment. Both are omitted from the deterministic narrative rather than refuted within it.
% DISAPPEARANCE_RATIONALE: Remove the reproductive-capacity constraint overnight — revert to scriptorial reproduction — and the confessional map rearranges: diffusion slows to hand-copy pace, censorship regains feasibility against finite manuscript counts, mass vernacular scripture does not arrive, and the reformers' survival against imperial coordination becomes improbable. Every seat's situation in this story depends on the constraint's operation.
% FOUNDING_PROBLEM: Explaining how a dissenting movement survived coordinated imperial hostility and reached mass publics. The reading crystallized in the nineteenth and twentieth centuries, before archival recovery of the scale and effectiveness of Catholic counter-printing, when technological capacity was the most visible available variable — and it supplied the mechanism that modernization narratives needed for the arrival of modernity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the archival record itself — surviving index volumes, confiscation registers, licensing ledgers, and Catholic printing-house account books — and by revisionist monographs whose authors hold no stake in the determinist canon. No beneficiary party attests the founding problem's death: gatekeeping institutions continue to trade on its liveness because the simple causal story remains commercially and pedagogically salable.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.22: by this reading's lights the arrangement extracts almost nothing, because nothing operates it — no party administers the constraint and therefore no party collects through it; the residual value registers commercial capture at the arrangement's edge (print-shop margins on vernacular stock), which the reading classifies as commerce riding on physics rather than extraction by the constraint. Suppression 0.15: suppression is a raw structural property, unscaled by power or scope, and on this reading the constraint coerces no one — the severity experienced by the censorship establishment is option-closure, which is carried by accessibility_collapse (0.88) rather than by enforcement. Theater_ratio 0.12: nearly all recorded activity around the constraint is functional — printing happened, diffusion happened, enforcement responded — with only a thin margin of ritual (condemnation ceremonies, symbolic burnings) that changed no outcomes. Resistance 0.70: authored HIGH despite the mountain claim, deliberately and without reconciliation — the reading's own source base chronicles vast, sustained, well-funded resistance; the reading holds that resistance was futile, and futility is a claim about efficacy, not about volume. Claim and metrics are left in tension on purpose: a mountain that meets heavy resistance and shelters beneficiaries is precisely the profile the false-summit detector exists to examine, and tuning either side to agree would destroy the datum. The temporal series run on one shared six-point grid (all three metrics authored at every point); the suppression_requirement series is near-flat by design — it documents the self-execution thesis, namely that the constraint's own enforcement machinery was negligible throughout, in contrast to the enormous enforcement machinery its opponents built.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the reformer seat the arrangement is a gift of physics: pure subsidy, no administrator, no bill — a mountain-or-rope flavor with zero felt extraction. From the censorship-establishment seat the same arrangement is ruin: institutional resources consumed against an immovable process, a strongly target-flavored experience approaching snare phenomenology even under a mountain claim. From the printer seat it is a market with hazard — benefit front-loaded, punishment distributed irregularly, exit real but priced. From the reading-public seat it is an environment, not a choice. The engine computes these per-seat classifications from power, exit, and role; the divergence between the beneficiary seats and the payer seat is the perspectival content of the story. The excluded seats mark where a payer coalition could have formed: catholic_press_operators plus humanist_mediator_scholars jointly possess the evidence and the program that would dissolve the inevitability claim, but the frame's omission of both is what keeps the coalition unassembled.
 *
 * DIRECTIONALITY LOGIC:
 *   Magisterial_reformers derive a low d from unambiguous beneficiary declaration — the constraint subsidizes them and they bear no maintenance burden. Vernacular_bible_printers are dual-positioned (beneficiary primary, payer secondary): the derivation cannot see that their costs are asymmetric and event-like (execution, confiscation) while gains are continuous (revenue), so an override sets d to 0.3 — net beneficiary with real cost exposure, not a symmetric seat. The sixteenth-century reading public are declared beneficiaries carrying a secondary payer role, but their trapped exit would push the derivation toward the target end, misreading a captive audience as a captive payer; the override sets d to 0.35, reflecting net subsidy under captivity. Roman_censorship_authorities need no override: institutional power, constrained exit, and sole placement in the victims array derive a d near the full-target end, which matches the reading's own claim that their entire enforcement program purchased delay only. The excluded seats sit outside the derivation by design — their function is counterfactual testimony, not directional weight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining dissent's survival against coordinated hostility before the Catholic counter-print archive was known — has been dissolved by specialist historiography, yet the arrangement persists in curricular and popular canon. That dead-problem-plus-live-structure profile is the mismatch the R5 consumer flags, cross-checked against the computed path; this story anticipates the flag rather than avoiding it. The classification work here prevents two symmetrical mislabelings: reading the genuine coordination achievement (standardized reproduction serving dispersed publics) as mere extraction cover would erase the real subsidy reformers and readers received; reading the mountain claim at face value would launder a constructed attribution as natural law while identifiable parties traded on it. The false-summit machinery is the designated resolver: beneficiaries are declared, the natural-law ambiguity is carried in a dedicated omega, and the engine owns the verdict. Mandatrophy resolution is thus deferred to computation, which is the deferential-realist posture this corpus exists to exercise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the technological_determinism reading of the kernel press_reformation_causation — what structurally changes if a sibling reading were adopted instead?',
    'Adopting strategic_deployment relocates reformers from downstream beneficiaries to agenda_setters and makes the church seat a contested adversary rather than a futile payer; adopting mutual_shaping removes the pure upstream mountain entirely, making capacity and movement co-determined. The disagreement is located in two structural elements: the direction of the causal arrow and the locus of agency.',
    'Sibling adoption would rewrite the beneficiary/victim structure, the directionality profile, and the claimed type of the emitted constraint; the epsilon referent stays fixed (the standing press-confession arrangement) while the reading-indexed value and type both move.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame record: one of three readings of the causation kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    natural_law_vs_constructed_attribution,
    'Is the press''s causal role a structural feature of communication itself — a natural-law constraint no party maintains — or a constructed attribution that serves identifiable narrators?',
    'Compare print-saturated regions with divergent confessional outcomes under equal access: northern Italy and Spain absorbed print at rates comparable to Saxony and Geneva yet remained Catholic for centuries. Structured cross-regional comparison isolates capacity from outcome.',
    'Divergent outcomes under equal access would establish the constraint as constructed rather than natural, confirming the false-summit signature (mountain claim with identifiable beneficiaries) and placing the engine''s reclassification chain in play.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_attribution, empirical, 'The irreducible natural-law-versus-constructed ambiguity required on a mountain claim that declares beneficiaries.').

omega_variable(
    censorship_efficacy_record,
    'Did censorship actually become impossible after print, or did enforcement remain costly-but-functional for centuries?',
    'Systematic analysis of index enforcement archives, confiscation registers, licensing compliance rates, and prosecution outcomes across jurisdictions and decades.',
    'Demonstrated long-run enforcement functionality would mean the accessibility-collapse measure is overstated, the impossibility axiom weakens, and the mountain claim loses its central mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_efficacy_record, empirical, 'Empirical status of the ''censorship impossible'' component.').

omega_variable(
    vernacular_counterfactual_contingency,
    'Was vernacular scripture inevitable, or contingent on political fragmentation preventing coordinated suppression?',
    'Structured counterfactual comparison between polities that suppressed vernacular Bibles successfully for generations (Spain, Portugal, Habsburg Italian states) and the fragmented Empire where suppression failed.',
    'Successful multi-generation suppression under unified authority would show inevitability was a political contingency, not a property of the medium, collapsing the reading toward the strategic_deployment sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_counterfactual_contingency, empirical, 'Counterfactual status of the ''vernacular scripture inevitable'' component.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__technological_determinism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__technological_determinism, theater_ratio, 40, 0.06).
narrative_ontology:measurement(pres_tr_t80, press_reformation_causation__technological_determinism, theater_ratio, 80, 0.08).
narrative_ontology:measurement(pres_tr_t120, press_reformation_causation__technological_determinism, theater_ratio, 120, 0.09).
narrative_ontology:measurement(pres_tr_t160, press_reformation_causation__technological_determinism, theater_ratio, 160, 0.1).
narrative_ontology:measurement(pres_tr_t200, press_reformation_causation__technological_determinism, theater_ratio, 200, 0.12).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__technological_determinism, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(pres_be_t80, press_reformation_causation__technological_determinism, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(pres_be_t120, press_reformation_causation__technological_determinism, base_extractiveness, 120, 0.19).
narrative_ontology:measurement(pres_be_t160, press_reformation_causation__technological_determinism, base_extractiveness, 160, 0.21).
narrative_ontology:measurement(pres_be_t200, press_reformation_causation__technological_determinism, base_extractiveness, 200, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__technological_determinism, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(pres_su_t40, press_reformation_causation__technological_determinism, suppression_requirement, 40, 0.09).
narrative_ontology:measurement(pres_su_t80, press_reformation_causation__technological_determinism, suppression_requirement, 80, 0.1).
narrative_ontology:measurement(pres_su_t120, press_reformation_causation__technological_determinism, suppression_requirement, 120, 0.11).
narrative_ontology:measurement(pres_su_t160, press_reformation_causation__technological_determinism, suppression_requirement, 160, 0.13).
narrative_ontology:measurement(pres_su_t200, press_reformation_causation__technological_determinism, suppression_requirement, 200, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The colloquial label 'the printing press caused the Reformation' decomposes into three structurally distinct readings of the kernel press_reformation_causation. This file instantiates technological_determinism (technology as exogenous mountain, reformers as downstream beneficiaries, church resistance futile). The siblings — strategic_deployment (purposeful actors directing neutral capacity) and mutual_shaping (co-evolution of medium and movement) — are separate constraint files with their own epsilon, beneficiary/victim structures, and classifications, linked here via affects_constraints. Note the components of this reading's claim ('censorship impossible', 'vernacular scripture inevitable') are NOT split into further files: they are joint commitments of one reading over one referent, and cutting them apart would manufacture sibling readings inside a reading, violating the one-reading-one-epsilon rule. The decomposition lives at the reading level, not the component level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causation__technological_determinism, moderate, 0.3).
constraint_indexing:directionality_override(press_reformation_causation__technological_determinism, powerless, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
