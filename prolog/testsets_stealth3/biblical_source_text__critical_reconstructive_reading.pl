% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical-Reconstructive Reading of the Biblical Source Text Kernel
 *   domain: religious/academic/linguistic
 *
 * SUMMARY:
 *   Within biblical studies, the critical-reconstructive reading of the
 *   source-text question holds that historical recovery of the (hypothetical)
 *   original text is the primary task, and that neither structural fidelity
 *   nor communicative meaning may be privileged until that textual basis is
 *   established. The ordering is enforced through journal peer review,
 *   edition-committee method policy, seminary curricula, and
 *   translation-project vetting. It solves a genuine coordination problem —
 *   every translator and interpreter gains a common evidentiary baseline —
 *   while imposing asymmetric costs: confessional communities bound to
 *   received texts bear repeated destabilization of the wording their
 *   authority rests on, and the academic guild collects the publication,
 *   grant, and curricular economy the program sustains. KEY AGENTS (by
 *   structural relationship): see key_agents. Claim/metric independence is
 *   deliberate: the claimed type states what I believe is structurally true
 *   of this reading's arrangement; the metrics state what I believe is
 *   descriptively true of its operation; the engine computes per-seat
 *   classifications from the structural data, and any divergence is signal,
 *   not error. This file is one reading of the kernel biblical_source_text;
 *   the formal-equivalence and dynamic-equivalence readings are separate
 *   constraint files linked in network.affects_constraints.
 *
 * KEY AGENTS:
 *   - academic_biblical_scholarship: Primary beneficiary (institutional/identity_locked) — collects the publication, grant, and curricular economy the reconstructive program sustains
 *   - critical_edition_committees: Agenda setter (institutional/constrained) — decides adjudication method and publishes the operative base text; secondarily collects editorial authority
 *   - received_text_confessional_communities: Primary payer (organized/identity_locked) — bears destabilization of the received wording its doctrinal and liturgical authority depends on
 *   - lay_scripture_readers: Diffuse payer (powerless/trapped) — absorbs wording instability and footnote-driven uncertainty with no seat in adjudication
 *   - translation_committees_and_bible_societies: Dual payer/beneficiary (institutional/constrained) — funds the apparatus and gains defensibility from it
 *   - manuscript_archive_institutions: Secondary beneficiary (powerful/arbitrage) — converts witness scarcity into access pricing and imaging revenue
 *   - meaning_first_interpretive_schools: Excluded voice (moderate/constrained) — the priority ordering rules their contribution out of turn
 *   - philological_method_historians: Analytical observer (analytical/analytical) — sees the full structure from outside every seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.58).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.5).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical-Reconstructive Reading of the Biblical Source Text Kernel").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/academic/linguistic").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, 'fe1c421b-fe73-48c2-aa9b-170704c5f149').
narrative_ontology:cs_kernel_codification('fe1c421b-fe73-48c2-aa9b-170704c5f149', distributed).
narrative_ontology:cs_authority_grounding('fe1c421b-fe73-48c2-aa9b-170704c5f149', expertise).
narrative_ontology:cs_interpretation_layer_present('fe1c421b-fe73-48c2-aa9b-170704c5f149').
narrative_ontology:cs_reading_relation('fe1c421b-fe73-48c2-aa9b-170704c5f149', biblical_source_text__formal_equivalence_reading, influences).
narrative_ontology:cs_reading_relation('fe1c421b-fe73-48c2-aa9b-170704c5f149', biblical_source_text__dynamic_equivalence_reading, influences).
narrative_ontology:cs_axiom('fe1c421b-fe73-48c2-aa9b-170704c5f149', foundational, textual_basis_precedes_interpretation).
narrative_ontology:cs_axiom_status(textual_basis_precedes_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('fe1c421b-fe73-48c2-aa9b-170704c5f149', textual_basis_precedes_interpretation, instrumental).
narrative_ontology:cs_axiom('fe1c421b-fe73-48c2-aa9b-170704c5f149', foundational, documentary_evidence_primacy).
narrative_ontology:cs_axiom_status(documentary_evidence_primacy, holdable).
narrative_ontology:cs_axiom_grounding('fe1c421b-fe73-48c2-aa9b-170704c5f149', documentary_evidence_primacy, empirically_contingent).
narrative_ontology:cs_reference_frame('fe1c421b-fe73-48c2-aa9b-170704c5f149', hypothetical_autograph_recovery_standard).
narrative_ontology:cs_drift_state('fe1c421b-fe73-48c2-aa9b-170704c5f149', contemporary_multiform_text_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fe1c421b-fe73-48c2-aa9b-170704c5f149', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, manuscript_archive_institutions).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, received_text_confessional_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, lay_scripture_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, critical_edition_committees).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, lay_scripture_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, translation_committees_and_bible_societies).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, translation_committees_and_bible_societies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains in ancient languages, paleography, and transmission history; staffs university chairs, edits the flagship journals, and wins the grants that fund manuscript analysis and edition projects. Publication and advancement run through apparatus-building and variant adjudication; leaving the field means abandoning the expertise that constitutes the career.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, beneficiary,
    institutional, generational, identity_locked, global).

% Standing international committees that decide which manuscript evidence counts, adopt the adjudication method, and publish the hand-edition that most translation projects treat as their working source text. Members circulate through the same institutes; the committees set method policy and collect the editorial authority that follows.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, critical_edition_committees, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, critical_edition_committees, beneficiary).

% Denominations and study traditions whose teaching, liturgy, and apologetics bind wording to a received printed text handed down through their institutions. Each new critical edition revises verses they preach from; adopting the revised wording would mean conceding that the transmitted text their authority was staked on was provisional, so they hold the line at real institutional cost.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, received_text_confessional_communities, payer,
    organized, civilizational, identity_locked, global).

% Worship and study inside those communities; meet footnotes announcing that long-familiar verses are absent from the earliest manuscripts, and watch wording shift across successive editions. They carry the uncertainty with no seat in any committee, and their practical alternative is remaining within whatever materials their community produces.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, lay_scripture_readers, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, lay_scripture_readers, beneficiary).

% Produce the translations actually read worldwide. They must license and follow the critical editions, budget for philological consultants, and schedule revisions whenever the base text changes, while defending their choices both to scholarly reviewers and to confessional customers. They gain defensibility from the scholarly grounding and lose speed and margin to it.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, translation_committees_and_bible_societies, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, translation_committees_and_bible_societies, beneficiary).

% Libraries, museums, and monastic collections holding the physical witnesses. Scholarly demand funds conservation, digitization, and prestige; the holders control access terms, imaging fees, and publication permissions, and can steer cooperation toward whichever buyers pay most.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, manuscript_archive_institutions, beneficiary,
    powerful, civilizational, arbitrage, global).

% Movements in theology and homiletics that read the canon as a coherent whole and let interpretive judgment and textual judgment inform each other. Under the reconstruct-first ordering their contributions reach the flagship venues only after philology signs off, so they publish at the margins and press their case from there.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, meaning_first_interpretive_schools, excluded,
    moderate, generational, constrained, global).

% Study how textual scholarship acquired its standing, comparing manuscript cultures and editing traditions across centuries. They take testimony from every other seat, commission comparative histories, and carry no stake in which reading prevails.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, philological_method_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:fixing_cost_class(biblical_source_text__critical_reconstructive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared evidentiary baseline — which Hebrew and Greek words stood in the source documents — so that translation and interpretation disputes are adjudicated against common manuscript evidence rather than against whichever institutional authority speaks loudest.
% TRANSFER_FUNCTION: Moves epistemic authority, and the resources that follow it (publication economy, grant funding, seminary curricular control, edition licensing), away from confessional institutions holding received texts and toward the academic textual-criticism guild; also moves interpretive timing, deferring translation and doctrinal appeal until reconstruction advances.
% ABSENT_VOICES: Received-text confessional scholars and the lay faithful appear mainly as objects of study rather than co-adjudicators; meaning-first theologians (canonical criticism, theological interpretation of Scripture) are structurally deprioritized and would object that meaning-making and textual judgment are mutually informing rather than strictly sequential.
% DISAPPEARANCE_RATIONALE: If the reconstruct-first ordering vanished overnight, translation practice would revert to received-text baselines or pragmatic hybrids, the apparatus economy (journals, institutes, edition projects) would collapse, confessional communities would regain uncontested textual authority, and famously variant passages would revert to their traditional status without adjudication records.
% FOUNDING_PROBLEM: The manuscript record contains thousands of witnesses with hundreds of thousands of variant readings, and the early printed Greek text froze late-medieval Byzantine wording as if it were the universal original. Claims about what Scripture says rested on contested foundations; the discipline was built to recover, from documentary evidence, the text closest to what the authors wrote.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting set by the translation committees themselves (payers who nonetheless license the editions), by confessionally-aligned scholars who reject the method's authority while conceding the variant problem is real, and by the cross-tradition survival of disputed passages that every party acknowledges. No corroboration would exist only within the guild's own publications.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-to-high: the reading renders a real verified service (a common evidentiary baseline), but the apparatus economy rewards production independently of convergence, revision cycles are perpetual, and authority has concentrated in the guild that runs the method. Suppression (0.50) reflects mature gatekeeping — review venues, edition-method policy, accreditation, and funder expectations — while genuine alternatives persist (received-text translations, majority-text projects, meaning-first journals) at reduced standing, so suppression is substantial but not total. Theater ratio (0.32) is rising: core manuscript work remains functional, but apparatus maintenance beyond consultation volume, method debates detached from translational outcomes, and certainty rhetoric about 'the original' amid documented pluriformity add a growing performative share. Accessibility collapse (0.62): once the evidentiary picture is accepted, structure-first and meaning-first shortcuts collapse inside scholarly space, but they survive intact inside confessional space, capping the collapse below natural-law levels. Resistance (0.55): sustained confessional counter-movements, meaning-first pushback, and publisher pressure against endless revision keep enforcement costly. The temporal series run on one shared grid (1707–2026, seven points) with all three metrics authored at every point; the rising extraction trajectory tracks the discipline's consolidation from Mill's apparatus through Westcott-Hort, the Dead Sea Scrolls shock, the Aland-era institutes, and the Coherence-Based Genealogical Method.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify very differently. From the guild's seat the arrangement is a self-authored discipline it built, maintains, and believes in — coordination it experiences as its own professional substance. From the received-text confessional seat the same structure arrives as externally imposed instability: each new edition revises preached wording and demands surrender of the identity-bearing text, with exit priced at apostasy. Translation committees straddle: they pay for the apparatus and draw legitimacy from it. The excluded meaning-first schools experience the ordering as gatekeeping that sequences their contribution out of relevance. Coalition potential among the payer seats exists in principle (received-text communities span Protestant TR advocates, Byzantine-priority Orthodox, and Masoretic-fixed Jewish traditions), but identity boundaries between those traditions fragment any joint refusal, which is why organized power has not converted into effective exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: academic_biblical_scholarship and manuscript_archive_institutions sit near the beneficiary pole (d near 0.0), with the archives pushed further by arbitrage-grade exit and the guild held near the pole by identity-lock on the benefiting side. Victims sit near the target pole: received_text_confessional_communities combine victim declaration with identity-locked exit (amplifying d toward full-target despite organized power), and lay_scripture_readers add powerlessness and trapped exit. Translation_committees_and_bible_societies derive near-symmetric from their dual payer/beneficiary declaration. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and spatial scope in the engine's computation — the global scope of the discipline modestly amplifies effective extraction on distant, unverifiable-from-inside payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: variant readings persist, new witnesses still surface, and no party — including the payers — denies the evidentiary problem exists. Mandatrophy is therefore not resolved, and the classification guards against both failure modes: labeling the arrangement a pure rope would conceal the asymmetric extraction running through the same structure (who pays: received-text communities and their lay members; who collects: the guild and the archive holders), while labeling it a pure snare would erase the verification function that translation committees demonstrably rely on and that even hostile parties quietly consume. The tangled classification keeps the coordination function and the extraction asymmetry simultaneously visible, which is the analytic content this domain actually presents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the critical_reconstructive_reading of the kernel biblical_source_text; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Compile and compare the sibling stories (formal_equivalence_reading, dynamic_equivalence_reading): inspect their victim sets, gatekeeping structures, and epsilon values against this file''s.',
    'The formal_equivalence sibling would shrink the payer set (received-text communities become aligned suppliers rather than targets) and relocate gatekeeping to translation style; the dynamic_equivalence sibling would demote the textual-basis gate entirely, dissolving the academic beneficiary economy and moving the cost burden toward reader-accessibility tradeoffs. The disagreement is located in the adequacy criterion for the source text: historical reconstruction versus structural fidelity versus communicative effectiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-way contested kernel; sibling deltas and the locus of disagreement.').

omega_variable(
    original_text_attainability,
    'Is a determinate ''original text'' recoverable even approximately, or does the target dissolve into multiform early traditions (two literary editions of Jeremiah, an expanding Romans, pluriform Qumran scroll families)?',
    'Genealogical analysis of versional and Qumran evidence; quantitative distance estimates between the method''s ''initial text'' outputs and any defensible autograph concept.',
    'If the original is irrecoverable, the discipline''s demand side chases a partly constructed object and the theater share understates decay; if recoverable, the coordination function is solid and current epsilon reflects genuine service cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_text_attainability, empirical, 'Whether the reconstructive target exists as described or is partly produced by the method itself.').

omega_variable(
    academic_rent_vs_coordination,
    'Does academic biblical scholarship collect rents (careers rewarding apparatus production independently of convergence toward the original), or does it purely coordinate verification?',
    'Compare the rate of edition convergence against growth in publication volume and grant funding; test whether funding follows unresolved textual problems or apparatus maintenance for its own sake.',
    'Rent capture would shift classification toward the snare-flavored end of the tangled spectrum and strengthen the capture verdict on the receipt surface; pure coordination would support a lower effective extraction on all seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_rent_vs_coordination, empirical, 'Whether the beneficiary seat captures surplus or renders commensurate service.').

omega_variable(
    confessional_destabilization_cost,
    'Is the destabilization borne by received-text confessional communities an incidental byproduct of honest method, or a maintained condition that the apparatus economy depends on?',
    'Track whether the discipline invests in mitigation (stable popular editions, translation-continuity policies, accessible apparatus summaries) or benefits rhetorically and financially from perpetual revision necessity.',
    'A maintained-condition finding raises effective extraction on that seat and hardens the payer classification; an incidental-byproduct finding treats part of the measured burden as ordinary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_destabilization_cost, empirical, 'Byproduct versus maintained-extraction character of confessional destabilization.').

omega_variable(
    reconstructive_gate_permanence,
    'Is the ''no privileging of structure or meaning until textual basis is established'' gate transitional — dissolving once adjudication methods converge — or structurally permanent because new witnesses perpetually reset the gate?',
    'Observe whether edition committees ever declare stability milestones or retire the gate for settled corpora, or whether every methodological advance reopens adjudication.',
    'A transitional finding recasts the arrangement toward scaffold dynamics (a discipline that should sunset); permanence confirms indefinite enforcement and supports the tangled classification with a rising enforcement trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstructive_gate_permanence, conceptual, 'Transitional versus permanent character of the reconstruct-first ordering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 1707, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1707, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1707, 0.06).
narrative_ontology:measurement(bibl_tr_t1850, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1850, 0.11).
narrative_ontology:measurement(bibl_tr_t1881, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1881, 0.18).
narrative_ontology:measurement(bibl_tr_t1947, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1947, 0.21).
narrative_ontology:measurement(bibl_tr_t1981, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1981, 0.25).
narrative_ontology:measurement(bibl_tr_t2012, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2012, 0.3).
narrative_ontology:measurement(bibl_tr_t2026, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2026, 0.32).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1707, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1707, 0.2).
narrative_ontology:measurement(bibl_be_t1850, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1850, 0.3).
narrative_ontology:measurement(bibl_be_t1881, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1881, 0.44).
narrative_ontology:measurement(bibl_be_t1947, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1947, 0.5).
narrative_ontology:measurement(bibl_be_t1981, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1981, 0.54).
narrative_ontology:measurement(bibl_be_t2012, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2012, 0.57).
narrative_ontology:measurement(bibl_be_t2026, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1707, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1707, 0.14).
narrative_ontology:measurement(bibl_su_t1850, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1850, 0.22).
narrative_ontology:measurement(bibl_su_t1881, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1881, 0.36).
narrative_ontology:measurement(bibl_su_t1947, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1947, 0.41).
narrative_ontology:measurement(bibl_su_t1981, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1981, 0.46).
narrative_ontology:measurement(bibl_su_t2012, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2012, 0.49).
narrative_ontology:measurement(bibl_su_t2026, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2026, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, information_standard).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the biblical source text question' conflates three structurally distinct arrangements. This file authors the critical-reconstructive reading alone (epsilon ~0.58; victims: received-text confessional communities and lay readers; beneficiaries: academic scholarship and archive holders). The formal-equivalence sibling carries a different victim set (intelligibility burdens on readers) and the dynamic-equivalence sibling carries extraction aimed at source-text precision itself. The three files are linked pairwise through affects_constraints; this reading is the upstream member because its adjudicated base text is the input the other two operate on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
