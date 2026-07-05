% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Study of Kodashim as Cosmic-Sustaining Performance (Talmudic Substitution Reading)
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kodashim_obligation kernel:
 *   that studying the laws of sacrifice (Kodashim, the Mishnaic/Talmudic
 *   order dealing with the Temple cult) constitutes, in itself, the
 *   cosmic/spiritual act the sacrifices once performed — not a placeholder
 *   for a future restored performance, not mere historical documentation, but
 *   the performance's structural equivalent. Under this reading the Temple's
 *   physical absence is irrelevant to efficacy: the claim of full
 *   substitution (grounded in classical Talmudic statements like the Hosea
 *   14:3 gloss) closes the gap between destroyed institution and continuing
 *   cosmic function entirely through study. This is deliberately a narrow,
 *   ε-invariant claim — it does NOT describe the preparationist reading
 *   (binding-but-unperformable-law-awaiting-restoration) or the archivist
 *   reading (defunct-system-as-heritage), which are separate constraints
 *   (study_as_preparation, study_as_archive) linked via
 *   network.affects_constraints, not folded into this one's classification.
 *
 * KEY AGENTS:
 *   - study_community: Primary beneficiary and agenda-setter (organized/mobile) — the practice constitutes rather than extracts
 *   - cosmic_order: Named non-agent beneficiary — the sustained function itself, not a collecting party
 *   - sibling_reading_preparationists: Excluded voice, different premise, same corpus
 *   - sibling_reading_archivists: Excluded voice, denies the cosmic-efficacy claim outright
 *   - textual_transmission_chain: Analytical observer — the lineage that authorized and carried the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.03).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.08).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Study of Kodashim as Cosmic-Sustaining Performance (Talmudic Substitution Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious_studies/jewish_law/textual_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, '0f13f190-a409-41c7-870c-d854406b3b5b').
narrative_ontology:cs_kernel_codification('0f13f190-a409-41c7-870c-d854406b3b5b', fixed_text).
narrative_ontology:cs_authority_grounding('0f13f190-a409-41c7-870c-d854406b3b5b', lineage).
narrative_ontology:cs_interpretation_layer_present('0f13f190-a409-41c7-870c-d854406b3b5b').
narrative_ontology:cs_reading_relation('0f13f190-a409-41c7-870c-d854406b3b5b', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('0f13f190-a409-41c7-870c-d854406b3b5b', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('0f13f190-a409-41c7-870c-d854406b3b5b', foundational, study_fully_substitutes_for_sacrifice).
narrative_ontology:cs_axiom_status(study_fully_substitutes_for_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('0f13f190-a409-41c7-870c-d854406b3b5b', study_fully_substitutes_for_sacrifice, theological).
narrative_ontology:cs_axiom('0f13f190-a409-41c7-870c-d854406b3b5b', secondary, temple_restoration_not_structurally_necessary_for_efficacy).
narrative_ontology:cs_axiom_status(temple_restoration_not_structurally_necessary_for_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('0f13f190-a409-41c7-870c-d854406b3b5b', temple_restoration_not_structurally_necessary_for_efficacy, theological).
narrative_ontology:cs_reference_frame('0f13f190-a409-41c7-870c-d854406b3b5b', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('0f13f190-a409-41c7-870c-d854406b3b5b', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0f13f190-a409-41c7-870c-d854406b3b5b', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, study_community).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, torah_study_substitutes_for_sacrifice).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, divine_service_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshiva students, scholars, and daily-cycle learners (Daf Yomi participants studying Zevachim and Menachot on schedule) treat sustained analysis of sacrificial law as itself efficacious — a completed spiritual act, not a rehearsal for a future act. They set the interpretive terms of what counts as adequate study and derive standing, meaning, and communal identity from the practice regardless of whether a Temple exists. Nothing is extracted from them; the practice is chosen and its 'cost' (time, intellectual labor) is constitutive of the benefit, not a payment exacted by someone else.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, study_community, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_performance, study_community, agenda_setter).

% Named non-agent beneficiary: the maintained cosmic/covenantal order the sacrificial system was said to sustain (atonement, communion between Israel and the divine, the ongoing structure of creation). Under this reading, study transmits the sustaining function directly — it is not an agent that could be a victim or a payer, but it is the thing the practice is FOR.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmic_order).

% Adherents of the study_as_preparation reading hold that the law remains binding-but-unperformable and study preserves technical knowledge for a future restored Temple. They are not victims of this constraint, but they are excluded from THIS constraint's framing: from this reading's seat, their emphasis on messianic restoration as the telos of study is a different constraint entirely, coexisting in the same textual tradition but resting on a different premise about what makes the study efficacious now.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, sibling_reading_preparationists, excluded,
    organized, generational, mobile, global).

% Adherents of the study_as_archive reading (often academically-inflected, identity/heritage-oriented communities) hold that Kodashim documents a defunct system and study is historical-preservation, not cosmic or legal performance. From this reading's seat they are also excluded from the performance-claim: their account denies the very cosmic efficacy this reading asserts, though both readings can be held by different parties without contradiction in the broader tradition.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, sibling_reading_archivists, excluded,
    moderate, generational, mobile, national).

% The rabbinic transmission lineage (Mishnah, Talmud, later commentators like Rashi, Tosafot, and the Meiri on the substitutionary reading of Hosea 14:3 and its Talmudic elaborations) that carried and authorized this reading across centuries without a functioning Temple. This lineage adjudicates what counts as valid study and interpretation but does not itself collect anything from the practice.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, textual_transmission_chain, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transmissible practice through which a dispersed community sustains a claimed cosmic/covenantal function (atonement, divine service continuity) without dependence on a physical Temple, priesthood, or animal sacrifice — solving the problem of continuity after catastrophic loss of the institution the law originally governed.
% TRANSFER_FUNCTION: Nothing is transferred from one party to another under this reading. Time and intellectual effort are expended by the studying community, but the product (interpretive completion, spiritual efficacy) accrues to the same community and to the cosmic order the practice is said to sustain — there is no extraction path from payer to beneficiary because performer and beneficiary largely coincide.
% ABSENT_VOICES: The sibling readings (study_as_preparation, study_as_archive) are not victims and would not describe themselves as harmed by this reading; they simply hold a different premise about what the study accomplishes. Practitioners for whom the Temple's absence registers as unresolved loss (mourning-centered observance, e.g. Tisha B'Av liturgical traditions) are not directly represented among the stakeholders here, though their felt absence is in tension with this reading's claim of full efficacy without restoration.
% DISAPPEARANCE_RATIONALE: If this specific reading (study-as-performance) disappeared, the practice of studying Kodashim would very likely continue under one of the sibling readings — the textual corpus, the study communities, and the transmission chain are shared infrastructure across all three readings. What would change is the STATED justification and phenomenology of the act: study would revert to preparation-for-restoration or archival-preservation framing. Whether 'the world rearranges' is contested precisely because the kernel (the obligation to study Kodashim) is invariant across readings; only the reading's account of what the study accomplishes disappears.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the sacrificial system that Kodashim governs could no longer be physically performed, threatening both the practical continuity of an entire legal corpus and the theological claim that atonement/divine service continues. This reading answers: the founding problem is solved permanently and completely by substituting study for performance — there is no residual gap.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources themselves attest the substitution claim (e.g., the reading of Hosea 14:3, 'let our lips substitute for bullocks,' and statements in Taanit and Menachot that one who studies sacrificial law is deemed to have brought the sacrifice). This corroboration comes from within the tradition, not from a fully external party — no source outside the interpreting communities themselves is cited for the cosmic-efficacy claim, which is itself the site of dispute: preparationist and archivist readings, drawing on the same corpus, deny that the founding problem is fully resolved by study alone. Stated plainly: no genuinely external corroboration of the cosmic-efficacy claim exists; all corroboration is internal to traditions that already hold some stake in one reading or another.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).
:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.03) because under this reading performer and beneficiary substantially coincide — the study community's effort constitutes the benefit rather than being extracted for someone else's benefit. Suppression is low (0.08): no one is coerced into believing the substitution claim, and dissenting readings persist openly within the same tradition. Theater ratio is low (0.1): the practice is not performative window-dressing over an atrophied function, because under this reading there is no separate 'real' function it is failing to perform — the study IS the function. Accessibility collapse is moderate (0.35), reflecting that alternative interpretive framings (preparation, archive) remain fully available and openly held by coexisting communities; nothing about this reading forecloses access to the others. Resistance is low (0.15): the claim is a live, comfortable position within traditional Judaism, not one requiring active defense against dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no victim set under this reading by design (per the expected structural delta) — the study community bears the 'cost' of sustained intellectual labor, but that cost is constitutive of the benefit they receive (spiritual efficacy, communal meaning, cosmic participation), not a transfer to a separate extracting party. Cosmic_order is listed as a non-agent beneficiary to name what the practice is FOR without implying it collects rents in any directionality-relevant sense — it is excluded from the beneficiary/victim derivation's agent-only path by the agent:false flag.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves what would otherwise look like mandatrophy (a law persisting long after its founding institution's destruction) by relocating the founding problem's solution from institutional performance to textual performance — the mandate is not stale, on this reading, because it was never actually about the physical Temple in the first place; the Temple was the historically contingent vehicle for a function study now carries directly. Whether this is genuine non-mandatrophy or a sophisticated cover story for an obligation that has, in the preparationist and archivist readings' view, actually gone dead, is exactly the kernel-level dispute this story deliberately does not resolve — that dispute is routed to omega variables rather than settled here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_claim_scope,
    'Does the Talmudic substitution language (e.g., the Hosea 14:3 gloss, statements in Menachot/Taanit) assert full cosmic-functional equivalence between study and sacrifice, or a lesser form of merit/reward that later tradition inflated into full substitution?',
    'Close textual-historical analysis of the earliest attestations of the substitution claim versus later amoraic and medieval elaborations; comparison with parallel rabbinic statements about prayer as sacrifice-substitute (tefillah kein negged tamidim).',
    'If the earliest sources intended only a lesser reward-claim, this reading''s zero-extractiveness/no-restoration-necessary structure would be a later theological overreach rather than the tradition''s own original position — shifting this constraint''s claimed_type toward something requiring reconciliation with the preparationist reading rather than standing as an independent, equally-original reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_claim_scope, empirical, 'Whether full substitutionary efficacy is original to the sources or a later elaboration.').

omega_variable(
    reading_selection_is_committer_choice,
    'Given that all three kernel readings (performance, preparation, archive) draw on the identical textual corpus and transmission chain, what determines which reading a given community or scholar adopts — theological commitment, denominational affiliation, or something else?',
    'Sociological/historical survey of which communities hold which reading and why (e.g., correlation with messianic theology commitments, denominational stance on Temple restoration, academic vs. traditional institutional setting).',
    'If reading-selection tracks prior theological commitments rather than independent textual argument, all three kernel readings are committer-relative rather than textually adjudicable — reinforcing that this is genuinely a kernel-contest requiring decomposition into separate stories, not a single constraint with an ambiguous ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_is_committer_choice, conceptual, 'Whether the three readings are textually adjudicable or purely committer-relative.').

omega_variable(
    cosmic_order_beneficiary_coherence,
    'Does naming ''cosmic_order'' as a non-agent beneficiary meaningfully describe a real structural relationship, or is it a way of asserting the practice''s value without being falsifiable by any observer?',
    'None available in principle — this is a theological claim not subject to empirical resolution; the question is whether the framework''s agent:false mechanism correctly brackets it out of directionality computation without smuggling in an unfalsifiable beneficiary that inflates the practice''s legitimacy.',
    'If the bracketing is inadequate, this reading could function as an unfalsifiable self-certifying loop (study benefits cosmic order; cosmic order''s benefit is unverifiable; therefore study is always beneficial) — which would itself be worth flagging as a structural feature of the reading rather than a bug in this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_order_beneficiary_coherence, conceptual, 'Whether a non-agent cosmic beneficiary is coherent or an unfalsifiable legitimation device.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(koda_tr_t0, projected).
narrative_ontology:measurement(koda_tr_t300, kodashim_obligation__study_as_performance, theater_ratio, 300, 0.12).
narrative_ontology:measurement_basis(koda_tr_t300, projected).
narrative_ontology:measurement(koda_tr_t700, kodashim_obligation__study_as_performance, theater_ratio, 700, 0.11).
narrative_ontology:measurement_basis(koda_tr_t700, projected).
narrative_ontology:measurement(koda_tr_t1200, kodashim_obligation__study_as_performance, theater_ratio, 1200, 0.1).
narrative_ontology:measurement_basis(koda_tr_t1200, projected).
narrative_ontology:measurement(koda_tr_t1650, kodashim_obligation__study_as_performance, theater_ratio, 1650, 0.1).
narrative_ontology:measurement_basis(koda_tr_t1650, observed).
narrative_ontology:measurement(koda_tr_t1950, kodashim_obligation__study_as_performance, theater_ratio, 1950, 0.1).
narrative_ontology:measurement_basis(koda_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(koda_be_t0, projected).
narrative_ontology:measurement(koda_be_t300, kodashim_obligation__study_as_performance, base_extractiveness, 300, 0.04).
narrative_ontology:measurement_basis(koda_be_t300, projected).
narrative_ontology:measurement(koda_be_t700, kodashim_obligation__study_as_performance, base_extractiveness, 700, 0.04).
narrative_ontology:measurement_basis(koda_be_t700, projected).
narrative_ontology:measurement(koda_be_t1200, kodashim_obligation__study_as_performance, base_extractiveness, 1200, 0.03).
narrative_ontology:measurement_basis(koda_be_t1200, projected).
narrative_ontology:measurement(koda_be_t1650, kodashim_obligation__study_as_performance, base_extractiveness, 1650, 0.03).
narrative_ontology:measurement_basis(koda_be_t1650, observed).
narrative_ontology:measurement(koda_be_t1950, kodashim_obligation__study_as_performance, base_extractiveness, 1950, 0.03).
narrative_ontology:measurement_basis(koda_be_t1950, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_performance, 0.06).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'the obligation to study Kodashim' into structurally distinct constraints per the ε-invariance principle. study_as_performance (this story) claims near-zero extraction and no victim set, because performer and beneficiary coincide and the cosmic function is asserted as fully realized through study alone. study_as_preparation (sibling) would carry a different structure: an unresolved obligation awaiting future performance, plausibly with higher accessibility_collapse (alternatives to waiting are foreclosed) and a live tension between binding law and impossibility of compliance. study_as_archive (sibling) would carry yet another structure: no cosmic claim at all, extraction near zero but for different reasons (heritage practice, not performance-substitution), and a different beneficiary set (identity/cultural continuity rather than cosmic order). All three share the same textual corpus and transmission chain but diverge sharply on what problem is being solved and whether it remains open.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
