% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: Synchronic/Diachronic Seam Test for IP Category Emergence (1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the third reading of the ip_category_emergence
 *   kernel: the synchronic_diachronic_seam test. Rather than asserting that
 *   1710 marks category emergence (thinkability_reading) or occupancy change
 *   (first_holding_reading), this reading tests whether those two claims are
 *   FORMALLY INDEPENDENT phenomena that merely coincided in 1710, or whether
 *   the apparent distinction is a temporal-framing artifact — an analytical
 *   illusion produced by describing one event through two different lenses
 *   (M4/M5 collapse test). The test itself has become a small institution: a
 *   diagnostic move cited by historians who want to know if the kernel has
 *   authentic internal structure, and used strategically by camps with a
 *   stake in the answer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.31).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.22).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.31).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Synchronic/Diachronic Seam Test for IP Category Emergence (1710)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '5a60b37a-c15e-4c29-bd8a-346966f9329c').
narrative_ontology:cs_kernel_codification('5a60b37a-c15e-4c29-bd8a-346966f9329c', distributed).
narrative_ontology:cs_authority_grounding('5a60b37a-c15e-4c29-bd8a-346966f9329c', distributed).
narrative_ontology:cs_reading_relation('5a60b37a-c15e-4c29-bd8a-346966f9329c', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('5a60b37a-c15e-4c29-bd8a-346966f9329c', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('5a60b37a-c15e-4c29-bd8a-346966f9329c', foundational, category_emergence_and_occupancy_change_are_severable_events).
narrative_ontology:cs_axiom_status(category_emergence_and_occupancy_change_are_severable_events, holdable).
narrative_ontology:cs_axiom_grounding('5a60b37a-c15e-4c29-bd8a-346966f9329c', category_emergence_and_occupancy_change_are_severable_events, empirically_contingent).
narrative_ontology:cs_axiom('5a60b37a-c15e-4c29-bd8a-346966f9329c', secondary, temporal_co_occurrence_does_not_entail_conceptual_identity).
narrative_ontology:cs_axiom_status(temporal_co_occurrence_does_not_entail_conceptual_identity, holdable).
narrative_ontology:cs_axiom_grounding('5a60b37a-c15e-4c29-bd8a-346966f9329c', temporal_co_occurrence_does_not_entail_conceptual_identity, conventional).
narrative_ontology:cs_created_at('5a60b37a-c15e-4c29-bd8a-346966f9329c', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, doctrinal_historians_favoring_unified_account).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, statute_of_anne_legal_apparatus).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, scholars_of_rival_decomposed_readings).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, litigants_relying_on_single_coherent_1710_narrative).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, litigants_relying_on_single_coherent_1710_narrative).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, kernel_structural_authenticity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The 1710 statute and its interpretive apparatus function as the fixed textual anchor that both the thinkability and first-holding readings cite as their origin point. The apparatus itself takes no position on whether category emergence and occupancy change are independent events or a single collapsed event; it simply persists as the shared reference that makes the seam question askable at all.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, statute_of_anne_legal_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).

% Historians who treat 1710 as a single clean origin event benefit from the seam collapsing to 'temporal framing artifact' — a unified narrative is easier to teach, cite, and build doctrine on than two formally independent events that happened to coincide. If the collapse test resolves toward independence, their unified account fractures into two harder-to-narrate claims.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, doctrinal_historians_favoring_unified_account, beneficiary,
    organized, generational, constrained, national).

% Scholars invested in the thinkability_reading or the first_holding_reading as SEPARATE, independently defensible claims bear the cost if the seam test resolves toward 'temporal framing artifact' — their distinct readings would be exposed as one event described twice, collapsing two research programs into one and devaluing work built on treating them as independent.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, scholars_of_rival_decomposed_readings, payer,
    moderate, biographical, constrained, national).

% Parties in IP disputes who cite 1710 as settled doctrinal ground benefit when courts treat category-emergence and occupancy-change as bundled (simpler precedent), but pay a cost in unpredictability whenever a court or scholar reopens the seam question and the outcome of a case turns on which of the two claims was actually operative.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, litigants_relying_on_single_coherent_1710_narrative, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, litigants_relying_on_single_coherent_1710_narrative, beneficiary).

% The broader field of legal historiography observes the collapse test as a methodological probe: does treating 'IP emerged in 1710' as one event versus two change any downstream doctrinal or historical claim? The field's interest is in whether the kernel has authentic internal structure or is an artifact of how legal historians happen to narrate origin moments.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_historiography_field, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, doctrinal_historians_favoring_unified_account).
narrative_ontology:fixing_cost_class(ip_category_emergence__synchronic_diachronic_seam, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The seam test coordinates scholarly and doctrinal attention onto a single diagnostic question — can category emergence (thinkability) and occupancy change (first-holding) be shown to vary independently, or do they always co-occur such that distinguishing them is a temporal-framing artifact rather than a real structural fact about 1710?
% TRANSFER_FUNCTION: Moves interpretive authority between two camps: if independence is established, credibility and citation weight shift toward scholars who decomposed the kernel into two claims; if collapse is established, weight shifts back toward the unified-account historians and the statute's own apparatus, which prefers a single clean origin story to two entangled ones.
% ABSENT_VOICES: The historical drafters and early claimants of 1710 rights have no voice in this test at all — the seam question is entirely a modern analytical construct applied retrospectively; no contemporary 18th-century source addresses whether these were formally independent or fused events, so any resolution is necessarily an inference, not a recovered fact.
% DISAPPEARANCE_RATIONALE: If the seam test vanished, the thinkability_reading and first_holding_reading would continue to be asserted independently by their respective camps, but the question of whether the kernel is AUTHENTIC (has real internal structure) or SPURIOUS (an artifact of narrative convenience) would go unexamined — doctrinal historians would experience this as no loss, while methodologically-minded historiographers would say a live diagnostic question has been abandoned rather than resolved.
% FOUNDING_PROBLEM: Legal historians noticed that 'IP began in 1710' is asserted in two structurally different ways — as a claim about a NEW CATEGORY becoming legally thinkable, and as a claim about a NEW CLAIMANT CLASS entering existing legitimate-holder status — and needed a way to test whether these were actually the same event described twice or genuinely separable phenomena.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by comparative legal historians working on other jurisdictions' IP origin moments (e.g. continental droit d'auteur formation), who independently flag the same synchronic/diachronic ambiguity when category-formation and rights-holder-formation dates coincide; no single camp inside the 1710 dispute is the sole source of the diagnostic question.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, contested).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).
:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31) and has risen slowly since 1710 as the seam question became a professionalized methodological tool rather than a neutral diagnostic — using the collapse test to defend or attack a rival camp's reading has real career and citation stakes. Suppression is low (0.22): no one is coercively prevented from arguing either side, though the unified-account camp's institutional weight makes the independence position harder to publish and cite. Theater ratio rose from 0.10 to 0.40 as the test increasingly functions as performative methodological rigor — invoked to signal sophistication — without always changing anyone's substantive doctrinal position. Accessibility collapse is moderate (0.45): the seam question remains genuinely contestable and alternative framings of 1710's meaning persist. Resistance is moderately high (0.55) because scholars with stakes in either decomposed reading actively resist collapse toward the unified account, and vice versa.
 *
 * DIRECTIONALITY LOGIC:
 *   The statute's own interpretive apparatus is the structural agenda-setter — it is the fixed text both camps argue over, but takes no position on the seam question itself. Doctrinal historians favoring the unified account are the nearest thing to a beneficiary: a collapsed, single-event narrative is more citable, more teachable, and easier to build precedent on, so a 'temporal framing artifact' verdict accrues to them. Scholars of the rival decomposed readings and litigants who built arguments on a stable single narrative both pay when the seam is reopened and re-litigated, because reopening the question destabilizes settled doctrinal shorthand regardless of which way it resolves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — is 1710 one event or two? — remains genuinely live rather than resolved-but-persisting, because the seam question keeps being asked afresh by each generation of legal historians confronting the same coincidence of category-formation and occupancy-change dates. This is not a mandatrophied constraint (a mandate that outlived its function); it is closer to an active methodological seam that periodically gets re-litigated as new comparative material (e.g. continental droit d'auteur) makes the question newly salient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_authenticity_vs_framing_artifact,
    'Is the apparent two-part structure of the ip_category_emergence kernel (thinkability + first-holding) an authentic feature of 1710 — two formally independent events that happened to coincide — or is it a temporal-framing artifact produced by describing one collapsed historical event through two different analytical lenses?',
    'Comparative analysis across jurisdictions where category-formation and rights-holder-formation dates diverge (e.g. cases where a category became legally thinkable well before any claimant class was recognized, or vice versa). If such divergence cases exist and are structurally comparable to 1710, independence is supported; if the two always co-occur across all studied jurisdictions, artifact is supported.',
    'If independence is established, thinkability_reading and first_holding_reading are validated as genuinely separable claims and the kernel has authentic internal structure. If artifact is established, both sibling readings collapse into restatements of one event, and the doctrinal weight currently split between two research programs should consolidate into one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_authenticity_vs_framing_artifact, conceptual, 'Whether the M4/M5 seam reflects real independent structure or a narrative-framing illusion.').

omega_variable(
    who_adjudicates_the_seam,
    'Is there any authority positioned to adjudicate the seam question definitively, or does the test remain permanently open because no comparative dataset can fully settle whether category-emergence and occupancy-change are conceptually separable versus historically inseparable at 1710 specifically?',
    'Track whether future comparative legal-historical scholarship converges on a consensus answer, or whether the question remains a live methodological dispute indefinitely (a signal itself, if so).',
    'A converging consensus would resolve the kernel''s structure; permanent non-convergence would itself be evidence that the seam question is underdetermined by available historical evidence, which changes how much doctrinal weight either sibling reading should be given.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_adjudicates_the_seam, empirical, 'Whether the seam question is empirically resolvable or structurally underdetermined.').

omega_variable(
    reading_framing_choice_ambiguity,
    'Given that the seam test itself could be framed either as a purely analytical/philosophical exercise (testing conceptual independence in the abstract) or as a historically-grounded claim (testing whether 1710 specifically instantiates independence or collapse), does the choice of framing change which sibling reading this constraint is read as supporting?',
    'Compare classification outcomes under an abstract-conceptual framing (treats the test as pure logic, independent of any specific historical claim) versus a historically-grounded framing (treats the test as an empirical claim about what actually happened in 1710). Document whether cs_pattern or beneficiary structure shifts.',
    'Under the abstract framing, no camp benefits structurally — the test is neutral logic. Under the historically-grounded framing, the unified-account historians retain a modest structural advantage because ambiguity defaults toward the simpler, already-dominant narrative. This story adopts the historically-grounded framing because the kernel context specifies a concrete 1710 event, not an abstract logical puzzle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_choice_ambiguity, conceptual, 'Alternative framings of the seam test (abstract-logical vs. historically-grounded) yield different beneficiary structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 1710, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(ip_c_tr_t1850, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(ip_c_tr_t1950, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(ip_c_tr_t1990, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(ip_c_tr_t2010, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(ip_c_tr_t2024, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1710, 0.15).
narrative_ontology:measurement(ip_c_be_t1850, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1850, 0.18).
narrative_ontology:measurement(ip_c_be_t1950, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(ip_c_be_t1990, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1990, 0.27).
narrative_ontology:measurement(ip_c_be_t2010, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(ip_c_be_t2024, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 2024, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ip_category_emergence__synchronic_diachronic_seam, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, first_holding_reading).

% DUAL FORMULATION NOTE:
% This constraint is the third member of the ip_category_emergence kernel triplet. thinkability_reading and first_holding_reading each assert a distinct substantive historical claim about 1710; this constraint (synchronic_diachronic_seam) is the second-order diagnostic that tests whether those two claims are genuinely independent or a temporal-framing artifact of a single collapsed event. It does not compete with either sibling for the same claim-space — it evaluates the relationship between them. A resolution here would restructure how much doctrinal and scholarly weight each sibling reading should carry, hence 'influences' rather than 'forecloses' or 'coexists_with' in both directions: this reading does not rule out either sibling's substantive claim, nor does it merely sit alongside them unrelated — it creates structural pressure on both by testing the authenticity of the distinction they each depend on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
