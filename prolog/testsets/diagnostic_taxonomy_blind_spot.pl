% ============================================================================
% CONSTRAINT STORY: diagnostic_taxonomy_blind_spot
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_diagnostic_taxonomy_blind_spot, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: diagnostic_taxonomy_blind_spot
 *   human_readable: Categorical Blind Spot for Caregiver-as-Vector in Diagnostic Frameworks
 *   domain: social epistemology / domestic power / diagnostic systems
 *
 * SUMMARY:
 *   Marfa Osipovna is in slow, unexplained decline while under the care of
 *   Foma Silovich, who administers her food and daily sustenance. Three
 *   independent diagnostic systems observe her case: the folk empirical
 *   pattern-memory of the neighbor women, who have seen the same grey
 *   nail-beds recur across unrelated households; the formal clinical taxonomy
 *   of the district physician, who notes the illness will not plateau under
 *   standard treatment; and the parish's communal moral accounting, which
 *   regards Foma as a devoted, self-sacrificing caregiver. Each system is
 *   applied rigorously within its own terms. None of them contains a category
 *   for 'the person administering care is the source of harm' — so the
 *   confirming evidence each system independently generates (recurrence
 *   across households, a refractory case trajectory, an absence of any
 *   alternative explanation) is filed as coincidence, medical mystery, or
 *   irrelevant to reputation, rather than escalated into suspicion of Foma.
 *   This constraint is downstream of victim_self_attribution_foreclosure:
 *   Marfa's own inability to form and voice the self-attributed suspicion
 *   that her caregiver is harming her compounds the diagnostic blind spot
 *   from the other direction — even if she suspected, no vocabulary exists
 *   for her to be heard making the claim.
 *
 * KEY AGENTS:
 *   - foma_silovich: primary beneficiary (moderate/mobile) — insulated from suspicion by categorical absence across all observing frameworks
 *   - marfa_osipovna: primary victim (powerless/trapped) — bears the harm, cannot be diagnosed correctly by any available system
 *   - neighbor_women: folk-pattern observers (moderate/constrained) — generate confirming evidence, file it as fate
 *   - district_physician: clinical observer (institutional/analytical) — generates confirming evidence, files it as refractory illness
 *   - parish_moral_accounting: communal observer (organized/constrained) — holds reputation ledger with no conversion path from 'devoted' to 'suspect'
 *   - analytical_observer: sees the shared structural gap across all three frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(diagnostic_taxonomy_blind_spot, 0.81).
domain_priors:suppression_score(diagnostic_taxonomy_blind_spot, 0.72).
domain_priors:theater_ratio(diagnostic_taxonomy_blind_spot, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(diagnostic_taxonomy_blind_spot, extractiveness, 0.81).
narrative_ontology:constraint_metric(diagnostic_taxonomy_blind_spot, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(diagnostic_taxonomy_blind_spot, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(diagnostic_taxonomy_blind_spot, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(diagnostic_taxonomy_blind_spot, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(diagnostic_taxonomy_blind_spot, snare).
narrative_ontology:human_readable(diagnostic_taxonomy_blind_spot, "Categorical Blind Spot for Caregiver-as-Vector in Diagnostic Frameworks").
narrative_ontology:topic_domain(diagnostic_taxonomy_blind_spot, "social epistemology / domestic power / diagnostic systems").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(diagnostic_taxonomy_blind_spot, foma_silovich).
narrative_ontology:constraint_victim(diagnostic_taxonomy_blind_spot, marfa_osipovna).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers care and sustenance to Marfa within the household. Every diagnostic vocabulary in play — the neighbor women's folk pattern-memory, the district physician's clinical taxonomy, the parish's moral accounting of who is a good or bad kin — has no slot for 'the one who feeds and tends is the source of the harm.' Because none of the observing frameworks can even formulate the hypothesis, no one ever escalates suspicion toward him, regardless of what evidence accumulates. He does not need to suppress anyone; the absence of the category does the work for him.
narrative_ontology:constraint_stakeholder(diagnostic_taxonomy_blind_spot, foma_silovich, beneficiary,
    moderate, biographical, mobile, local).

% Suffers a slow, non-plateauing decline — grey nail-beds, a case pattern that has appeared in other households under similar caregiving arrangements — that would be recognizable as poisoning or deliberate harm if any observer's diagnostic system contained a caregiver-as-vector category. She is physically dependent on the person harming her for food and basic care, and has no independent channel to have the pattern read correctly, since the same blind spot exists in every institution she could appeal to.
narrative_ontology:constraint_stakeholder(diagnostic_taxonomy_blind_spot, marfa_osipovna, payer,
    powerless, immediate, trapped, local).

% Hold rich folk empirical pattern-memory — they have seen grey nail-beds before, in other households, and could in principle recognize the recurrence. But their pattern library files these repetitions as regional misfortune, bad water, or ill-luck households, because their taxonomy has categories for illness and fate but not for domestic administration of poison by a caregiver. They generate the confirming evidence and then discard its significance.
narrative_ontology:constraint_stakeholder(diagnostic_taxonomy_blind_spot, neighbor_women, observer,
    moderate, generational, constrained, local).

% Applies formal clinical taxonomy rigorously: symptoms are logged, differentials considered, treatments prescribed. The taxonomy has no diagnostic code for intra-household caregiver-administered harm as a live differential in a case like Marfa's — it sorts cases into disease categories, not household-power categories. He notes the case 'will not plateau' under treatment and records it as a puzzling refractory illness rather than escalating suspicion of the household.
narrative_ontology:constraint_stakeholder(diagnostic_taxonomy_blind_spot, district_physician, observer,
    institutional, biographical, analytical, regional).

% Tracks who is a dutiful spouse, a good provider, a faithful caregiver — a communal ledger of moral reputation. Foma is locally regarded as attentive and sacrificing, having taken on Marfa's care. This ledger has categories for negligence and cruelty in the abstract but does not know how to convert 'devoted caregiver' into 'suspect' without new evidence the ledger itself is not built to solicit or weigh.
narrative_ontology:constraint_stakeholder(diagnostic_taxonomy_blind_spot, parish_moral_accounting, observer,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(diagnostic_taxonomy_blind_spot, parish_moral_accounting, excluded).

% Sees that all three diagnostic frameworks are internally rigorous and simultaneously structurally incapable of the one classification that would matter here. Notes that the pattern-matching evidence (recurring grey nail-beds across unrelated households, a refractory case) is actually generated by these systems but never escalated, because escalation requires a category none of them possess.
narrative_ontology:constraint_stakeholder(diagnostic_taxonomy_blind_spot, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(diagnostic_taxonomy_blind_spot, foma_silovich).
narrative_ontology:fixing_cost_class(diagnostic_taxonomy_blind_spot, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Each framework solves a real diagnostic problem within its domain: folk memory tracks regional health patterns, clinical taxonomy standardizes treatment of disease, moral accounting sustains community trust in caregiving relationships. None of these functions is fraudulent on its own terms.
% TRANSFER_FUNCTION: Moves suspicion and scrutiny away from the person administering care and toward impersonal categories (fate, disease, reputation), which in effect transfers protective attention away from the victim and insulates the caregiver from ever being investigated.
% ABSENT_VOICES: Marfa herself has no standing to name her own caregiver as the cause without a framework that can hear the claim; any protoype accusation she might make is filterable back into 'fate' or 'illness' by the very listeners she would need to convince. The upstream constraint (victim_self_attribution_foreclosure) further forecloses her from even forming the self-attribution that would let her voice the suspicion.
% DISAPPEARANCE_RATIONALE: If a caregiver-as-vector category existed and were live in even one of these frameworks, the same evidence already being generated (recurring grey nail-beds, refractory case trajectory) would trigger escalation — investigation, intervention, or at minimum suspicion voiced aloud. The world would rearrange around a named suspect rather than an unexplained illness.
% FOUNDING_PROBLEM: Each diagnostic system was built to solve a real classification problem in its domain — folk memory for regional health hazards, clinical taxonomy for disease treatment, moral accounting for communal trust in caregiving — none of which anticipated domestic caregiver-administered harm as the failure mode needing coverage.
% FOUNDING_PROBLEM_CORROBORATION: No party inside the three diagnostic communities corroborates the gap, since by construction none of them can name the missing category from within their own vocabulary. The analytical observer, standing outside all three frameworks, is the only seat from which the blind spot itself becomes visible and namable; no institutional or communal authority attests to it independently.
narrative_ontology:disappearance_verdict(diagnostic_taxonomy_blind_spot, world_rearranges).
narrative_ontology:founding_problem_status(diagnostic_taxonomy_blind_spot, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(diagnostic_taxonomy_blind_spot, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-17',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(diagnostic_taxonomy_blind_spot, 'none', 1).
narrative_ontology:epsilon_provenance(diagnostic_taxonomy_blind_spot, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(diagnostic_taxonomy_blind_spot_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(diagnostic_taxonomy_blind_spot, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(diagnostic_taxonomy_blind_spot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) and rising because the harm to Marfa compounds over time while remaining unaddressed — the constraint's persistence is measured by the accumulating, unescalated evidence trail, not by any single diagnostic failure. Suppression (0.72) is high but distinct from active coercion: no one is silencing anyone; the suppression is the structural incapacity of the available categories to represent the true hypothesis, which functions as suppression without an active suppressor. Theater ratio is moderate (0.4) and rising, reflecting that all three frameworks continue performing rigorous diagnostic activity (folk consultation, clinical examination, moral评估) that looks like functioning oversight while never converging on the relevant category — activity substitutes for detection. Resistance is low (0.28) because there is no organized force pushing back against the blind spot; it is not contested, it is simply unthought.
 *
 * PERSPECTIVAL GAP:
 *   From Foma's seat, nothing needs defending — no framework ever generates suspicion toward him, so there is no extraction to conceal and no enforcement to maintain; the snare operates entirely through absence, not vigilance. From Marfa's seat, the same structure is a total foreclosure: every institution she could appeal to independently confirms the pattern (grey nail-beds, non-plateauing illness) yet independently discards its significance, because the missing category is missing identically in all three places. The observer seats (neighbor women, physician, parish) each experience their own diagnostic work as sound and complete within their domain — the gap is invisible from inside any single framework and only visible from the analytical seat that can compare all three.
 *
 * DIRECTIONALITY LOGIC:
 *   Foma is coded near the full-beneficiary end: he administers the care that is the vector of harm, and every diagnostic system's blind spot means the extraction is never traced back to him — he need take no suppressive action to maintain this, which is precisely how a blind-spot snare differs from an actively enforced one. Marfa is coded near the full-target end: trapped by physical dependency on her caregiver and further isolated by every social system meant to protect her failing to name the correct hypothesis. The observer stakeholders are coded closer to symmetric/analytical — they neither benefit from nor are targeted by the arrangement, but their rigorous-yet-blind operation is the mechanism by which the extraction persists undetected.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not classified as a Mountain despite each framework's internal rigor and apparent naturalness — the beneficiary declaration (foma_silovich) and victim declaration (marfa_osipovna) block that reading and force the engine to weigh the structural asymmetry: someone benefits from a categorical absence that someone else is dying inside of. It is also not classified as innocent coordination failure (rope), because the absence is not randomly distributed — it happens to fall exactly where it protects the person administering care from ever being investigated, across three independently constructed vocabularies. Naming it snare rather than tangled_rope reflects that no genuine coordination function is being extracted through: the folk-memory, clinical, and moral systems are not coordinating anyone in relation to Foma's caregiving at all — they simply cannot see him. There is no active enforcement to maintain the blind spot (a tangled_rope requirement), only the structural incapacity of the shared vocabulary — hence snare rather than tangled_rope, distinguishing this from the upstream victim_self_attribution_foreclosure, which does involve active internalized enforcement of Marfa's inability to accuse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_absence_vs_active_concealment,
    'Is the missing caregiver-as-vector category a genuine, unintentional gap in each framework''s conceptual vocabulary, or has Foma (or the surrounding social structure) actively cultivated conditions that keep the category from forming or being invoked?',
    'Historical comparison: does the same gap appear in diagnostic frameworks with no domestic-abuse-suppressing social incentive at all (control case), or is the gap specifically correlated with cases where a caregiver benefits from non-detection?',
    'If the gap is a universal, incentive-independent feature of these diagnostic vocabularies, the classification leans toward a piton-adjacent structural failure that happens to be exploitable. If the gap is asymmetrically maintained or exploited by beneficiaries like Foma, the snare classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_absence_vs_active_concealment, conceptual, 'Whether the diagnostic blind spot is a neutral structural gap or an actively exploited one.').

omega_variable(
    escalation_threshold_location,
    'How much confirming evidence would each framework need to accumulate before some observer, even without a formal caregiver-as-vector category, independently reasons their way to suspicion through informal inference?',
    'Track historical cases where informal suspicion eventually broke through despite categorical absence — what triggered the break, and how much unescalated evidence preceded it?',
    'A low threshold suggests the blind spot is fragile and likely to be interrupted eventually; a high or absent threshold suggests the blind spot is durable and self-reinforcing, strengthening the snare classification and lowering any expectation of self-correction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_threshold_location, empirical, 'How durable the categorical blind spot is against accumulating informal evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(diagnostic_taxonomy_blind_spot, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(diag_tr_t0, diagnostic_taxonomy_blind_spot, theater_ratio, 0, 0.25).
narrative_ontology:measurement(diag_tr_t4, diagnostic_taxonomy_blind_spot, theater_ratio, 4, 0.28).
narrative_ontology:measurement(diag_tr_t8, diagnostic_taxonomy_blind_spot, theater_ratio, 8, 0.31).
narrative_ontology:measurement(diag_tr_t12, diagnostic_taxonomy_blind_spot, theater_ratio, 12, 0.34).
narrative_ontology:measurement(diag_tr_t16, diagnostic_taxonomy_blind_spot, theater_ratio, 16, 0.36).
narrative_ontology:measurement(diag_tr_t20, diagnostic_taxonomy_blind_spot, theater_ratio, 20, 0.38).
narrative_ontology:measurement(diag_tr_t24, diagnostic_taxonomy_blind_spot, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(diag_be_t0, diagnostic_taxonomy_blind_spot, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(diag_be_t4, diagnostic_taxonomy_blind_spot, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(diag_be_t8, diagnostic_taxonomy_blind_spot, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(diag_be_t12, diagnostic_taxonomy_blind_spot, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(diag_be_t16, diagnostic_taxonomy_blind_spot, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(diag_be_t20, diagnostic_taxonomy_blind_spot, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(diag_be_t24, diagnostic_taxonomy_blind_spot, base_extractiveness, 24, 0.81).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(diagnostic_taxonomy_blind_spot, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(diagnostic_taxonomy_blind_spot, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of victim_self_attribution_foreclosure (tangled_rope): that constraint forecloses Marfa's own capacity to form and voice the self-attributed suspicion that her caregiver is the source of harm, which compounds the external diagnostic blind spot modeled here. The two constraints share the same beneficiary (foma_silovich) and victim (marfa_osipovna) but describe distinct structural mechanisms — one operates on Marfa's internal cognitive/moral vocabulary (upstream), the other on the external diagnostic vocabularies of her community and institutions (this story) — and are linked rather than merged per the ε-invariance principle, since they have different ε referents (internalized self-blame apparatus vs. external categorical incapacity) and different victim-facing mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
