% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Script Reform as Literacy/Efficiency Instrument (Instrumentalist Reading)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   This story instantiates the instrumentalist reading of the orthographic
 *   legitimacy kernel: a state's script reform is justified as legitimate
 *   because and only because it raises literacy rates and lowers
 *   administrative training cost. Under this reading, script choice carries
 *   no civilizational or religious meaning — it is a technology to be
 *   optimized against measurable outcomes. The reform's early years require
 *   heavy enforcement (mandatory curricula, civil service script
 *   requirements) because the scribal elite's incumbency is real; as the
 *   newly-literate generation ages into the workforce, enforcement need
 *   declines because the new script becomes the default lived reality rather
 *   than an imposed one. This is a single ε-invariant claim about ONE
 *   reading; the modernist_reading (Western-alignment/rupture justification)
 *   and continuity_reading (preserving religious/literary access) are
 *   separate constraints with their own ε values, not alternate lenses on
 *   this one.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — mandates and enforces reform, gains cheaper-to-train bureaucracy
 *   - newly_literate_population: beneficiary (powerless/trapped) — gains literacy access previously foreclosed by orthographic complexity
 *   - arabic_literate_scribal_elite: payer (moderate/constrained) — sunk training investment devalued
 *   - religious_education_establishment: payer (organized/constrained) — loses administrative-literacy monopoly, retains religious authority
 *   - literacy_statisticians: observer (analytical/analytical) — measures the outcome the reading's legitimacy standard depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.55).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Script Reform as Literacy/Efficiency Instrument (Instrumentalist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, '8e79b32b-acd3-4aac-adb7-5600c3aa9be6').
narrative_ontology:cs_kernel_codification('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', distributed).
narrative_ontology:cs_authority_grounding('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', distributed).
narrative_ontology:cs_reading_relation('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', orthographic_legitimacy_kernel__continuity_reading, influences).
narrative_ontology:cs_axiom('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', foundational, legitimacy_grounded_in_measurable_outcomes).
narrative_ontology:cs_axiom_status(legitimacy_grounded_in_measurable_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', legitimacy_grounded_in_measurable_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', foundational, script_is_neutral_instrument_not_identity_marker).
narrative_ontology:cs_axiom_status(script_is_neutral_instrument_not_identity_marker, holdable).
narrative_ontology:cs_axiom_grounding('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', script_is_neutral_instrument_not_identity_marker, instrumental).
narrative_ontology:cs_reference_frame('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', pre_reform_scribal_literacy_bottleneck).
narrative_ontology:cs_drift_state('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', contemporary_post_reform_plateau, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8e79b32b-acd3-4aac-adb7-5600c3aa9be6', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_scribal_elite).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, religious_education_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the new phonetic script for all official documents, schooling, and print, justifying the change by citing literacy rate targets and reduced training time for clerks and teachers. Gains a more easily trained bureaucratic cadre and lower administrative overhead; enforces the change through school curricula and civil service requirements.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, beneficiary).

% Rural and working-class populations who could not master the old orthography's irregular consonantal spelling now acquire functional literacy in a fraction of the time under the new phonetic system. They have no realistic capacity to demand the old script's return; the new system is simply the one available to them.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    powerless, biographical, trapped, national).

% Clerks, notaries, and calligraphers whose decades of training in the old script become unmarketable almost overnight. Some retrain in the new system at a competitive disadvantage against younger literates; others lose administrative employment entirely. Their exit options are limited to retraining, emigration to jurisdictions that retained the old script, or exit from clerical work altogether.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_scribal_elite, payer,
    moderate, biographical, constrained, national).

% Institutions teaching scripture and jurisprudence through the old script lose their monopoly on literacy transmission as state schools become the primary literacy pathway. They retain religious authority but lose the administrative/legal literacy function that once anchored their institutional relevance. Under this reading their loss is treated as an efficiency externality, not as a targeted attack on tradition.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, religious_education_establishment, payer,
    organized, generational, constrained, national).

% Measure literacy rates before and after the reform and report the efficiency gains that justify the reform under this reading's own terms. Their reports are cited by the state as vindication and contested by the scribal elite as methodologically incomplete (ignoring transitional costs and cultural loss, which this reading treats as out of scope).
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, literacy_statisticians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, phonetically regular script that can be taught quickly solves the genuine problem of mass illiteracy and slow bureaucratic training under an orthography with high learning overhead.
% TRANSFER_FUNCTION: Moves administrative and educational literacy capital from a small trained scribal class to a large newly-literate population, and moves training-time cost savings to the state administrative apparatus; the scribal elite's sunk investment in the old script is transferred to a devalued asset with no compensation.
% ABSENT_VOICES: The scribal elite and religious educators raise cultural-continuity and identity objections, but under this reading those objections are treated as illegitimate inputs to a legitimacy standard defined purely by literacy statistics and administrative cost — their objections are heard but structurally excluded from the standard of justification itself.
% DISAPPEARANCE_RATIONALE: If the instrumentalist standard were dropped, literacy programs and administrative training would likely persist in some form, but the specific legitimacy grounds for having replaced the old script would collapse — the state would need a new justification (continuity or modernity) for a change already made, or would face renewed pressure to reverse or hybridize the script. Whether the world 'rearranges' depends on whether the underlying literacy gains are attributed to the script change itself or to concurrent schooling expansion, which is exactly what is contested between this reading and its siblings.
% FOUNDING_PROBLEM: Mass illiteracy and slow bureaucratic training under an orthography whose spelling did not track pronunciation reliably, which the state needed solved quickly to build a modern administrative and educated citizenry.
% FOUNDING_PROBLEM_CORROBORATION: Independent literacy statisticians and international literacy-comparison bodies attest that measured literacy rates rose substantially post-reform, corroborating the founding problem's resolution from outside the state apparatus that benefits from the claim. The scribal elite and religious educators, from outside the beneficiary group, dispute that literacy gains are attributable to the script change alone rather than concurrent compulsory schooling expansion — so corroboration exists but is contested on causal attribution, not on the raw statistics.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, contested).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and falls initially as the coordination benefit (mass literacy) materializes, then ticks back up slightly by T=40 reflecting recognition that the scribal elite's losses were never compensated and administrative efficiency gains partly reflect suppressed alternatives rather than pure Pareto improvement. Suppression starts high (0.75) — early enforcement against continued old-script use in official contexts is substantial — and declines as adoption becomes self-sustaining through generational turnover, consistent with a genuine (if imperfect) coordination function rather than pure extraction requiring permanent coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat this looks like coordination succeeding on its own declared terms (literacy statistics). From the scribal elite's seat the same enforcement machinery looks like an uncompensated taking of professional capital dressed in efficiency language. The engine should compute these divergently from the same structural facts; this reading does not adjudicate which seat is 'right' about the reform's meaning — only the modernist and continuity readings dispute meaning; this reading disputes only whether the literacy gains justify the transitional costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The state administrative apparatus and newly literate population sit toward the beneficiary end: the state gains cheaper training and a larger literate tax/conscription/civil-service base, ordinary people gain literacy access previously gated by orthographic complexity. The scribal elite and religious education establishment sit toward the target end: their embodied capital (years of training in the old system) is devalued by state fiat, and their exit options are genuinely constrained (retrain at a disadvantage, emigrate, or exit the trade). This is the rope-like reading precisely because the coordination function (literacy) is real and the extraction (devaluing the old elite's capital) is a side effect of solving that problem, not the point of the reform.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass illiteracy, slow bureaucratic training) is contested as live vs. dead: the state's administrative literacy problem was substantially and durably solved, but the reform's ongoing legitimacy claim increasingly rests on continued invocation of that historical achievement rather than a live, currently-measured crisis. This risks calcifying into a piton-adjacent legitimacy claim if the state continues to justify script maintenance by literacy statistics that have long since plateaued rather than by any live coordination problem — the six_questions founding_problem_status of 'contested' flags this without resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentalist_vs_modernist_motivation,
    'Was the historical reform''s actual driving justification instrumentalist (literacy/efficiency) or modernist (civilizational rupture), with the instrumentalist framing serving as post-hoc technical cover for a rupture already decided on identity grounds?',
    'Archival analysis of contemporaneous state deliberation records, legislative debate transcripts, and internal administrative correspondence to determine which justificatory register was actually operative in the decision, versus which was used in public communication.',
    'If modernist motivation was primary and instrumentalist framing was cover, this story''s ε and beneficiary structure would need to be re-derived under the modernist_reading instead, and the moderate rope-like ε authored here would understate the extraction hidden behind the efficiency narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalist_vs_modernist_motivation, conceptual, 'Whether the instrumentalist justification was the real operative logic or a technical gloss over a modernist/civilizational decision.').

omega_variable(
    literacy_attribution_causality,
    'Are the literacy gains cited to justify this reading actually caused by the script change, or by concurrent factors (compulsory schooling expansion, print technology diffusion, urbanization) that would have raised literacy under either script?',
    'Comparative analysis against jurisdictions that expanded schooling without changing script, controlling for schooling-rate and urbanization trends to isolate the script-change effect.',
    'If literacy gains are substantially attributable to concurrent schooling expansion rather than the script change itself, the instrumentalist reading''s core justificatory claim weakens considerably, raising effective extractiveness since the scribal elite''s losses would no longer be offset by a genuine coordination gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_attribution_causality, empirical, 'Whether measured literacy gains are causally attributable to the orthographic reform itself.').

omega_variable(
    scribal_elite_compensation_absence,
    'Does the complete absence of compensation or transition support for the displaced scribal elite indicate that the reform''s coordination function was pursued negligently, or that the reading treats their loss as categorically outside the legitimacy calculus by design?',
    'Review of contemporaneous policy proposals to determine whether compensation mechanisms were considered and rejected, versus never contemplated.',
    'If compensation was considered and deliberately rejected, this pushes the constraint''s actual operation toward tangled_rope (coordination function real, but asymmetric extraction actively chosen rather than incidental) rather than a clean rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scribal_elite_compensation_absence, empirical, 'Whether the scribal elite''s uncompensated loss was negligent oversight or a designed feature of the reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(orth_tr_t8, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(orth_tr_t16, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(orth_tr_t24, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(orth_tr_t32, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(orth_be_t8, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(orth_be_t16, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(orth_be_t24, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(orth_be_t32, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(orth_su_t8, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(orth_su_t16, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(orth_su_t24, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(orth_su_t32, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'orthographic legitimacy' per the ε-invariance principle. The instrumentalist_reading (this file) authors moderate ε (~0.42, rope-like) grounded in literacy/efficiency outcomes. The modernist_reading authors a different ε grounded in civilizational-rupture justification (expected higher suppression/extraction given identity-based exclusion of the old script's association with the prior civilizational order). The continuity_reading authors yet another ε grounded in preservation of religious/literary access (expected to treat any script change imposing efficiency costs on tradition-access as extractive). All three share the same underlying historical reform event but diverge because each reading contests different aspects of the same kernel using different legitimacy standards. Linked bidirectionally; each sibling file documents the same decomposition in its own narrative_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
