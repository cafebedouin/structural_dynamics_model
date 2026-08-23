% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Dual-Register Catastrophe-Memory Ritual (Hybrid Encoding Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Communities in hazard-prone regions maintain ritual calendars that bind
 *   two registers into single performances: a symbolic register
 *   (boundary-marking, identity, obligation) and a practical register
 *   (evacuation timing, routes, storage discipline, kin protocol). This file
 *   instantiates the hybrid_encoding_reading of the kernel
 *   catastrophe_memory_survival: both registers are operative, and survival
 *   depends on both — which is why the arrangement cannot be decomposed
 *   without damaging transmission. The epsilon referent is the standing
 *   arrangement under contest — living communities maintaining integrated
 *   catastrophe-memory practice — assessed by this reading's own lights,
 *   under which the arrangement is substantially functional and minimally
 *   extractive; it is NOT authored for the decomposed arrangement that
 *   sibling readings or modernizing agencies would produce. KEY AGENTS (by
 *   structural relationship): catastrophe_memory_communities — primary
 *   beneficiary (organized/identity_locked), maintains both registers and
 *   collects memory persistence plus coordinated hazard response;
 *   ritual_specialist_lineages — agenda-setter and beneficiary
 *   (moderate/identity_locked), administer the calendar and transmit the
 *   integrated practice; binary_classification_analysts — primary cost-bearer
 *   (institutional/mobile), whose single-register instruments systematically
 *   fail against the structure; modernizing_state_agencies — excluded seat
 *   (institutional/arbitrage), redesigning from outside the conversation;
 *   youth_generation_participants — beneficiary with payer secondary
 *   (moderate/constrained); heritage_market_intermediaries — late-arriving
 *   beneficiary (moderate/arbitrage) capturing the commodification skim;
 *   longitudinal_field_ethnographers — analytical observer holding the
 *   comparative record.
 *
 * KEY AGENTS:
 *   - catastrophe_memory_communities: primary beneficiary (organized/identity_locked) — maintains both registers; collects memory persistence and coordinated hazard response
 *   - ritual_specialist_lineages: agenda_setter and beneficiary (moderate/identity_locked) — administer the calendar, transmit integrated practice, absorb drift through interpretation
 *   - binary_classification_analysts: primary cost-bearer (institutional/mobile) — single-register coding schemes and intervention designs systematically fail
 *   - modernizing_state_agencies: excluded seat (institutional/arbitrage) — inventory, document, and design replacements without sitting in community deliberation
 *   - youth_generation_participants: beneficiary with payer secondary (moderate/constrained) — bear participation costs now, collect embedded knowledge only at next recurrence
 *   - heritage_market_intermediaries: late-arriving beneficiary (moderate/arbitrage) — capture the growing commodification skim without carrying obligations
 *   - longitudinal_field_ethnographers: analytical observer — sees the full dual-register structure and the outcomes of decomposition attempts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Dual-Register Catastrophe-Memory Ritual (Hybrid Encoding Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '48bca4da-872a-4b8d-b2d5-e28468d78169').
narrative_ontology:cs_kernel_codification('48bca4da-872a-4b8d-b2d5-e28468d78169', distributed).
narrative_ontology:cs_authority_grounding('48bca4da-872a-4b8d-b2d5-e28468d78169', practice).
narrative_ontology:cs_interpretation_layer_present('48bca4da-872a-4b8d-b2d5-e28468d78169').
narrative_ontology:cs_reading_relation('48bca4da-872a-4b8d-b2d5-e28468d78169', catastrophe_memory_survival__symbol_survival_reading, forecloses).
narrative_ontology:cs_reading_relation('48bca4da-872a-4b8d-b2d5-e28468d78169', catastrophe_memory_survival__competence_transmission_reading, forecloses).
narrative_ontology:cs_axiom('48bca4da-872a-4b8d-b2d5-e28468d78169', foundational, register_separation_destroys_both).
narrative_ontology:cs_axiom_status(register_separation_destroys_both, holdable).
narrative_ontology:cs_axiom_grounding('48bca4da-872a-4b8d-b2d5-e28468d78169', register_separation_destroys_both, empirically_contingent).
narrative_ontology:cs_axiom('48bca4da-872a-4b8d-b2d5-e28468d78169', foundational, unresolved_duality_sustains_transmission).
narrative_ontology:cs_axiom_status(unresolved_duality_sustains_transmission, holdable).
narrative_ontology:cs_axiom_grounding('48bca4da-872a-4b8d-b2d5-e28468d78169', unresolved_duality_sustains_transmission, instrumental).
narrative_ontology:cs_reference_frame('48bca4da-872a-4b8d-b2d5-e28468d78169', integrated_dual_register_practice).
narrative_ontology:cs_drift_state('48bca4da-872a-4b8d-b2d5-e28468d78169', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48bca4da-872a-4b8d-b2d5-e28468d78169', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, ritual_specialist_lineages).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, youth_generation_participants).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, heritage_market_intermediaries).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, binary_classification_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, youth_generation_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in hazard-prone regions where floods, tsunamis, or droughts recur on intervals longer than any member's memory. They perform the ritual calendar together — processions, fasts, chants, first-fruits offerings — and in doing so rehearse where to go, when to move, what to store, and who owes whom. The practice is fused with their mutual-aid network and recognized identity; leaving it was never experienced as a separately available choice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Elder lineages who keep the calendar, teach the chants, decide adaptations after near-misses or bad seasons, and control what may be shown to outsiders. Their standing rests on demonstrated transmission; they receive deference and modest material support and carry the burden of continuity. The role cannot be handed to someone raised outside the practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_specialist_lineages, agenda_setter,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, ritual_specialist_lineages, beneficiary).

% Academic and applied professionals — folklorists, secularization theorists, program evaluators — who sort each practice into a single category: belief-symbol or technique. Their coding schemes return unstable labels for the same rite across decades, intervention designs built on one register underperform in the field, and review processes penalize the ambiguity they are unable to resolve. They can switch frameworks or fields, and some do.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, binary_classification_analysts, payer,
    institutional, biographical, mobile, global).

% Disaster-management directorates, education ministries, and heritage boards that inventory rituals, fund documentation projects, and design replacement systems — evacuation applications, memorial days, safety curricula. They act on the practices without sitting in the community's deliberations; their consultations reach appointed culture officers, not the lineages that hold the integrated knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, modernizing_state_agencies, excluded,
    institutional, biographical, arbitrage, national).

% Younger members who attend because parents and peers do. Participation costs them time and sometimes schooling or wage work; the payoff — knowing why the chant says run uphill — arrives only when a hazard recurs. Some migrate to cities and thin their participation; those who remain inherit the full practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, youth_generation_participants, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, youth_generation_participants, payer).

% Festival promoters, tour operators, and content producers who arrived in recent decades. They stage performances for visitors, pay appearance fees, and favor the photogenic surface of the rites. They take revenue from the practice without carrying its obligations, and their demand shapes which elements get emphasized.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, heritage_market_intermediaries, beneficiary,
    moderate, immediate, arbitrage, continental).

% Researchers who have followed particular communities across decades, before and after decomposition attempts. They hold the comparative record — which registers were lost, what happened in the next hazard — and publish outside the advocacy of any seat.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, longitudinal_field_ethnographers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, heritage_market_intermediaries).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Carries community-scale memory across recurrence gaps longer than any member's lifespan: the symbolic register keeps the practice motivationally alive and marks who belongs, while the practical register embeds hazard-specific protocol — evacuation timing, high-ground routes, seed and water discipline, kin obligations — inside the same performances, so that neither register survives the other's loss.
% TRANSFER_FUNCTION: Moves encoded protocol and identity commitment from elder to younger generations through participatory performance; moves interpretive authority and deference to specialist lineages; and, when outside institutions engage, moves decontextualized fragments — a chant text, a festival date — into archives, curricula, and tourism products.
% ABSENT_VOICES: Tradition-bearing elders who insist the two registers cannot be separated are rarely seated on the heritage boards, school committees, and disaster-planning bodies that classify and redesign the practices; diaspora members who lost the practical register and experienced what that cost are likewise absent from the classification literature that treats symbolism as the whole story.
% DISAPPEARANCE_RATIONALE: If the integrated practices vanished overnight, communities would lose the motivational container and the embedded protocol simultaneously: hazard responses calibrated to local timing and terrain would lapse until expensively reinvented, kin-coordination obligations would need new enforcement mechanisms, and identity boundaries would blur within a generation or two. The recorded post-decomposition cases show exactly this rearrangement in slow motion; overnight disappearance would compress it.
% FOUNDING_PROBLEM: Catastrophes recur on intervals longer than individual memory: a flood or tsunami that strikes every few generations arrives to a population that no longer remembers why the old rules existed. The arrangement was built — or selected — to bind survival-critical knowledge to practices people would keep performing for reasons of identity and meaning, so the knowledge outlives the memory of why it matters.
% FOUNDING_PROBLEM_CORROBORATION: Hazard historians and disaster ethnographers outside the benefiting parties corroborate both the problem and the mechanism: the Simeulue smong chants (knowledge of the 1907 earthquake and tsunami transmitted orally, credited with near-zero adult casualties in the 2004 event), documented alignments between ritual calendars and flood or drought seasons in agrarian societies, and post-disaster comparisons showing communities with intact ritual protocols responding faster than neighboring communities relying solely on official channels. No corroborating source outside the arrangement attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because participation costs approximate genuine coordination costs and almost nothing transfers out of the community; the small late-interval rise tracks the heritage-market skim, not internal rent-setting. Suppression (0.30) reflects residual social sanction around non-participation, and the suppression_requirement series is authored deliberately rather than left static because the story's enforcement history is real: compulsion machinery (community discipline, clerical sanction) decayed steadily across the interval as participation became voluntary — a falling trajectory, distinct from the scalar's job of describing the current level. Theater_ratio is low (0.12) because under this reading the symbolic register is functional, not ornamental: performance and function coincide; the modest rise tracks staged performances shedding embedded content for visitors. Accessibility_collapse (0.38) is rope-appropriate: written manuals, state memorials, and safety curricula remain workable alternatives and are actively attempted — they simply underperform. Resistance (0.35) comes from two directions: modernizers pushing substitution, and analysts pushing classification, both meeting community defense of integration. All three metric series run on one shared seven-point grid (t=0..60 by decades) so no metric row is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   The community seat and the analyst seat should compute differently from identical structural data. From inside the community, the arrangement is life-infrastructure: the same performance that marks belonging rehearses the route to high ground. From the analyst seat, the identical structure presents as an object that defeats classification — codes flip, categories leak, interventions underperform — so the constraint reads as an obstacle to knowledge rather than as coordination. The state-agency seat sees replaceable custom; the intermediary seat sees product; the lineage seat sees an inheritance it does not fully control. The engine computes these per-seat divergences from power, exit, and directional position; this story authors the structure and refuses to adjudicate the seats by fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   Communities and specialist lineages sit near the beneficiary pole: the arrangement subsidizes them with memory persistence, mutual-aid coherence, and interpretive authority, and their identity-lock amplifies the subsidy side of the computation. Youth participants sit near symmetric — costs now, payoff deferred to a hazard recurrence they may not personally witness. Binary_classification_analysts are declared victims and the derivation places them at high directionality, correctly marking them as cost-bearers; but what they bear is epistemic defeat, not transferred goods — no wealth, labor, or attention flows from them into the arrangement — which is why low base epsilon keeps their effective extraction modest even at high d. Heritage_market_intermediaries derive low d as incidental beneficiaries while nonetheless capturing the growing measurable skim; the receipt surface names them accordingly, and the commentary flags the asymmetry between benefit-position and receipt-position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: hazards still recur beyond individual memory spans, and the corroborating record shows the mechanism working. Nothing here has atrophied into performance-without-function, so no mandatrophy resolution is declared. The classification risk runs in the opposite direction from the usual case: because the arrangement resists analysis and consists largely of performance, a careless reading could score theater high and read the whole structure as hollow ritual, or score the analyst victim set as evidence of extraction. Scoring theater low (the performance IS the function), extraction low (little transfers out), and recording the analyst cost-bearers separately lets the corpus distinguish 'defeats binary analysis' from 'extracts from participants' — the former is this constraint, the latter it is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This story is one reading of the kernel catastrophe_memory_survival — the hybrid_encoding_reading, which holds that ritual operates on dual registers (symbolic boundary-maintenance AND embedded practical knowledge) with survival depending on both. Which register(s) actually carry the survival value, and is the disagreement located in the necessity/sufficiency structure of the two registers?',
    'Natural-experiment comparison across communities where one register was lost or deliberately decomposed (chants archived but calendar abandoned, or protocol retained but performance secularized): track hazard-response outcomes and transmission continuity in the next recurrence interval.',
    'Adopting a sibling reading restructures this constraint entirely: under symbol_survival_reading the practical register is dispensable and decomposition becomes rational; under competence_transmission_reading the symbolic register is cover. Either way the analyst victim set dissolves, the beneficiary set narrows to one register''s carriers, and epsilon and type change per the sibling files — never averaged into this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: this constraint is the hybrid reading of a three-way kernel contest; the dispute lives in the joint-necessity claim.').

omega_variable(
    emergence_vs_maintenance,
    'Is the dual-register structure an emergent product of cultural selection on transmission fidelity (a regularity of how memory-bearing practices evolve under catastrophe pressure), or a maintained coordination achievement that deliberate redesign could in principle reproduce?',
    'Cross-cultural comparative database of independent post-catastrophe ritual innovations: if dual-register forms repeatedly self-organize without central design, the structure is selection-grade; if they appear only where specialist lineages actively maintain both registers, it is maintained.',
    'If emergent, the constraint trends toward natural-law treatment and decomposition attempts are predictably futile; if maintained, well-designed substitutes could in principle replicate the function, changing the fixing-cost assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_vs_maintenance, conceptual, 'Whether the dual-register structure is selected regularity or engineered coordination.').

omega_variable(
    commodification_extraction_trajectory,
    'Does the late-interval rise in theater_ratio and base_extractiveness indicate rent-seeking layered onto a functioning arrangement (performance value skimmed by outside intermediaries while embedded content thins), or benign adaptation of the symbolic register to new economic conditions?',
    'Track whether commodified performances retain practical-register content (route drills, seasonal timing, kin obligations) or shed it as staging intensifies; compare hazard-response indicators in heavily commodified versus protected communities.',
    'Continued shedding supports an extraction-accumulation reading and eventual drift toward a hybrid coordination/extraction profile; content retention supports treating the rise as adaptive evolution with negligible structural consequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commodification_extraction_trajectory, empirical, 'Whether heritage commodification is extraction accumulation or adaptation.').

omega_variable(
    case_generalizability,
    'Do the celebrated corroborations (Simeulue smong transmission, ritual-calendar/hazard-season alignments) generalize across hazard-prone regions, or are they survivorship-selected exceptions?',
    'Systematic survey of ritual practice versus reconstructed hazard chronology across independent hazard-prone regions, controlling for publication bias toward dramatic cases.',
    'Weak generalization would shrink the vindicated proposition set, strengthen the analyst-seat position that dual-register claims rest on cherry-picked cases, and raise the effective weight of the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(case_generalizability, empirical, 'Generalizability of ritual-hazard transmission corroborations beyond flagship cases.').

omega_variable(
    framing_underdetermination,
    'Is the kernel the lived commitment of practicing communities (authority grounded in practice, with specialist lineages as interpreters), or the scholarly controversy about ritual function (authority distributed across competing academic readings)? Two coherent framings assign the kernel different authorities and different victim sets.',
    'Ask whose commitments persist independent of the debate: the arrangement predates and outlives the scholarly contest, and communities — not disputing scholars — maintain the practice, which guided the choice of the practice-grounded framing here.',
    'Adopting the scholarly-controversy framing would reassign authority_grounding to distributed, invalidate the interpretation-layer declaration, and relocate the victim set from binary-classifying analysts to the disputing scholars themselves — a materially different constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'CS framing under-determination: lived-practice kernel versus scholarly-controversy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cms_hybrid_encoding_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(cms_hybrid_encoding_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(cms_hybrid_encoding_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(cms_hybrid_encoding_tr_t30, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(cms_hybrid_encoding_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cms_hybrid_encoding_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.11).
narrative_ontology:measurement(cms_hybrid_encoding_tr_t60, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(cms_hybrid_encoding_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(cms_hybrid_encoding_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(cms_hybrid_encoding_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(cms_hybrid_encoding_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(cms_hybrid_encoding_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(cms_hybrid_encoding_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(cms_hybrid_encoding_be_t60, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 60, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cms_hybrid_encoding_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cms_hybrid_encoding_su_t10, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(cms_hybrid_encoding_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(cms_hybrid_encoding_su_t30, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(cms_hybrid_encoding_su_t40, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(cms_hybrid_encoding_su_t50, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 50, 0.33).
narrative_ontology:measurement(cms_hybrid_encoding_su_t60, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 60, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'ritual preserves catastrophe memory' decomposes into three structurally distinct readings of one kernel. This file instantiates the hybrid_encoding_reading (both registers operative and jointly necessary; low epsilon; the standing integrated arrangement is functional). The sibling files instantiate symbol_survival_reading (symbolic continuity as the operative mechanism) and competence_transmission_reading (practical-knowledge transmission as the operative mechanism). Epsilon differs across the family because the referent differs: each reading assesses the standing arrangement by its own lights, and each licenses a different decomposition of practice. The hybrid reading is upstream of both siblings in one sense — its joint-necessity premise is what the single-register readings deny — and downstream in another: single-register research programs supply the classification pressure this reading's victim set bears. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
