% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Article 4 NDCs — Voluntary Sovereigntist Reading
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 architecture — nationally determined
 *   contributions filed on a five-year cycle, reviewed through a transparency
 *   framework, aggregated by a global stocktake, and backed by no
 *   penalty-bearing compliance mechanism — is presented here through the
 *   sovereigntist reading: NDCs are voluntary, self-determined pledges that
 *   preserve national energy sovereignty. This is ONE reading of the
 *   paris_article_4_ndc kernel; the supranational and equity readings are
 *   separate constraints with their own epsilon values, authored in their own
 *   files and linked through the network. The epsilon referent throughout is
 *   the standing voluntary arrangement itself, assessed by this reading's own
 *   lights — not the binding regime the supranational sibling would build.
 *   The claim/metric gap is deliberate: the reading CLAIMS a low-extraction
 *   coordination rope, while the authored metrics record growing announcement
 *   theater (0.30 to 0.45) and slowly accumulating burden asymmetry — the
 *   engine measures that divergence rather than the author reconciling it.
 *
 * KEY AGENTS:
 *   - fossil_dependent_exporters: Primary beneficiary (organized/constrained) — preserves export pathways, blocks bindingness in negotiation
 *   - major_emerging_economies: Secondary beneficiary (powerful/mobile) — retains development policy space behind intensity targets
 *   - industrialized_states: Dual-positioned beneficiary-payer (institutional/mobile) — reputational gains at low legal risk, pays finance expectations
 *   - climate_vulnerable_nations: Primary payer (organized/trapped) — bears the exposure externality of everyone else's sovereignty plus disproportionate reporting burden
 *   - unfccc_secretariat: Administrator (institutional/constrained) — runs the machinery, binds nothing, captures no rent
 *   - climate_advocacy_coalitions: Excluded voice (organized/trapped) — present in every room, vote in none
 *   - ipcc_assessment_community: Analytical observer (institutional/analytical) — supplies the yardstick the stocktake consumes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.24).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.14).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Article 4 NDCs — Voluntary Sovereigntist Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance/treaty_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, 'e4cdbca5-c999-46fc-b70a-d5c701835ae4').
narrative_ontology:cs_kernel_codification('e4cdbca5-c999-46fc-b70a-d5c701835ae4', fixed_text).
narrative_ontology:cs_authority_grounding('e4cdbca5-c999-46fc-b70a-d5c701835ae4', lineage).
narrative_ontology:cs_interpretation_layer_present('e4cdbca5-c999-46fc-b70a-d5c701835ae4').
narrative_ontology:cs_reading_relation('e4cdbca5-c999-46fc-b70a-d5c701835ae4', paris_article_4_ndc__supranational_reading, forecloses).
narrative_ontology:cs_reading_relation('e4cdbca5-c999-46fc-b70a-d5c701835ae4', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('e4cdbca5-c999-46fc-b70a-d5c701835ae4', foundational, mitigation_ambition_must_be_nationally_self_determined).
narrative_ontology:cs_axiom_status(mitigation_ambition_must_be_nationally_self_determined, holdable).
narrative_ontology:cs_axiom_grounding('e4cdbca5-c999-46fc-b70a-d5c701835ae4', mitigation_ambition_must_be_nationally_self_determined, deontological).
narrative_ontology:cs_axiom('e4cdbca5-c999-46fc-b70a-d5c701835ae4', foundational, voluntary_universality_outperforms_binding_narrowness).
narrative_ontology:cs_axiom_status(voluntary_universality_outperforms_binding_narrowness, holdable).
narrative_ontology:cs_axiom_grounding('e4cdbca5-c999-46fc-b70a-d5c701835ae4', voluntary_universality_outperforms_binding_narrowness, instrumental).
narrative_ontology:cs_reference_frame('e4cdbca5-c999-46fc-b70a-d5c701835ae4', sovereign_national_determination_compact).
narrative_ontology:cs_drift_state('e4cdbca5-c999-46fc-b70a-d5c701835ae4', post_first_global_stocktake, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e4cdbca5-c999-46fc-b70a-d5c701835ae4', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_exporters).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, major_emerging_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, industrialized_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, industrialized_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National revenues and development plans in these states are built on hydrocarbon export streams. Inside the negotiations they defend voluntary pledge language, insist on development-rights framing, and block proposals for binding reduction schedules or review mechanisms carrying consequences. The arrangement asks nothing of them they have not chosen: their pledges set distant peaking dates and long horizons. Leaving the treaty would cost diplomatic standing and invite coordinated pressure without reducing world demand for their exports, so continued participation remains cheaper than exit.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_exporters, beneficiary,
    organized, generational, constrained, global).

% Large, fast-growing emitters whose near-term priority is industrial development. The voluntary design lets them pledge intensity targets and conditional ranges rather than absolute caps, keeps differentiation language intact, and pairs their pledges with technology-transfer and finance expectations directed at other parties. They face rising reputational pressure as their share of global emissions grows, but their domestic markets and South-South partnerships mean they can absorb diplomatic friction that smaller states cannot.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, major_emerging_economies, beneficiary,
    powerful, generational, mobile, global).

% Secured the non-binding architecture they sought after domestic ratification politics sank earlier binding designs. They collect reputational returns from pledge leadership and long-term target announcements at low legal risk, while paying through climate-finance mobilization expectations and the domestic decarbonization costs of their own volunteered pledges. Exit is available — one large member has withdrawn and re-entered — and minilateral clubs offer partial substitutes, which keeps their participation a recurring political choice rather than a settled fact.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, industrialized_states, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, industrialized_states, payer).

% Low-lying island and least-developed states whose exposure to warming vastly exceeds anything their own emissions could cause. They participate fully — filing pledges, transparency reports, and stocktake submissions — and their coalition wins agenda visibility, loss-and-damage language, and finance promises. What the arrangement's design asks them to accept is that every other state's energy choices remain sovereign while their own territory is at stake; the reporting workload falls hardest on administrations with the least capacity to carry it. Exiting would surrender their only universal forum without slowing the warming.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_nations, payer,
    organized, civilizational, trapped, global).

% Administers the machinery the pledges run on: maintains the NDC registry, publishes synthesis reports aggregating pledged targets, organizes expert review of transparency submissions, and services the periodic global stocktake. It sets procedural agendas and timetables but holds no power over pledge content; every substantive rule requires consensus among the parties themselves. Its budget and mandate depend on the continuation of the process it serves.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Transnational NGOs, youth movements, and scientific-advocacy networks holding observer accreditation who fill the corridors of every negotiating session but cast no votes. They press for binding schedules, faster phase-outs, and accountability mechanisms, and their campaigns shape media coverage and domestic politics in key capitals. Their leverage runs entirely through states they cannot compel; walking out of the process would forfeit the access their strategy depends on.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_advocacy_coalitions, excluded,
    organized, generational, trapped, global).

% Produces the assessment cycles that feed the global stocktake and calibrates what the pledges are measured against. It catalogs the gap between aggregated pledges and agreed temperature limits without taking a position on the treaty's design, and each assessment cycle raises the reputational cost of weak pledges for every party at once.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, ipcc_assessment_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__sovereigntist_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a universal architecture for coordinating national climate action: common greenhouse-gas accounting rules, a scheduled cycle of nationally determined pledges, expert review of transparency reports, and a periodic global stocktake aggregating collective progress against agreed temperature limits. It solves the participation problem — every major emitter operates inside the same framework — while leaving target levels to each government.
% TRANSFER_FUNCTION: Moves reputational credit and diplomatic standing among pledging states through the announcement-and-review cycle; moves administrative reporting effort from all parties (most heavily, relative to capacity, from small and poor administrations) into the UNFCCC review apparatus; and channels negotiated climate finance and technology cooperation from developed toward developing economies on a voluntary-mobilization basis. It transfers no legally enforceable obligation in any direction.
% ABSENT_VOICES: Future generations hold no seat anywhere in the process. Subnational governments, cities, and firms that implement or exceed the pledges appear only inside national delegations. Loss-and-damage claimants negotiate as petitioners for voluntary funds rather than as holders of enforceable claims. Civil society holds observer badges but no vote in the rooms where pledge rules are decided.
% DISAPPEARANCE_RATIONALE: The five-year pledge calendar, the transparency review cycle, the finance-mobilization expectations, and the universal diplomatic forum would all dissolve overnight. Climate governance would fragment into minilateral clubs and bilateral arrangements weighted toward the largest emitters, and vulnerable states would lose the only venue where their voice scales to parity with the largest polluters. Domestic policies already enacted would persist, but the expectation machinery linking them would not.
% FOUNDING_PROBLEM: After Kyoto's top-down binding targets produced ratification failure and retreat, negotiators needed an architecture that could achieve universal participation — including the United States and the large developing emitters — without triggering sovereignty-protective rejection. The founding problem was designing a climate regime that everyone would actually join.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Alliance of Small Island States' repeated ministerial declarations that participation-focused design has delivered neither adequacy nor protection attest the founding bargain's cost from the exposed seat; the post-Kyoto regime-design literature documents the deliberate trade of bindingness for universality after the 2001-2009 negotiation failures; and the withdrawal-and-return behavior of a major emitter attests that participation remains the live problem the design was built around. No beneficiary-party source is relied upon for the genealogy.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time points map T0-T10 onto 2015 (adoption) through 2025 (after the second pledge cycle and the first global stocktake); all three tracked series share this one grid. Extractiveness is low (ending 0.24) because the arrangement's costs are volunteered: the residual extraction is the capacity-disproportionate reporting burden falling on small poor administrations and the design-level externalization of warming risk onto the exposed, both of which accumulate slowly as pledge cycles repeat. Suppression is minimal (0.14): Article 28 provides a working exit, pledges may be revised, and the compliance mechanism is explicitly facilitative. The suppression_requirement series is authored because the interval genuinely tracks enforcement-character change: facilitative machinery was built (Katowice rulebook, transparency review operationalization, rising to 0.15 by T6) while coercive ambition atrophied (dipping to 0.14 as bindingness programs failed). Theater_ratio climbs from 0.30 to 0.45 as the 2019-2021 net-zero announcement wave detached headline pledges from implementation pathways — real transparency and stocktake functions persist underneath, keeping the ratio below the proxy-replacement threshold. Accessibility_collapse is low (0.20): minilateral clubs, climate litigation, border-adjustment regimes, and unilateral action all thrive alongside the treaty. Resistance is low-moderate (0.25): light demands buy acquiescence, with resistance arriving from both flanks — fossil states against even reporting rigor, ambitious states and advocates against the design's weakness. A five-year rhythmic modulation (pledge-cycle pressure spikes around COP moments, then dissipates) rides on these trends; the cycle concentrates reputational pressure intermittently, which functions as soft reinforcement rather than noise.
 *
 * PERSPECTIVAL GAP:
 *   The sharpest seat divergence in this story is between freedom and exposure experienced under the same word 'voluntary.' From the mobile beneficiary seats, the arrangement is maximal liberty: pledge what you choose, revise when you like, exit if you must. From the trapped payer seat, the identical design is abandonment with paperwork — the sovereignty everyone else exercises is the hazard they cannot escape, and their coalition's organizational power buys voice without veto. The secretariat seat computes as administration without capture: it runs the process, depends on it, and collects no extraction. The excluded advocacy seat registers the design's unanimity as an artifact of who was never given a vote. The engine computes these per-seat classifications from the structural data; this reading's rope claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map cleanly onto directionality. Three beneficiary groups derive low d: fossil exporters (beneficiary, constrained exit — they stay because staying is cheap, sitting near the subsidy end), emerging economies (beneficiary, mobile — mobility damps their d further), and industrialized states (beneficiary by design authorship, with a genuine secondary payer position carried as a secondary role rather than an override, because the power-atom-keyed override surface cannot distinguish them from the secretariat at the same institutional atom). The trapped payer derives high d: climate_vulnerable_nations sit near the full-target end — trapped exit plus existential exposure amplifies whatever the arrangement costs them. Undeclared seats (secretariat, advocates, IPCC) revert to symmetric or analytical defaults, which matches their situations: nothing substantial flows to or from them through the pledge transfer itself. Scope amplification is modest here: the arrangement is global, but its demands are light enough that verification difficulty adds little to effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions of mislabeling. Read from the supranational seat, this arrangement looks like pure theater — pledges extracting the appearance of action while emissions outrun them; that reading would mislabel a functioning coordination core as a snare. Read from this seat, the arrangement is a rope whose core functions (common accounting, scheduled review, finance channeling, universal membership) still operate beneath growing announcement performance. The theater trajectory (0.30 to 0.45) is the number to watch: it approaches but has not crossed the proxy-replacement threshold, because the stocktake and transparency machinery still bind expectations in ways that alter state behavior at the margin. Mandatrophy is unresolved rather than resolved: the founding problem (universal participation) is substantially solved, but the mandate has quietly expanded to ambition-raising that the voluntary design was never built to compel — the contested R5 status records exactly this half-life, and the mismatch consumer will find no dead-mandate-plus-world_rearranges zombie signature here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the paris_article_4_ndc kernel; would instantiating the supranational or equity reading instead change the constraint''s epsilon, beneficiary/victim structure, and computed type?',
    'Author and compile the sibling reading files over the identical referent arrangement; compare computed per-seat classifications and epsilon across the three readings.',
    'The supranational reading would author high epsilon over the same arrangement (appearance-of-action extraction riding a real coordination core) and likely compute a tangled_rope or snare profile; the equity reading would author moderate-high epsilon centered on differentiated-burden asymmetry. This file''s rope classification is indexical to the sovereigntist reading, not a topic-level fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of epsilon over the shared NDC arrangement.').

omega_variable(
    pledge_crystallization_risk,
    'Can voluntary pledges crystallize into de facto obligations through transparency-review evolution, CMA decisions, domestic litigation citing NDCs, or advisory-opinion cascades?',
    'Track CMA output language across pledge cycles, domestic court treatment of NDC pledges, and uptake of international climate advisory opinions in national jurisprudence.',
    'If pledges harden into obligations, the sovereigntist frame erodes from within: epsilon rises, suppression rises, and this reading converges toward the supranational sibling''s constraint without any treaty amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pledge_crystallization_risk, empirical, 'Risk of the voluntary design hardening into bindingness through accumulated legal practice.').

omega_variable(
    exposure_externalization_status,
    'Do the costs borne by climate-vulnerable nations count as extraction through the arrangement''s design (a structural payment exacted by the sovereignty-preserving choice) or as harms the arrangement merely fails to prevent?',
    'Counterfactual regime comparison: quantify ambition and outcome differences between the standing voluntary design and the binding alternatives actually on the table during the negotiation history (Copenhagen texts, Kyoto-style schedules).',
    'If counted as through-the-structure payment, the payer seat''s directionality rises and the computed classification drifts toward tangled_rope despite the low coercive overhead; if counted as unprevented harm, the rope reading stands with the payer seat explained as exposure rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exposure_externalization_status, conceptual, 'Whether vulnerable-state costs are extracted by the design or merely unprevented by it.').

omega_variable(
    fossil_pathway_durability,
    'Is the development-pathway preservation this reading counts as its central benefit durable, or does the voluntary design entrench asset lock-in that converts later into a disorderly, more costly transition?',
    'Stranded-asset modeling and observed divestment and transition rates in fossil-dependent economies measured against their stated pathway plans.',
    'If lock-in dominates, the reading''s principal benefit is deferred cost and the beneficiary seats'' directionality shifts toward the target end over successive pledge cycles; if pathways prove adjustable, the low-epsilon rope reading is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_pathway_durability, empirical, 'Durability of the development-pathway benefit the sovereigntist design preserves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndc_sovereigntist_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ndc_sovereigntist_tr_t2, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2, 0.32).
narrative_ontology:measurement(ndc_sovereigntist_tr_t4, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(ndc_sovereigntist_tr_t6, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(ndc_sovereigntist_tr_t8, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(ndc_sovereigntist_tr_t10, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(ndc_sovereigntist_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(ndc_sovereigntist_be_t2, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2, 0.16).
narrative_ontology:measurement(ndc_sovereigntist_be_t4, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 4, 0.18).
narrative_ontology:measurement(ndc_sovereigntist_be_t6, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 6, 0.2).
narrative_ontology:measurement(ndc_sovereigntist_be_t8, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(ndc_sovereigntist_be_t10, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 10, 0.24).

% Suppression requirement over time
narrative_ontology:measurement(ndc_sovereigntist_su_t0, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement(ndc_sovereigntist_su_t2, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2, 0.09).
narrative_ontology:measurement(ndc_sovereigntist_su_t4, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 4, 0.12).
narrative_ontology:measurement(ndc_sovereigntist_su_t6, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 6, 0.15).
narrative_ontology:measurement(ndc_sovereigntist_su_t8, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 8, 0.15).
narrative_ontology:measurement(ndc_sovereigntist_su_t10, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 10, 0.14).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, information_standard).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, kyoto_binding_targets_legacy).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Paris NDC regime' decomposes into three structurally distinct constraints — the sovereigntist reading (this file), the supranational reading, and the equity reading of the same Article 4 kernel — each with its own epsilon, victim set, and type. The sovereigntist reading is upstream in legitimacy terms: its voluntary design is the legal substrate both siblings contest, its persistence raises the cost of the supranational sibling's bindingness program, and its differentiation-friendly structure accommodates the equity sibling's claims. Kyoto's binding-target legacy is the ancestral constraint whose ratification failure this design was built to avoid; the family is linked so contamination and drift propagate visibly across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
