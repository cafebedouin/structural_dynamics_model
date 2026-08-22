% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Broad Effects Test of the Commerce Clause (Aggregate-Economic-Activity Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   A constitutional reading under which Congress may regulate any economic
 *   activity whose individual instances, summed across the national economy,
 *   substantially affect interstate commerce, including prohibiting the
 *   activity outright and controlling it comprehensively. Established by the
 *   1937 reversal, entrenched by the 1942 home-consumption wheat case,
 *   deployed for national public-accommodations law in 1964, and reaffirmed
 *   for the economic core in 2005, the reading converts the clause from a
 *   barrier-clearing trade provision into a general warrant for national
 *   economic governance. This file authors one member of a decomposed
 *   constraint family; the family structure and sibling deltas are recorded
 *   in network.dual_formulation_note and commentary.kernel_context. The
 *   claim/metric relationship is deliberately unreconciled: the reading is
 *   CLAIMED as tangled_rope, genuine interstate coordination purchased with
 *   real displacement of state regulatory authority, while the metrics
 *   describe the arrangement's actual operation independently.
 *
 * KEY AGENTS:
 *   - united_states_congress: agenda-setter and receipt point ([institutional]/[arbitrage]) — holds and exercises the power; displaced regulatory authority accrues here first
 *   - supreme_court: boundary administrator ([institutional]/[analytical]) — maintains, trims, or restores the reading case by case
 *   - federal_regulatory_agencies: primary beneficiary ([institutional]/[constrained]) — jurisdiction, budgets, and mandates scale with the reading's breadth
 *   - state_governments: primary target ([organized]/[trapped]) — occupied regulatory fields; no exit from the constitutional order
 *   - small_scale_intrastate_producers: target ([powerless]/[trapped]) — home-use conduct reached by statistical aggregation
 *   - personal_use_possessors: target ([powerless]/[trapped]) — state-authorized possession rendered federally reachable
 *   - national_interest_groups: beneficiary ([powerful]/[mobile]) — single uniform-policy venue replacing fifty-state campaigns
 *   - civil_rights_enforcement_coalitions: beneficiary ([organized]/[constrained]) — the 1964 public-accommodations vehicle
 *   - national_market_firms: dual-positioned beneficiary/target ([powerful]/[constrained]) — uniform rules gained, federal compliance costs paid
 *   - federalism_advocacy_movement: excluded voice ([organized]/[trapped]) — six decades outside the operative interpretive conversation
 *   - constitutional_law_scholars: analytical observer ([analytical]/[analytical]) — maps the doctrine and maintains the competing frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.6).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.58).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.6).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Broad Effects Test of the Commerce Clause (Aggregate-Economic-Activity Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'ab18d3e5-fffb-4c8d-8489-39ff91048ffb').
narrative_ontology:cs_kernel_codification('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', fixed_text).
narrative_ontology:cs_authority_grounding('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', lineage).
narrative_ontology:cs_interpretation_layer_present('ab18d3e5-fffb-4c8d-8489-39ff91048ffb').
narrative_ontology:cs_reading_relation('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', foundational, aggregate_economic_effects_create_federal_jurisdiction).
narrative_ontology:cs_axiom_status(aggregate_economic_effects_create_federal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', aggregate_economic_effects_create_federal_jurisdiction, empirically_contingent).
narrative_ontology:cs_axiom('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', foundational, regulate_comprehends_prohibition_and_control).
narrative_ontology:cs_axiom_status(regulate_comprehends_prohibition_and_control, holdable).
narrative_ontology:cs_axiom_grounding('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', regulate_comprehends_prohibition_and_control, conventional).
narrative_ontology:cs_reference_frame('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', national_market_integration_grant).
narrative_ontology:cs_drift_state('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', contemporary_post_lopez_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('ab18d3e5-fffb-4c8d-8489-39ff91048ffb', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_coalitions).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_market_firms).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, small_scale_intrastate_producers).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, personal_use_possessors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, national_market_firms).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, aggregate_effects_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, commerce_power_includes_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the power this reading confers and exercises it by statute: defines what counts as regulating commerce, drafts the jurisdictional elements and legislative findings that carry intrastate activity into federal schemes, and decides when to prohibit outright rather than condition. Regulatory authority displaced from the states lands here first, and when the reading is trimmed the institution substitutes other instruments such as spending conditions and enforcement legislation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, united_states_congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Administers the textual boundary case by case: decides whether an activity's summed effects bring it within federal reach, and periodically redraws the line, trimming in 1995 and 2000, restoring the economic core in 2005, and declining to extend it further in 2012. Its interpretive commitments are the mechanism that keeps the reading operative.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Receive delegated authority over whatever the reading sweeps in; their mandates, budgets, and headcount scale with its breadth. They draft the findings of substantial effects that sustain each extension, and a narrowing of the reading shrinks their jurisdiction directly. Their career structures and institutional self-conceptions are built on the fields they administer.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).

% Govern their territories inside a constitutional order in which Congress may occupy any regulatory field once economic effects are shown. Preemption removes policy space they previously controlled; they retain concurrent authority only where Congress has not acted, and they cannot withdraw from the order that allocates authority this way. They appear in the interpretive process chiefly as litigants.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    organized, generational, trapped, regional).

% Grow, raise, or manufacture for local sale or home use. Under the aggregation principle their individually negligible output sums with that of millions of others into a substantial national effect, placing their planting quotas, production methods, and surpluses under federal administration, including what they feed to their own livestock or consume on their own land.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, small_scale_intrastate_producers, payer,
    powerless, immediate, trapped, local).

% Hold or cultivate small quantities for personal, non-commercial purposes under their own state's law. The same aggregation logic reaches purely intrastate possession, so federal criminal exposure attaches to activity their state expressly authorizes, and no change in their own conduct removes them from federal reach.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, personal_use_possessors, payer,
    powerless, immediate, trapped, local).

% Organize around policy preferences they want enacted uniformly. A single federal statute reaches every state at once, so they concentrate advocacy on Congress and the agencies rather than running fifty separate campaigns, and they supply much of the legislative-findings record on which each extension rests.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    powerful, generational, mobile, national).

% Used the reading to reach discriminatory practices at nominally local businesses serving interstate travelers, whose refusals of service summed across the country into a substantial burden on interstate movement. The public-accommodations victories of 1964 stand on this reading's breadth, and the later narrowings removed one of their tools.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_coalitions, beneficiary,
    organized, generational, constrained, national).

% Operate across state lines and face one federal compliance regime instead of fifty divergent state codes, lowering the cost of doing business nationally. They also absorb federal compliance costs and lose state rules that favored them; they litigate and lobby at the boundary, and relocating does not exempt them from federal schemes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_market_firms, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, national_market_firms, payer).

% State officials, scholars, and advocates who argue the reading leaves no principled outer bound on national authority. Through the 1937 to 1995 settlement they held no seat in the interpretive conversation, their arguments surfacing mainly in dissenting opinions, and their revival came only after six decades outside the operative doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federalism_advocacy_movement, excluded,
    organized, generational, trapped, national).

% Map the doctrine's development, document how far the aggregation principle reaches, and maintain the competing interpretive frameworks that structure debate over the clause's meaning. They collect nothing and bear nothing under the reading; their product is the analytical record.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, united_states_congress).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves problems no single state can solve: interstate externalities such as airsheds, waterways, financial contagion, and disease that spill across borders; regulatory competition in which states undercut one another's standards to attract business; trade barriers states erect against sister states; and the transaction costs of fifty divergent commercial codes for firms operating nationally. The reading supplies the jurisdictional warrant for addressing all of these at national scale.
% TRANSFER_FUNCTION: Moves regulatory authority from state capitals to Congress and federal agencies, together with the discretion, staffing, and budgets that follow it; moves compliance obligations onto firms and individuals whose intrastate activity is drawn into federal schemes; and moves policy-setting leverage to national interest groups able to act through a single federal venue.
% ABSENT_VOICES: State governments as sovereigns never held a seat in the interpretive settlement that produced this reading; they enter only as occasional litigants, and the 1937 to 1995 consolidation occurred over their sustained objection. Small-scale intrastate producers and personal-use possessors had no voice at all when the aggregation principle was fixed: the landmark home-consumption and home-cultivation cases were decided without anyone speaking for the class of people whose household conduct would henceforth be federally reachable.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the constitutional foundation of a large share of federal regulatory and criminal statutes would fail or force re-grounding in other enumerated powers; national programs governing agriculture, environment, labor standards, food and drug safety, and civil-rights-era public accommodations would devolve, fragment, or lapse; fifty-state patchworks would re-emerge in fields currently governed uniformly. The dependence is real and enormous, which is precisely why no narrowing has ever approached withdrawal of the core.
% FOUNDING_PROBLEM: Two problems, in sequence. The founding problem proper: under the Articles of Confederation, states imposed tariffs and discriminations against sister-state commerce and no national authority existed to clear them or guarantee uniform commercial rules; the clause was written as the remedy. The successor problem invoked since the New Deal: nationwide economic crises and interstate externalities that state-by-state regulation demonstrably failed to manage.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the beneficiary set by economic-historical scholarship documenting the 1780s tariff wars and interstate trade discriminations. Current status divides along the kernel's own fault line: interstate-spillover research in environmental and financial economics documents problems no single state can manage, attesting liveness, while state litigants and federalism scholarship attest the original barrier-clearing problem was solved long ago and the power now runs on acquired jurisdiction, attesting obsolescence. No out-of-set source settles the dispute, hence contested.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.60 because the reading's reach is bounded less by doctrine than by congressional appetite: any economic activity aggregates, prohibition is expressly included, and commerce findings receive near-total deference. Suppression (0.58) is structural, not internalized: preemption and supremacy close state alternatives whenever Congress occupies a field, while unoccupied fields stay open, so suppression tracks occupation rather than persuasion. Theater (0.35) reflects a test increasingly recited rather than applied: substantial-effects review functions as a rubber stamp for economic activity, though the 1995 and 2000 decisions show it can still bite at the edges. Accessibility collapse (0.58): within operative doctrine from 1937 to 1995 the narrower readings were treated as unavailable, surviving only in dissent; they have since partially re-entered, so alternatives are dimmed rather than extinguished. Resistance (0.60): the 1995, 2000, and 2012 decisions plus a sustained federalism revival meet the reading without dislodging its core. The measurement series share one eight-point grid (1937 to 2025 mapped to t=0 to t=88): ascent through 1942 and the civil-rights era, peak preemption circa 1982, the trims of 1995 and 2000, the 2005 restoration of the economic core, and a stable contested equilibrium thereafter. Fixing is prohibitive: unwinding the reading would strand the statutory edifice built on it, which is why every narrowing has been surgical.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the Congress and agency seats the arrangement is an instrument they operate and staff: jurisdiction arrives, programs launch, and the reading presents as capability. From the state-government seat the same instrument is displacement: fields they governed are occupied by preemption they did not consent to, and their recourse is litigation they usually lose. From the small-producer and possessor seats it is exposure without visibility, since federal reach arrives through statistical aggregation of their own household conduct. The Court's seat has flipped historically: it authored the broad reading, trimmed it, restored its core, and now polices its edges, so its computed position tracks whichever doctrine is current. Institutional identity binds the agencies: their mandates, career structures, and self-conceptions are constituted by the jurisdiction this reading supplies, so they defend it as self-preservation rather than by argument, and if that identity frame broke, agency-side defense of the reading's breadth would collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: federal_regulatory_agencies, national_interest_groups, civil_rights_enforcement_coalitions, and national_market_firms sit toward the subsidized end, receiving jurisdiction, a uniform policy venue, an enforcement vehicle, and compliance simplification respectively; the firms are dual-positioned, paying federal compliance costs alongside the uniformity gain. Victim declarations drive high directionality: state_governments (organized, trapped) bear occupied regulatory fields; small_scale_intrastate_producers and personal_use_possessors (powerless, trapped, local) bear federal reach over conduct they cannot relocate away from. Congress holds the agenda-setting seat and is the receipt point: authority displaced from the states accrues to it first, before delegation, so its directionality sits near the beneficiary end despite its distance from day-to-day enforcement. The Court administers the boundary without collecting from it. Suppression enters the engine's computation unscaled, as a raw structural property; only extractiveness is scaled by directionality and spatial scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem in its original form, clearing state trade barriers, is dead: no serious party argues the Articles-era pathology persists. The successor problem, interstate externalities and national-market governance, is live but is not the problem the clause was written for, and the parties dispute whether today's arrangement serves it or merely inherits jurisdiction. Authoring the status as contested blocks two opposite errors: declaring the mandate resolved, which would fit a vestigial arrangement the world no longer needs and is false here since the world visibly rearranges without the reading; and declaring the mandate fully live, which would launder acquired-jurisdiction persistence as necessity. The tangled_rope claim performs the same double work against mislabeling: against the pure-coordination mislabel, the victim declarations and the aggregation principle's unbounded character record real displacement; against the pure-extraction mislabel, the coordination function is genuine and the civil-rights lineage shows the power accomplishing what no state could. The open question carried in the omegas, whether aggregation has a stopping point, is exactly the fault line along which this reading would degrade toward pure extraction if the answer is no.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint instantiates the broad_effects_test reading of the commerce_clause_scope kernel; how would the classification shift if the arrangement were read through the narrow_originalist or intermediate_channels readings instead?',
    'Generate the sibling stories and compare computed classifications: the narrow reading shrinks the governed set to cross-border traders and restores state regulatory autonomy; the intermediate reading retains the economic-aggregation core but caps it at economic activity, cuts attenuated causal chains, and requires jurisdictional elements for non-economic activity.',
    'Under narrow_originalist the arrangement likely computes with low extraction and a genuine trade-facilitation function; under intermediate_channels it remains a hybrid with a materially smaller victim set. The expansive victim set and high state-sovereignty displacement measured here are properties of THIS reading, not of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    aggregation_stopping_point,
    'Does the aggregation principle contain a principled stopping point, or does it extend federal jurisdiction to all human activity, since any conduct aggregates to a substantial national sum across millions of actors?',
    'Doctrinal analysis of the post-1995 limiting principles (economic-activity requirement, jurisdictional elements, prohibition of attenuated causal chains) stress-tested against the hard cases the reading itself produced: home-consumed wheat (1942) and state-authorized home-grown medicine (2005). Identify whether any class of intrastate conduct remains structurally unreachable.',
    'If no stopping point exists, the reading operates as a plenary national police power and the arrangement shifts toward pure extraction wearing a coordination story; a defensible stopping point preserves the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_stopping_point, conceptual, 'Whether the aggregation principle is bounded or asymptotically total.').

omega_variable(
    civil_rights_valence,
    'Does the civil-rights use of the commerce power count as a genuine coordination achievement delivered by this reading, or as a valued outcome that launders the reading''s broader displacement?',
    'Counterfactual institutional analysis: could 1964 public-accommodations desegregation have been achieved through Fourteenth Amendment enforcement legislation or state-level action, at what coverage, durability, and delay; compare against the commerce-route record.',
    'If comparable outcomes were available through other enumerated powers, civil_rights_enforcement_coalitions thins out of the beneficiary ledger and the reading''s coordination case weakens; if the commerce vehicle was uniquely effective, the benefit is genuine and load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_rights_valence, preference, 'Valence of the reading''s most celebrated application.').

omega_variable(
    persistence_source,
    'Does the broad reading persist because downstream constituencies demand the coordination it provides, or because incumbent federal institutions defend the jurisdiction they have already acquired?',
    'Examine the 1995, 2000, and 2012 narrowing episodes: whether coordinated political and institutional defense restored scope after each trim (as in the 2005 reaffirmation of the economic core), and model whether the reading would survive withdrawal of agency and congressional support.',
    'Demand-side persistence supports a coordination-led classification; supplier-side persistence (agencies defending acquired mandates as self-preservation) indicates entrenchment riding on a coordination legacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_source, empirical, 'Whether persistence is demand-driven or supplier-enforced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 0, 88).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__broad_effects_test, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comm_tr_t5, commerce_clause_scope__broad_effects_test, theater_ratio, 5, 0.2).
narrative_ontology:measurement(comm_tr_t27, commerce_clause_scope__broad_effects_test, theater_ratio, 27, 0.29).
narrative_ontology:measurement(comm_tr_t45, commerce_clause_scope__broad_effects_test, theater_ratio, 45, 0.37).
narrative_ontology:measurement(comm_tr_t58, commerce_clause_scope__broad_effects_test, theater_ratio, 58, 0.32).
narrative_ontology:measurement(comm_tr_t63, commerce_clause_scope__broad_effects_test, theater_ratio, 63, 0.33).
narrative_ontology:measurement(comm_tr_t75, commerce_clause_scope__broad_effects_test, theater_ratio, 75, 0.36).
narrative_ontology:measurement(comm_tr_t88, commerce_clause_scope__broad_effects_test, theater_ratio, 88, 0.35).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__broad_effects_test, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comm_be_t5, commerce_clause_scope__broad_effects_test, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(comm_be_t27, commerce_clause_scope__broad_effects_test, base_extractiveness, 27, 0.58).
narrative_ontology:measurement(comm_be_t45, commerce_clause_scope__broad_effects_test, base_extractiveness, 45, 0.64).
narrative_ontology:measurement(comm_be_t58, commerce_clause_scope__broad_effects_test, base_extractiveness, 58, 0.62).
narrative_ontology:measurement(comm_be_t63, commerce_clause_scope__broad_effects_test, base_extractiveness, 63, 0.61).
narrative_ontology:measurement(comm_be_t75, commerce_clause_scope__broad_effects_test, base_extractiveness, 75, 0.6).
narrative_ontology:measurement(comm_be_t88, commerce_clause_scope__broad_effects_test, base_extractiveness, 88, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__broad_effects_test, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(comm_su_t5, commerce_clause_scope__broad_effects_test, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(comm_su_t27, commerce_clause_scope__broad_effects_test, suppression_requirement, 27, 0.6).
narrative_ontology:measurement(comm_su_t45, commerce_clause_scope__broad_effects_test, suppression_requirement, 45, 0.66).
narrative_ontology:measurement(comm_su_t58, commerce_clause_scope__broad_effects_test, suppression_requirement, 58, 0.6).
narrative_ontology:measurement(comm_su_t63, commerce_clause_scope__broad_effects_test, suppression_requirement, 63, 0.59).
narrative_ontology:measurement(comm_su_t75, commerce_clause_scope__broad_effects_test, suppression_requirement, 75, 0.58).
narrative_ontology:measurement(comm_su_t88, commerce_clause_scope__broad_effects_test, suppression_requirement, 88, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Commerce Clause' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that must not share one story: the broad-effects reading (this file), the narrow originalist reading (commerce equals trade crossing state lines; regulate equals facilitate), and the intermediate channels reading (Lopez's three categories with limiting principles). Their epsilon values differ widely because their victim sets and coordination ledgers differ: the broad reading reaches virtually all economic activity via aggregation and displaces state police powers wholesale; the narrow reading reaches only cross-border trade; the intermediate reading keeps the economic core but caps aggregation. This file authors the broad claim alone and links its siblings through network.affects_constraints; upstream-downstream pressure runs from this reading to the intermediate reading, whose limiting principles were forged as corrections to this one's overreach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
