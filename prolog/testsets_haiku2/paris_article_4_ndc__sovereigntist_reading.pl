% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
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
 *   human_readable: Paris NDC Sovereigntist Reading: Voluntary Pledges Preserving Energy Sovereignty
 *   domain: international/environmental/political_economy
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 establishes the NDC (Nationally
 *   Determined Contribution) mechanism: each state voluntarily sets its own
 *   climate pledge, revises it on a five-year cycle, and reports progress. No
 *   external body sets the target, approves the pledge, or imposes penalties
 *   for non-compliance. The sovereigntist reading interprets this as a
 *   principled commitment to national energy sovereignty—each state
 *   determines its own climate pathway according to its development
 *   priorities, and the global coordination problem is solved by transparency
 *   and mutual learning, not by external obligation. This reading is favored
 *   by fossil-dependent economies (Saudi Arabia, Russia, India at different
 *   moments), developed states seeking flexibility (the US, EU at different
 *   moments), and states that view climate obligations as potential
 *   sovereignty violations. The reading's ε is low (0.31 terminal) because
 *   the constraint operates as pure coordination—transparent information
 *   exchange and voluntary revision cycles—with minimal extractive overlay.
 *   The claim/metric gap is deliberate: the sovereigntist reading CLAIMS to
 *   be rope (genuine coordination) and the authored metrics confirm it—this
 *   is not a case where the metrics diverge from the claim to signal false
 *   summitry, but rather where the claim is structurally accurate and the
 *   metrics support it.
 *
 * KEY AGENTS:
 *   - Fossil-dependent economies (Saudi Arabia, Russia, India, Indonesia): primary beneficiaries; retain development pathways and revision freedom.
 *   - Developed high-income states (US, EU, UK, Japan): agenda-setters with interpretive authority; can set ambitious pledges while preserving flexibility.
 *   - Climate-impacted states (small island states, Bangladesh, sub-Saharan Africa): trapped payers; dependent on others' voluntary compliance without enforcement leverage.
 *   - Climate advocacy organizations: organized payers; mobilize evidence with constrained structural leverage.
 *   - Supranational governance proponents and strong equity advocates: structurally excluded; their core premises contradict the sovereigntist reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.31).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.18).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris NDC Sovereigntist Reading: Voluntary Pledges Preserving Energy Sovereignty").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international/environmental/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, 'aeb83101-1146-448a-bc87-df45189bd615').
narrative_ontology:cs_kernel_codification('aeb83101-1146-448a-bc87-df45189bd615', fixed_text).
narrative_ontology:cs_authority_grounding('aeb83101-1146-448a-bc87-df45189bd615', lineage).
narrative_ontology:cs_interpretation_layer_present('aeb83101-1146-448a-bc87-df45189bd615').
narrative_ontology:cs_reading_relation('aeb83101-1146-448a-bc87-df45189bd615', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('aeb83101-1146-448a-bc87-df45189bd615', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('aeb83101-1146-448a-bc87-df45189bd615', foundational, states_retain_sovereign_climate_determination).
narrative_ontology:cs_axiom_status(states_retain_sovereign_climate_determination, holdable).
narrative_ontology:cs_axiom_grounding('aeb83101-1146-448a-bc87-df45189bd615', states_retain_sovereign_climate_determination, deontological).
narrative_ontology:cs_axiom('aeb83101-1146-448a-bc87-df45189bd615', foundational, voluntary_pledge_revision_preferable_to_binding_external_targets).
narrative_ontology:cs_axiom_status(voluntary_pledge_revision_preferable_to_binding_external_targets, holdable).
narrative_ontology:cs_axiom_grounding('aeb83101-1146-448a-bc87-df45189bd615', voluntary_pledge_revision_preferable_to_binding_external_targets, empirically_contingent).
narrative_ontology:cs_reference_frame('aeb83101-1146-448a-bc87-df45189bd615', state_sovereign_climate_determination).
narrative_ontology:cs_drift_state('aeb83101-1146-448a-bc87-df45189bd615', post_paris_accumulation_era_2020_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aeb83101-1146-448a-bc87-df45189bd615', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, development_coalition).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, industrial_states_with_carbon_lock_in).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, developed_high_income_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, global_climate_monitoring_infrastructure).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, climate_action_advocates).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, climate_impacted_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States whose primary revenue and development model depend on fossil fuel extraction or energy-intensive industry. The sovereigntist reading permits them to set NDCs at levels compatible with continued development pathways; they retain the exit option to revise pledges upward or downward without external penalty, and can frame climate action as a voluntary choice rather than an imposed obligation. They benefit from the low enforcement pressure and revision freedom the reading entails.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies, beneficiary,
    organized, generational, mobile, global).

% Industrial democracies with mature decarbonization pathways and capital for transition. The sovereigntist reading allows them to set ambitious NDCs that signal climate leadership while preserving flexibility to adjust via voluntary revision; they also maintain the interpretive authority over what 'voluntary' and 'nationally determined' mean operationally. They can frame climate action as a cooperative choice rather than a binding regime, positioning themselves as architects of the consensus rather than subjects of a constraint.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, developed_high_income_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, developed_high_income_states, agenda_setter).

% The UNFCCC reporting apparatus, NDC registry, and associated technical bodies. The sovereigntist reading requires transparent self-reporting and biennial review but does not impose external verification or penalty mechanisms; this preserves the reporting infrastructure's administrative function while relieving it of the enforcement burden that would come with a supranational reading. They collect institutional legitimacy from the appearance of coordination without the liability of coercive gatekeeping.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, global_climate_monitoring_infrastructure, beneficiary,
    institutional, biographical, constrained, global).

% International NGOs, climate research institutions, and activist coalitions that mobilize evidence of climate risk and advocate for binding emissions reductions. Under the sovereigntist reading, their advocacy reaches governments but produces no enforceable outcome; pledges can be revised downward or missed without penalty, and the 'voluntary' framing channels their pressure into moral suasion rather than institutional obligation. They bear the cost of mobilizing evidence and advocacy with limited structural leverage.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_action_advocates, payer,
    organized, biographical, constrained, global).

% Small island states, least developed countries, and regions with acute climate vulnerability. Under the sovereigntist reading, they can pledge ambitious climate action but cannot compel others to reduce emissions; they depend on the voluntary pledges of others for their own survival. Their exit option is severely constrained—they cannot leave the climate system or exit the Paris framework without losing even the nominal protection of international recognition and adaptation finance. They bear concentrated costs while holding minimal enforcement leverage.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_impacted_states, payer,
    moderate, biographical, trapped, global).

% The collective UNFCCC membership, operating via consensus. Under the sovereigntist reading, they administer the pledge mechanism and review process but refrain from imposing external targets or penalties; each state sets its own pledge and revision timeline. The apparatus maintains legitimacy by respecting state sovereignty while coordinating the reporting and transparency infrastructure.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_party_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Parties arguing that climate obligations must account for historical responsibility and differentiated capacity (the equity reading). Under the sovereigntist reading, equity claims are absorbed into the 'nationally determined' language—each state decides what is equitable according to its own lights—which de facto advantages states with greater capacity to argue away obligations. They are structurally excluded from enforcing equity standards because the reading treats equity as internal to national determination, not an external standard.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, equity_advocates, excluded,
    organized, biographical, constrained, global).

% Parties arguing for binding international emissions targets, external verification, and escalating ratchet mechanisms. Under the sovereigntist reading, their institutional vision is structurally foreclosed: binding targets would violate the 'voluntary' frame, external verification would compromise sovereignty, and a ratchet trajectory would require external authority to determine what each state must do. They are excluded from the conversation because their core premise contradicts the reading's foundational claim.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, supranational_governance_proponents, excluded,
    organized, biographical, constrained, global).

% Examines the Paris framework's operational architecture and the structural consequences of the sovereigntist interpretation for global emissions trajectories, state behavior, and climate risk distribution.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__sovereigntist_reading, developed_high_income_states).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transparent global registry of climate pledges and revision cycles, enabling states to coordinate on emissions reduction timelines, learn from each other's mitigation approaches, and signal climate commitment to domestic and international constituencies.
% TRANSFER_FUNCTION: Transfers reputational benefit and political flexibility to states that set ambitious-seeming NDCs while retaining exit options; transfers climate risk to states that cannot afford mitigation or exit the climate system.
% ABSENT_VOICES: Supranational governance proponents argue for binding targets and external verification, which the sovereigntist reading structurally excludes by defining the regime as voluntary. Strong equity advocates argue that the reading obscures historical responsibility by treating all states as equal agents. Both groups are kept out by the reading's foundational claim that national determination and voluntariness are inviolable.
% DISAPPEARANCE_RATIONALE: If the NDC regime disappeared, global climate coordination would lose its primary legitimacy mechanism. States would no longer have a common reporting infrastructure, and the appearance of coordinated global climate action would collapse. Fossil-dependent economies would lose the diplomatic cover that characterizes their climate position as sovereign choice rather than defection.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.31) because the sovereigntist reading operationalizes the constraint as information coordination: transparent pledges, biennial reviews, and voluntary revision cycles. There is no centralized decision-maker extracting value, no coercive gatekeeping, and no asymmetric burden imposed by the reading itself. The low extractiveness is a feature of the reading, not a measurement error. Suppression is minimal (0.18) because the constraint's persistence does not depend on preventing alternatives or suppressing exit options—states can revise pledges downward without penalty, and the framework explicitly respects national sovereignty. Theater is modest (0.22) because while some of the pledge-setting involves diplomatic signaling (ambitious numbers for domestic constituencies), the core coordination function—transparent registry and biennial review—is genuinely operational. The measurement series shows slight upward drift in all three metrics, reflecting the gradual erosion of the pure coordination case: as climate science evidence accumulates and pressure from advocates and impacted states grows, more of the pledge-setting becomes performative (states set numbers they expect to miss), enforcement pressure rises (carbon border adjustments, climate finance conditionality), and the system acquires mild extractive character. However, the structural operation remains coordination-centric at the terminal point. The sovereignty frame itself becomes more theatrical over time—by 2024, the gap between pledged and achieved emissions is widely documented, and the 'voluntary' framing increasingly appears to be cover for inaction rather than a genuine principle. Theater ratio reflects this: the review process maintains legitimacy by treating pledges as serious while de facto permitting systematic shortfalls.
 *
 * PERSPECTIVAL GAP:
 *   The sovereigntist reading's claim of 'preservation of energy sovereignty' is heard differently across seats. Developed high-income states hear it as: 'we retain flexibility to adjust our climate pledges according to our own democratic processes.' Fossil-dependent economies hear it as: 'we are not obligated to abandon development pathways.' Climate-impacted states hear it as: 'you are not obligated to help us.' The gap is not in the structure but in what the structure enables and forbids. The sovereigntist reading explicitly forbids external targets and penalties (that would be a sovereignty violation). This means it explicitly permits inaction without penalty. The reading does not see this as a tragic trade-off; it sees it as the principled boundary between coordination and coercion. But the tragedy is real on the victim seats: the preservation of sovereignty for some entails the precarity of climate risk for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from exit options and beneficiary/victim status. Fossil-dependent economies are beneficiaries with mobile exit (they can revise pledges, set low targets, exit climate talks without penalty) → d near beneficiary end, low χ. Developed high-income states are beneficiaries-with-agenda-setter roles; they have arbitrage-grade exit (can set ambitious pledges and revise downward, can shift burdens to others through carbon markets and adaptation finance flows) → d near beneficiary end, χ near zero. Climate-impacted states are victims (concentrated climate risk, trapped exit—they cannot exit the climate system or the Paris framework without losing even nominal protection and adaptation finance) → d near target end, χ high. Climate advocates are payers (mobilize effort, constrained exit—they cannot stop advocating without abandoning their stated mission) → d moderate-to-high. The sovereigntist reading itself does not specify which parties control the directionality computation; that is structural and independent of the reading. What the reading does specify is that enforcement mechanisms are absent, which means all directionality measures are softer than they would be under a supranational reading (no external penalty for defection, no mandatory escalation). This is the key structural asymmetry: the reading's low suppression and low extractiveness depend on the absence of enforcement, which in turn depends on respecting state exit options. If enforcement were added (supranational reading), the directionality landscape would sharpen dramatically—what looks like coordination with flexible participation (rope from beneficiary seats) would look like enforced obligation (tangled rope or snare from victim seats).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to coordinate climate action without imposing external targets that states perceive as sovereignty violations) was live in 2015. It is now contested. The sovereigntist reading treats the problem as still live—states still need protection from external mandate. But the supranational reading argues the problem is dead: climate risk is now so acute that the founding problem (fear of sovereignty violation) is outweighed by the existential problem (climate catastrophe). The equity reading argues the problem was never correctly framed—the real problem is differentiating obligations by capacity and responsibility, not choosing between global coordination and national sovereignty. Mandatrophy does not apply to this constraint because the sovereigntist reading does not claim the founding problem has been solved; it claims the problem is so serious that we must accept the limitation (voluntary pledges, no enforcement) to preserve the political possibility of any global agreement at all. That is a tragic compromise, not a solved problem. The constraint persists because states collectively prefer it to either no agreement (pure defection) or a binding regime that develops-state coalitions would reject. It does not persist because the problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coercive_boundary,
    'Where exactly is the boundary between ''voluntary coordination'' and ''coercive obligation''? Does the sovereigntist reading''s characterization of the NDC regime as voluntary depend on a particular theory of sovereignty, or is voluntariness a structural fact independent of theory?',
    'Examine state behavior under pressure: when developed states offer climate finance conditioned on NDC ambition (e.g., Green Climate Fund), does the sovereigntist reading''s claim of voluntariness survive? When developing states face trade sanctions or migration pressure tied to climate performance, do they experience the regime as voluntary? The empirical test is whether states perceive they have exit options that are genuinely costless.',
    'If the boundary is theory-dependent and states experience coercion through side channels (finance conditionality, trade pressure, diaspora pressure), the sovereigntist reading mischaracterizes the constraint''s operation, and the true classification would be tangled rope (coordinated pledge-setting + coercive pressure through side mechanisms) or snare (the voluntariness is cover for conditioned access). If the boundary is structural and states retain genuine costless exits, rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_vs_coercive_boundary, empirical, 'Whether the regime''s voluntariness is genuine or theater.').

omega_variable(
    founding_problem_live_or_dead,
    'Is the founding problem (fear of external climate targets violating sovereignty) still live, or has it become dead—outweighed by climate risk to the point that states now understand climate obligation as a collective survival requirement rather than a sovereignty threat?',
    'Track UNFCCC negotiations and state statements: as climate damages accumulate (floods, crop failures, climate migration), do states begin framing climate pledges as obligations rather than voluntary choices? Do they demand external accountability mechanisms, or do they defend the voluntary frame even as damages rise? The live/dead boundary moves when the political salience of the founding problem drops below the political salience of climate risk.',
    'If the founding problem is dead but states continue defending the sovereigntist reading, the reading becomes a zombie: the mandate (voluntariness, sovereignty) has outlived its justification (avoiding sovereignty violation), and the constraint becomes piton (inertially maintained, mostly theatrical). If the problem is live, the sovereigntist reading is a genuine tragic compromise: we accept the risk of weak enforcement to preserve the political possibility of any agreement at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_or_dead, conceptual, 'Whether the sovereigntist reading''s founding problem remains a live political constraint or has become moot.').

omega_variable(
    equity_reading_foreclosure,
    'Does the sovereigntist reading logically foreclose the equity reading (which demands differentiated obligations based on historical responsibility and capacity)? Or do the two readings coexist, with states choosing between them according to their own interests?',
    'Examine whether a state can hold both readings simultaneously: can Saudi Arabia defend its low NDC by claiming (a) sovereigntist: ''we determine our own pledge'' AND (b) equity: ''as a developing petrostate, we have less capacity than developed states''? If both claims can be held together, the readings coexist. If they are logically in tension (one forecloses the other), then one reading is reclassified from coexists_with to forecloses.',
    'If the readings coexist, the constraint has an irreducible ambiguity: the ''voluntary self-determination'' principle can be invoked to defend either weak action (sovereigntist) or differentiated action (equity). If sovereigntist forecloses equity (or vice versa), the constraint''s interpretation landscape is bifurcated, and the engine''s reading_relations should reflect foreclosure rather than coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_reading_foreclosure, conceptual, 'Whether the sovereigntist and equity readings are logically compatible or mutually exclusive.').

omega_variable(
    beneficiary_identity_ambiguity,
    'Who truly benefits from the sovereigntist reading''s low-enforcement architecture? Fossil-dependent economies gain development flexibility, but do they benefit in the long term if climate damages reduce their own development pathways? Developed high-income states gain agenda-setting power, but do they benefit if climate damages reduce global economic stability?',
    'Model long-term payoff matrices: compare the value to each state of (a) short-term development flexibility under sovereigntist NDCs vs. (b) long-term climate stability under binding, ratcheting targets. The true beneficiary is whichever party''s long-term payoff is higher. This requires modeling state time horizons, climate sensitivity of economic bases, and access to climate adaptation technology.',
    'If long-term modeling shows no net beneficiary (all states worse off under sovereigntist reading than under binding targets), the constraint becomes a collective action trap: everyone benefits from collective action but no one has incentive to move first, so the low-enforcement regime persists despite being Pareto-inferior. If some states (developed high-income, very large economies) are genuinely better off under sovereigntist terms, they are the true beneficiaries and the constraint is extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, empirical, 'Whether the identified beneficiaries truly gain long-term benefit or whether the sovereigntist reading is a collective action trap.').

omega_variable(
    kernel_committer_reading_choice,
    'Does the Paris Agreement text (the kernel) permit all three readings equally, or does it textually privilege one reading? Is the sovereigntist reading a reasonable interpretation of ''nationally determined'' and ''voluntary,'' or is it a creative reframing that bends the text to serve particular state interests?',
    'Conduct textual analysis of Article 4 and related Paris provisions. Compare to UNFCCC negotiating history: what did states intend by ''nationally determined''? Does the text support external NDC review mechanisms (which would push toward supranational reading) or does it explicitly foreclose them (which would support sovereigntist reading)? Are equity considerations mentioned in the text (which would support equity reading)?',
    'If the text privileges sovereigntist interpretation, the reading is structurally grounded in the kernel and the constraint is a genuine instantiation of what the parties agreed to. If the text is ambiguous or the sovereigntist reading bends it, the reading''s legitimacy depends on political power to impose that interpretation, which would shift the constraint type toward tangled rope or snare (might-makes-right interpretation mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_reading_choice, empirical, 'Whether the Paris text textually supports the sovereigntist reading or whether the reading is a creative reframing serving state interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(pari_tr_t25, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(pari_be_t25, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 25, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 15, 0.16).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(pari_su_t25, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 25, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.12).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% The Paris Article 4 NDC kernel generates three structurally distinct constraints instantiated by three live readings. The sovereigntist reading (this constraint) operationalizes NDCs as voluntary coordination preserving national energy sovereignty; ε=0.31, type=rope. The supranational reading would operationalize NDCs as binding commitments on a ratcheting trajectory with enforcement; ε would be higher, type would be tangled rope or snare. The equity reading would operationalize NDCs as differentiated obligations grounded in historical responsibility and capacity; it would identify different beneficiary/victim sets and shift stakeholder directionality. The three constraints are linked by network.affects_constraints: they share the same kernel text but instantiate different structural readings. The sovereigntist reading influences the supranational and equity readings by setting the baseline (low-enforcement architecture); those readings must argue against the sovereigntist frame to gain acceptance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
