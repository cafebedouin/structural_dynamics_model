% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Binding Referee Arrangement (Binding-Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This story authors the binding-referee reading of WTO dispute settlement
 *   as a clean, epsilon-invariant constraint: panels issue rulings that bind,
 *   compliance obligations rest on negotiated treaty law, and within covered
 *   domains member states have surrendered the discretion to treat adverse
 *   findings as optional policy input. Non-compliance is a treaty violation
 *   carrying authorized consequences, not a sovereign choice. The arrangement
 *   coordinates genuine collective action — it is the only reason trade
 *   disputes among unequals resolve procedurally instead of punitively —
 *   while the same machinery extracts policy autonomy asymmetrically,
 *   concentrating enforcement pressure on members who cannot retaliate in
 *   kind. KEY AGENTS (by structural relationship): dsb_panel_apparatus —
 *   agenda setter issuing and tracking rulings (institutional/constrained);
 *   large_trading_powers — primary beneficiary with dual payer exposure,
 *   enforcing access against rivals while absorbing rulings against
 *   themselves (institutional/constrained); export_industry_lobbies — pure
 *   beneficiaries collecting enforceable predictability (organized/mobile);
 *   least_developed_members — primary targets bearing full bindingness with
 *   minimal retaliation capacity (powerless/trapped);
 *   domestic_regulatory_bodies — targets whose protective instruments are
 *   struck down (institutional-national/constrained);
 *   public_interest_advocates — excluded voice (moderate/constrained);
 *   trade_law_analysts — analytical observer (analytical/analytical). This
 *   file is one member of a three-story kernel family; the sibling readings
 *   are separate constraints with their own epsilon values and are linked via
 *   network edges, not folded into this classification.
 *
 * KEY AGENTS:
 *   - dsb_panel_apparatus: agenda setter — issues rulings, reviews compliance, authorizes countermeasures (institutional/constrained)
 *   - large_trading_powers: primary beneficiary with secondary payer position — enforce access claims abroad, absorb rulings at home (institutional/constrained)
 *   - least_developed_members: primary target — full compliance obligations without symmetric enforcement capacity (powerless/trapped)
 *   - domestic_regulatory_bodies: target — health, safety, and environmental instruments exposed to strike-down (institutional/constrained)
 *   - export_industry_lobbies: pure beneficiary — collects enforceable market predictability, bears no compliance exposure (organized/mobile)
 *   - public_interest_advocates: excluded — would contest the subordination of public protections, has no seat (moderate/constrained)
 *   - trade_law_analysts: analytical observer — sees the full negotiated-versus-interpreted obligation structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.62).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.74).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Referee Arrangement (Binding-Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '451a4334-d531-4c00-a5e9-c6b16afe742e').
narrative_ontology:cs_kernel_codification('451a4334-d531-4c00-a5e9-c6b16afe742e', fixed_text).
narrative_ontology:cs_authority_grounding('451a4334-d531-4c00-a5e9-c6b16afe742e', lineage).
narrative_ontology:cs_interpretation_layer_present('451a4334-d531-4c00-a5e9-c6b16afe742e').
narrative_ontology:cs_reading_relation('451a4334-d531-4c00-a5e9-c6b16afe742e', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('451a4334-d531-4c00-a5e9-c6b16afe742e', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('451a4334-d531-4c00-a5e9-c6b16afe742e', foundational, rulings_bind_via_negotiated_treaty_law).
narrative_ontology:cs_axiom_status(rulings_bind_via_negotiated_treaty_law, holdable).
narrative_ontology:cs_axiom_grounding('451a4334-d531-4c00-a5e9-c6b16afe742e', rulings_bind_via_negotiated_treaty_law, conventional).
narrative_ontology:cs_axiom('451a4334-d531-4c00-a5e9-c6b16afe742e', secondary, discretion_surrender_is_enforceable_market_access_exchange).
narrative_ontology:cs_axiom_status(discretion_surrender_is_enforceable_market_access_exchange, holdable).
narrative_ontology:cs_axiom_grounding('451a4334-d531-4c00-a5e9-c6b16afe742e', discretion_surrender_is_enforceable_market_access_exchange, instrumental).
narrative_ontology:cs_reference_frame('451a4334-d531-4c00-a5e9-c6b16afe742e', negotiated_treaty_binding_framework).
narrative_ontology:cs_drift_state('451a4334-d531-4c00-a5e9-c6b16afe742e', post_appellate_body_paralysis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('451a4334-d531-4c00-a5e9-c6b16afe742e', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, large_trading_powers).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_industry_lobbies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, least_developed_members).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_regulatory_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, large_trading_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes panels, circulates rulings, and runs the monthly meetings at which compliance status is reviewed and retaliation is authorized. Its authority rests on the treaty text and on members continuing to submit disputes and honor rulings. When a large member blocks appointment of adjudicators, the apparatus cannot convene its top review tier and must operate through substitutes; it cannot compel any government to comply, only certify non-compliance and open the door to authorized countermeasures.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, dsb_panel_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Initiate the largest share of disputes, enjoy enforceable market-access claims against smaller partners, and shape interpretive precedent through the volume and sophistication of their litigation. Simultaneously, their own subsidies, safeguard measures, and regulatory choices are the most frequent targets of rulings, and they absorb the largest absolute exposure to authorized retaliation. Leaving the system would forfeit the access guarantees they extend and enjoy, so they remain inside while selectively resisting organs they find inconvenient.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, large_trading_powers, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, large_trading_powers, payer).

% Receive predictable, enforceable access to foreign markets without paying the constraint's costs: their governments carry the compliance exposure, and the firms themselves can relocate supply chains or shift export destinations if a particular market closes. They press their capitals to litigate aggressively and to honor rulings abroad while lobbying for exceptions at home.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, export_industry_lobbies, beneficiary,
    organized, biographical, mobile, global).

% Accept the full weight of binding rulings with a fraction of the litigation capacity, legal staffing, and retaliation leverage of the large members. Their policy experiments in health, food security, and industrial policy are exposed to challenge, while their own ability to retaliate against a non-compliant large economy is close to nil, so the enforcement threat that disciplines others barely works in their favor. Exiting the system would mean losing preferential access they depend on; staying means absorbing obligations sized for economies they do not have. Occasional coalitions with similar members give episodic bargaining power but no standing enforcement capacity.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, least_developed_members, payer,
    powerless, generational, trapped, national).

% Health, food-safety, environmental, and industrial agencies whose protective measures can be struck down as trade-restrictive by a ruling their capital is treaty-bound to respect. They did not consent individually to the loss of instrument; the surrender happened at the level of trade ministries joining the treaty. Their recourse is redesigning measures to survive legal scrutiny, which consumes regulatory budgets and narrows the policy toolkit.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_regulatory_bodies, payer,
    institutional, biographical, constrained, national).

% Civil society, consumer, environmental, and development organizations argue that trade obligations routinely outrank public protections yet have no seat in the dispute process, which is closed to non-state parties except as filtered through member governments. They observe rulings after the fact and contest outcomes politically in national arenas where trade commitments constrain what their own governments may legislate.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, public_interest_advocates, excluded,
    moderate, biographical, constrained, global).

% Academic and institutional researchers who map the gap between negotiated text and accumulated interpretive obligation, track compliance delays by member size, and evaluate whether rulings track treaty law or drift past it. They bear neither the compliance exposure nor the access gains and can state the structure plainly.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, trade_law_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__binding_referee_reading, large_trading_powers).
narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, compulsory forum that converts trade conflicts among more than 160 sovereigns from tit-for-tat tariff wars into adjudicated procedures with defined timelines, evidentiary standards, and a legal route back to compliance — solving the collective-action problem that every dispute otherwise threatens general escalation.
% TRANSFER_FUNCTION: Moves policy discretion and regulatory autonomy from member-state governments — disproportionately from smaller members with weak retaliation capacity — toward a common adjudicative standard enforced by authorized countermeasures, and moves market predictability toward exporting industries and their home capitals.
% ABSENT_VOICES: Public-interest and civil-society advocates are structurally outside the process; affected domestic populations whose health, environmental, or food-security protections become the subject of rulings never appear as parties; smallest members without Geneva delegations participate nominally but rarely shape precedent.
% DISAPPEARANCE_RATIONALE: If binding rulings and their retaliation backing vanished overnight, disputes would revert to bilateral power politics and tariff spirals: large members would settle scores unilaterally, small members would lose the only venue where their grievances carry procedural weight, exporters would price political risk back into every market, and the broader preference architecture built on enforceable access would unravel into competing blocs.
% FOUNDING_PROBLEM: Interwar tariff warfare: the 1930s showed that unenforceable trade commitments collapse into retaliatory spirals, and the GATT decades showed that a dispute process any contracting party could block produces paper rights. The arrangement was built to make trade commitments credible by attaching consequences to violation.
% FOUNDING_PROBLEM_CORROBORATION: Economic history of the interwar tariff spiral corroborates the founding problem from outside the beneficiary set, as do developing-country negotiating archives recording why enforceable dispute settlement was the price of accepting deep liberalization. Continued demand is attested behaviorally: when the top review tier stopped functioning, dozens of members — including middle powers who gain little from the status quo — opted into a substitute appeal mechanism rather than abandon binding adjudication, revealing demand for the function independent of any single beneficiary's preferences.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claimed type, tangled_rope, reflects structural truth: the arrangement possesses BOTH a genuine coordination function (compulsory, rules-based conflict resolution that solves the tariff-spiral collective-action problem) AND asymmetric extraction routed through the same machinery (policy discretion surrendered in proportion to weakness, enforcement pressure inversely proportional to retaliation capacity), and it requires active enforcement (panel convocation, compliance review, countermeasure authorization). The metrics describe actual operation. Extractiveness 0.62: the sovereignty transfer is real and the obligation set has grown beyond what any member legislature voted clause-by-clause, but members receive enforceable access in return, tempering raw extraction below snare levels. Suppression 0.74 and unscaled by design: it records the raw structural closure of alternatives — Article 23 forbids unilateral action and forum shopping, exit means forfeiting the entire preference architecture, and defiance invites certified retaliation — and suppression is NOT scaled by power or scope; only extractiveness carries those modifiers in the engine. Accessibility collapse 0.58: within covered domains alternatives collapse sharply (no lawful route around the process), but bilateral deals, regional arrangements, and outright unilateralism persist as costly outside options, so collapse is substantial but incomplete. Resistance 0.62: a major member has actively paralyzed the top review tier for years, compliance delays are routine among the strong, and scholarly and political challenges to adjudicative authority are organized and ongoing. Theater ratio 0.30 and rising: panels genuinely adjudicate, but a growing share of activity is performative maintenance — monthly meetings certifying non-compliance everyone knows will stand, an appellate step that nominally exists while unable to convene, consensus rituals over outcomes already determined. The temporal series run on one shared grid (points 0-30 at intervals of 6) so every tracked metric is authored at every examined point. Extractiveness climbs gradually as negotiated rule-making froze and adjudication became the main channel of rule change; suppression_requirement climbs faster, modeling enforcement hardening — countermeasures normalizing, cross-retaliation extending the threat surface, and finally paralysis forcing reliance on unilateral pressure outside the machinery; theater accelerates late-interval as formal function outpaces real function.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is functioning rule of law: rulings issue, compliance is tracked, the machine works. From the least-developed-member seat the identical structure operates as enforced obligation without reciprocal protection — bound in proportion to weakness. From the large-power seat it is an instrument: decisive when aimed at rivals, resistable when returned to sender, which is precisely the dual beneficiary/payer position authored here. Export lobbies see only the predictability dividend; domestic regulators see only the lost instrument. The engine computes these per-seat classifications from the structural data; this story's claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive derivation. Least_developed_members and domestic_regulatory_bodies sit near the full-target end: victims with trapped or constrained exit, identity of policy autonomy implicated. Export_industry_lobbies sit near the beneficiary end: they collect the constraint's product while remaining mobile enough to arbitrage any single market's closure. Large_trading_powers derive mid-range rather than pure-beneficiary: their secondary payer role is structural, not incidental — they are the most frequently ruled-against members and the principal funders of the system they contest, and their constrained exit includes a distinctive lever (organ paralysis) that lets them reshape the constraint's operation without leaving it. The apparatus itself sits mildly beneficiary-side: it collects no rents, but its institutional existence is constituted by the arrangement's continuation. No directionality_overrides are authored: the derivation chain from roles plus exit options reproduces the true relationships here, and per-power-atom overrides would misfire badly in this story because three agents share the institutional atom with opposed structural positions — an override keyed to that atom would drag regulators and the apparatus along with the large powers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as a pure rope would erase the extraction: the sovereignty surrender is real, unevenly distributed, and enforced — small members fund the constraint's legitimacy while receiving a fraction of its protection. Reading it as a pure snare would erase the coordination: the tariff-spiral prevention function is genuine, demonstrably demanded, and irreplaceable at current scale — the founding problem is live, corroborated from outside the beneficiary set by interwar economic history and by members' revealed demand when the appellate tier failed. Tangled rope holds both facts in one structure. Mandatrophy is not resolved: the founding function has not atrophied into performance, and the arrangement is not yet a piton — parties still profit and suffer enough to fight over it actively, which is the opposite of inertial drift. The forward risk is legible in the measurement series: rising theater and hardening suppression with stagnant negotiated-rule renewal is the signature path by which a tangled rope degrades toward theatrical maintenance, and the interval-end values sit at the early edge of that trajectory, not in it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_binding_vs_advisory,
    'This story instantiates the binding_referee_reading of the wto_dsb_authority kernel: rulings bind, compliance obligations are treaty-grounded, and covered-domain policy discretion has been surrendered. Would the advisory_coordination_reading restructure the same arrangement into a fundamentally lighter constraint?',
    'Comparative classification of the sibling story: if compliance pressure is modeled as voluntary facilitation with ultimate discretion retained, epsilon falls sharply, suppression loses its enforcement backbone, and the seat classifications converge toward rope-range results.',
    'Under the sibling reading the arrangement''s extraction is largely reframed as chosen coordination cost; the tangled_rope reading of THIS story would not transfer, and the corpus would hold two structurally different constraints sharing one colloquial label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_binding_vs_advisory, conceptual, 'Reading-indexed classification: bindingness versus advisory facilitation as rival accounts of what DSB rulings are.').

omega_variable(
    interpretive_accumulation_share,
    'What share of the current obligation structure descends from explicitly negotiated treaty text versus accumulated through interpretive rulings that members never voted on?',
    'Clause-by-clause provenance audit of operative rulings: classify each enforced obligation by origin in negotiated text, ministerial decision, or adjudicative gloss.',
    'A high interpretive share shifts extraction from consented exchange toward unconsented rule creation, sharpening the extraction asymmetry and strengthening the case of the judicial_activism_reading sibling; a low share supports the treaty-grounding axiom this reading stands on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_accumulation_share, empirical, 'Negotiated-text versus adjudicative-gloss composition of the binding obligation set.').

omega_variable(
    retaliation_symmetry_effectiveness,
    'Does authorized retaliation discipline large and small violators symmetrically, or does the enforcement threat concentrate on small targets while large economies absorb, delay, or ignore countermeasures at acceptable cost?',
    'Compliance-delay distributions conditioned on initiator and target economic size; frequency of prolonged non-compliance by members with disproportionate retaliation immunity.',
    'If asymmetric, effective extraction for small members far exceeds the headline measure, widening seat-level divergence and pushing the weakest-member seats toward snare-range experience while large-power seats stay coordination-range.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retaliation_symmetry_effectiveness, empirical, 'Whether the retaliation mechanism enforces uniformly across power asymmetries.').

omega_variable(
    sovereignty_exchange_valuation,
    'Is the explicit trade of policy discretion for guaranteed market access a fair exchange, and for whom — member governments, domestic regulators, exporting industries, or the populations subject to the regulated protections?',
    'Not resolvable by data alone: the answer depends on how each seat weights regulatory autonomy against economic predictability, and populations whose protections are traded away were never offered the deal.',
    'Different valuations place the same arrangement anywhere from legitimate contract to imposed extraction; the classification of this story is indexed to the member-state exchange frame, and population-level valuation would redistribute the victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_exchange_valuation, preference, 'Value-dependence of the sovereignty-for-access exchange assessment across seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(wto__tr_t6, wto_dsb_authority__binding_referee_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(wto__tr_t12, wto_dsb_authority__binding_referee_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(wto__tr_t18, wto_dsb_authority__binding_referee_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(wto__tr_t24, wto_dsb_authority__binding_referee_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__binding_referee_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(wto__be_t6, wto_dsb_authority__binding_referee_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement(wto__be_t12, wto_dsb_authority__binding_referee_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(wto__be_t18, wto_dsb_authority__binding_referee_reading, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(wto__be_t24, wto_dsb_authority__binding_referee_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__binding_referee_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(wto__su_t6, wto_dsb_authority__binding_referee_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(wto__su_t12, wto_dsb_authority__binding_referee_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(wto__su_t18, wto_dsb_authority__binding_referee_reading, suppression_requirement, 18, 0.63).
narrative_ontology:measurement(wto__su_t24, wto_dsb_authority__binding_referee_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(wto__su_t30, wto_dsb_authority__binding_referee_reading, suppression_requirement, 30, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% Kernel family decomposition: the colloquial label 'WTO dispute settlement authority' covers three structurally distinct claims that share one institutional shell but differ in what rulings ARE. This story (binding_referee_reading) authors the binding arrangement with its treaty-grounded compliance pressure. The advisory_coordination_reading authors the same institutions as discretionary facilitation — lower epsilon, weaker suppression, coordination-dominated structure. The judicial_activism_reading authors the obligation-accumulation layer as illegitimate interpretive legislation — extraction located in drift rather than bindingness per se. Each file carries its own stable epsilon, beneficiaries, and victims; they are linked here because the binding reading's accumulating rulings are the raw material that feeds the activism critique, and because the advisory reading's discretion-retention premise is the direct negation of this file's foundational axiom. Upstream-downstream structure: the binding reading's operational record influences the conditions under which the sibling critiques gain or lose force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
