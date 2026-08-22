% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDCs — CBDR Equity Reading
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Paris Article 4 NDC kernel:
 *   the claim that nationally determined contributions are interpretable only
 *   through Common But Differentiated Responsibilities, with structural
 *   distinctions between developed and developing states treated as
 *   mandatory. Under this reading the standing arrangement is a
 *   differentiated bargain — developed states accept binding absolute targets
 *   and transfer obligations while developing states retain policy space and
 *   receive support, and the developing-country bloc holds a working veto
 *   over any move toward uniform or internationally enforced commitments. The
 *   epsilon referent is that standing differentiated arrangement as the
 *   equity reading itself assesses it — not the voluntarist regime the
 *   sovereigntist sibling would endorse and not the uniform ratchet the
 *   supranational sibling would impose; those are separate constraints in
 *   separate files. KEY AGENTS (by structural relationship): g77_china_bloc:
 *   agenda-setting coalition ([organized]/[constrained]) — drafts and defends
 *   the differentiation line, holds the consensus lever, and collects for its
 *   members; developed_state_parties: primary target
 *   ([powerful]/[constrained]) — bear binding absolute targets and the
 *   finance obligation; major_emerging_economies: principal beneficiary
 *   ([powerful]/[constrained]) — retain policy space and absorb the largest
 *   finance and technology shares; least_developed_countries: dependent
 *   beneficiary ([powerless]/[trapped]) — receive priority support, shape
 *   nothing; small_island_states: cross-pressured beneficiary
 *   ([organized]/[trapped]) — collect adaptation support while campaigning
 *   against the bloc line; developed_economy_energy_industries: secondary
 *   target ([organized]/[constrained]) — carry compliance costs and
 *   border-charge exposure; unfccc_secretariat: administrator
 *   ([institutional]/[constrained]) — applies the categories, decides
 *   nothing; ipcc_assessment_body: analytical observer
 *   ([institutional]/[analytical]) — supplies the factual premises every
 *   reading argues from; future_generations: absent party
 *   ([powerless]/[trapped]) — inherit the temperature outcome, hold no seat;
 *   frontline_climate_communities: absent party ([powerless]/[trapped]) —
 *   absorb impacts, represented only indirectly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.62).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.5).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDCs — CBDR Equity Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '299d7517-d06f-4b83-80c6-a7aa757724f9').
narrative_ontology:cs_kernel_codification('299d7517-d06f-4b83-80c6-a7aa757724f9', fixed_text).
narrative_ontology:cs_authority_grounding('299d7517-d06f-4b83-80c6-a7aa757724f9', lineage).
narrative_ontology:cs_interpretation_layer_present('299d7517-d06f-4b83-80c6-a7aa757724f9').
narrative_ontology:cs_reading_relation('299d7517-d06f-4b83-80c6-a7aa757724f9', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('299d7517-d06f-4b83-80c6-a7aa757724f9', paris_article_4_ndc__supranational_reading, forecloses).
narrative_ontology:cs_axiom('299d7517-d06f-4b83-80c6-a7aa757724f9', foundational, cbdr_structural_distinction_mandatory).
narrative_ontology:cs_axiom_status(cbdr_structural_distinction_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('299d7517-d06f-4b83-80c6-a7aa757724f9', cbdr_structural_distinction_mandatory, deontological).
narrative_ontology:cs_axiom('299d7517-d06f-4b83-80c6-a7aa757724f9', secondary, developed_leadership_transfer_obligation).
narrative_ontology:cs_axiom_status(developed_leadership_transfer_obligation, holdable).
narrative_ontology:cs_axiom_grounding('299d7517-d06f-4b83-80c6-a7aa757724f9', developed_leadership_transfer_obligation, instrumental).
narrative_ontology:cs_reference_frame('299d7517-d06f-4b83-80c6-a7aa757724f9', rio_firewall_differentiation).
narrative_ontology:cs_drift_state('299d7517-d06f-4b83-80c6-a7aa757724f9', contemporary_paris_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('299d7517-d06f-4b83-80c6-a7aa757724f9', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, major_emerging_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, small_island_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, g77_china_bloc).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_parties).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_economy_energy_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A negotiating coalition of more than 130 developing countries that acts as a bloc in the annual climate talks. It drafts and defends the interpretive line that national pledges must be weighed against each country's historical contribution to warming and its level of development, and it holds the consensus rule as leverage: nothing it regards as erasing the developed/developing divide passes. Its members receive finance, technology, and flexibility under the arrangements it defends, and any member weighing defection faces loss of bargaining weight and isolation within the group.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, g77_china_bloc, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, g77_china_bloc, beneficiary).

% The treaty body that convenes the negotiations, compiles and synthesizes each country's pledge submissions, and runs the transparency reviews. It applies the differentiation categories in its paperwork — which countries are invited to which funds, which reporting flexibilities apply — but decides nothing itself, and its continued existence depends on the regime it services.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% OECD-range governments that accept earlier and deeper cuts, economy-wide absolute targets, and the bulk of international climate finance. Domestic industries and voters carry the compliance costs, and finance lines compete with other budget priorities. Leaving the framework entirely, as one major emitter did for four years, carries diplomatic and trade costs, so exit is exercised rarely and reversibly.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_parties, payer,
    powerful, biographical, constrained, global).

% Fossil fuel producers, heavy manufacturers, and utilities headquartered in developed economies. They operate under the steepest national targets and, increasingly, under border-carbon charges applied to imports from countries without comparable pricing. Relocating production abroad lowers the domestic compliance bill but exposes them to those same border charges and to stranded assets, so relocation is partial and expensive.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_economy_energy_industries, payer,
    organized, biographical, constrained, global).

% Large, rapidly industrializing states — the biggest current emitters inside the developing grouping. They submit pledges framed as intensity or peaking trajectories rather than absolute economy-wide cuts, receive a large share of international climate finance and technology flows, and keep full discretion over domestic energy buildout. Stepping out of the grouping would mean accepting developed-economy obligations; staying means defending a classification that their own emissions statistics increasingly strain.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, major_emerging_economies, beneficiary,
    powerful, generational, constrained, global).

% Lowest-income states with negligible historical emissions. They receive priority access to adaptation finance, capacity-building, and reporting flexibilities, and depend on those flows for basic climate resilience. They have no realistic outside option: leaving the framework forfeits the support, and staying leaves them exposed to impacts driven overwhelmingly by others' emissions.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, least_developed_countries, beneficiary,
    powerless, immediate, trapped, regional).

% A coalition of island nations whose physical survival depends on total global warming levels. They collect adaptation and loss-and-damage support under the differentiated arrangements, but they also campaign inside the grouping for the largest emitters — including fellow developing members — to accept firmer targets, a position that puts them at odds with the bloc line they otherwise rely on.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, small_island_states, beneficiary,
    organized, biographical, trapped, regional).

% People not yet born who will live under whatever temperature outcome the current bargain produces. They hold no seat, vote, or delegation in the negotiations; their interests enter only through advocacy arguments made by others. Nothing in the arrangement gives them recourse if the balance struck proves inadequate.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Settlements on coastlines, drylands, and floodplains in both rich and poor countries that absorb floods, heat, and storms regardless of which governments signed what. They are represented only indirectly, through national delegations whose priorities may lie elsewhere, and migration is their main available response to impacts.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, frontline_climate_communities, excluded,
    powerless, immediate, trapped, local).

% The scientific assessment body whose carbon-budget and attribution findings supply the factual premises every reading of the treaty argues from. It takes no position on how burdens should be divided, but its reports repeatedly reshape what counts as a fair share.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, ipcc_assessment_body, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, major_emerging_economies).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps every major emitter inside a single climate framework by pricing participation according to capacity and historical responsibility: developed states accept earlier, deeper, economy-wide cuts and finance transfers; developing states accept participation, transparency, and eventual peaking in exchange for policy space and support. Without the differential pricing, neither the 1992 nor the 2015 round produced a universal agreement.
% TRANSFER_FUNCTION: Moves public climate finance (on the order of $100 billion per year pledged for 2020, under renegotiation toward a larger post-2025 goal), technology, and capacity from developed-state budgets to developing-state programs; moves regulatory discretion in the opposite direction, concentrating binding absolute targets on developed economies while developing states retain domestic energy-policy freedom; and distributes reputational obligation asymmetrically.
% ABSENT_VOICES: Future generations and frontline climate communities hold no seat anywhere in the party-driven process. Inside the developing grouping, dissenting members — island states pressing for firmer obligations on the largest emitters — are disciplined by bloc consensus. Developed-state legislatures and taxpayers feel the finance lines only indirectly, through budget politics conducted far from the negotiating room.
% DISAPPEARANCE_RATIONALE: If the differentiation requirement vanished overnight, the regime's core bargain collapses: developing-state parties would treat uniform expectations as a breach of the 1992 settlement and withhold participation or demand renegotiation, finance flows would lose their framing and stall, and the universal-participation architecture would have to be rebuilt around either pure voluntarism or uniform bindingness — each of which has already failed once (Kyoto's ratification crisis, Copenhagen's collapse).
% FOUNDING_PROBLEM: How to bring all states into a common climate regime when industrialized states caused most cumulative emissions and developing states needed room to grow — the deadlock the 1992 convention resolved by writing differentiation into the regime's foundation.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and OECD emissions statistics corroborate that the underlying asymmetry — cumulative historical responsibility and per-capita capability gaps — remains real, and developed-state ratifications of the 1992 convention attest the problem's existence from outside the benefiting parties. But the same external sources dispute whether the 1992 category boundaries still track that asymmetry now that the largest current emitter and several high-income states sit inside the developing grouping; no external source attests that the binary split as drawn remains the correct mapping.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.62 at interval end) because the costs are real but legally soft: the finance obligation lives in political commitments and 'shall/should' drafting rather than enforceable law, and the binding absolute targets on developed states are self-chosen in form even where they bind in practice. Suppression is moderate (0.50) and unscaled by construction — it records the raw foreclosure of alternatives: no sanctions exist, but consensus rules plus the demonstrated collapse of uniform designs (Copenhagen) plus costly, reversible exit make rival architectures hard to sustain. Theater (0.44) tracks pledge inflation: re-announced finance goals, provided-versus-mobilized accounting disputes, loan-counted-as-finance practices, and headline funds capitalized well below their announcements — below the substitution threshold, but the trend is steadily upward. Accessibility collapse (0.58) reflects that uniform-commitment alternatives demonstrably destroy the coalition, while border-adjustment and minilateral routes keep partial alternatives alive outside the treaty frame. Resistance (0.60) is sustained from both flanks: developed states resist expanding obligations (ratification failures, withdrawal episodes, unilateral border measures), and parts of the developing grouping resist the frozen binary from the other side. The temporal series run on one shared grid (t=0..30, five-year steps anchored at Rio 1992 = 0) with every tracked metric authored at every point; the extractiveness dip at t=10 is the post-ratification-crisis realization drop, not a change in the obligation structure.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the developed-state payer seat the arrangement operates as enforced asymmetry: binding targets plus open-ended transfers, exit priced high, alternatives foreclosed by consensus rules. From the major-emerging-economy seat the same structure operates as protected development space plus incoming resources. From the bloc seat it is a legitimate defense of a founding settlement. The two powerful seats sit at the same nominal power level with opposite directionalities — what separates them is not power but position relative to the differentiation line and the different price of exit (alliance and trade exposure for developed states, loss of classification benefits for emerging ones). The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (major emerging economies, LDCs, island states, the bloc) derive low directionality — the arrangement subsidizes them, and trapped or constrained exits matter mainly for how firmly they hold their position. Declared victims (developed-state parties, developed-economy energy industries) derive high directionality — they bear the binding targets and the transfers, with constrained rather than arbitrage-grade exit, which pushes them toward the full-target end. The bloc's dual position (it administers the line AND its members collect under it) keeps its directionality at the beneficiary end despite its agenda-setting power. One override is declared: the two institutional seats (secretariat, IPCC) are set to d=0.5 because they neither pay nor collect — they administer and measure — and the canonical fallback for the institutional power atom would otherwise guess at a structural relationship the story declares to be symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both standard misreadings. Calling this a snare ignores the documented coordination achievement: universal participation in a climate framework exists nowhere without the differential pricing, and the arrangement's founding problem remains materially live (attribution and capability gaps are measured facts). Calling it a rope ignores the documented asymmetry: identifiable seats pay persistently, identifiable seats collect, the gains concentrate, and fixing the asymmetry is prohibitively expensive under consensus rules. The founding problem is authored contested rather than dead, so no zombie flag fires; theater at 0.44 sits below the substitution threshold, keeping piton drift off the table for now — but the monotonic rise in theater across the whole interval is the number to watch, because pledge-and-review dynamics are exactly the mechanism by which a coordination shell outlives its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the Article 4 kernel governs NDC interpretation — the equity reading instantiated here, the sovereigntist reading (pledges as unconstrained national choice), or the supranational reading (pledges as a uniform binding ratchet with international accountability)?',
    'Observed COP decision practice, compliance-mechanism design, and the outcome of the coming commitment-cycle negotiations: whichever reading survives in operative decisions is the one the kernel instantiates.',
    'Under the sovereigntist sibling this constraint dissolves into national choice — differentiation becomes optional courtesy and extraction falls toward coordination-cost levels. Under the supranational sibling the victim set redistributes: developing states acquire binding ratchet obligations and the equity bloc loses its veto.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which of the three readings governs the Article 4 kernel.').

omega_variable(
    category_boundary_validity,
    'Do the fixed 1992 developed/developing categories still track the responsibility and capability asymmetry they were drawn from, given that the largest current emitter and several high-income states sit inside the developing grouping?',
    'Updated responsibility-and-capability indices and graduation criteria (cumulative emissions shares, per-capita income, current emission shares) applied to the grouping''s membership.',
    'If the boundary no longer tracks the asymmetry, the arrangement''s costs land on states its rationale no longer covers and its benefits flow to states it no longer protects — the structure drifts from justified redistribution toward misdirected extraction concentrated on the mismatched cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_boundary_validity, empirical, 'Whether the binary categories still map onto the underlying asymmetry.').

omega_variable(
    finance_delivery_gap,
    'Are the transfer obligations real deliveries or re-announced pledges — what share of reported climate finance is new, grant-based, and additional to prior development assistance?',
    'Independent audit of provided-versus-mobilized accounting, loan/grant composition, and overlap with pre-existing aid budgets.',
    'If reported delivery is systematically inflated, the theater ratio is understated and the arrangement''s coordination function is thinner than it appears, raising degradation risk; if delivery is genuine, the transfer half of the bargain is substantively live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_delivery_gap, empirical, 'Pledged versus delivered climate finance.').

omega_variable(
    dynamic_vs_static_differentiation,
    'Should differentiation be static (categories frozen at 1992) or dynamic (obligations graduating as capability and emissions grow)?',
    'Negotiation texts and party positions: whether graduation criteria enter operative decisions and whether major emerging economies accept review triggers tied to capability thresholds.',
    'A dynamic settlement converts this arrangement toward a transitional design with built-in obsolescence pressure; a static settlement entrenches the current asymmetry indefinitely and sharpens the category-validity dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dynamic_vs_static_differentiation, preference, 'Static versus evolving differentiation.').

omega_variable(
    bloc_cohesion_identity_lock,
    'Is the negotiating bloc''s cohesion interest-based (members defect when the bargain stops paying) or identity-locked (membership constitutive of members'' diplomatic self-conception)?',
    'Defection history under divergent member interests — oil exporters versus island states versus least-developed members — and whether the bloc sustains unified positions when internal splits become material.',
    'If cohesion is interest-based, the veto power backing this reading decays as finance expectations shift; if identity-locked, the bloc defends the differentiation frame even against its members'' material interests, stabilizing the arrangement beyond its payoff structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bloc_cohesion_identity_lock, empirical, 'Interest-based versus identity-locked bloc cohesion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__equity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__equity_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__equity_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__equity_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__equity_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(pari_tr_t25, paris_article_4_ndc__equity_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(pari_tr_t30, paris_article_4_ndc__equity_reading, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__equity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__equity_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__equity_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__equity_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__equity_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(pari_be_t25, paris_article_4_ndc__equity_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(pari_be_t30, paris_article_4_ndc__equity_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__equity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__equity_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__equity_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__equity_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__equity_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(pari_su_t25, paris_article_4_ndc__equity_reading, suppression_requirement, 25, 0.46).
narrative_ontology:measurement(pari_su_t30, paris_article_4_ndc__equity_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, resource_allocation).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Paris Article 4 NDCs'. The single treaty phrase covers three structurally distinct claims with different epsilon values and different victim sets: this equity reading (interpretation bounded by mandatory differentiation; developed states pay, developing states retain policy space), the sovereigntist reading (interpretation unconstrained; extraction near coordination-cost levels), and the supranational reading (uniform binding ratchet; victim set shifts toward developing states). Per the epsilon-invariance principle these are separate stories linked by network edges rather than one story with a measurement parameter; the upstream equity settlement constrains what the sibling readings can enact, since the bloc's consensus position gates any move toward either pole.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
