% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Print-Mediated Dissemination Channel of the Early Reformation
 *   domain: historical epistemology/religious history/political economy
 *
 * SUMMARY:
 *   The technological-mediation reading isolates one layer of the Reformation
 *   composite: the print-dissemination channel that, between roughly 1500 and
 *   1560, turned locally confined theological dissent into a continental mass
 *   movement. The standing arrangement under assessment is that channel as it
 *   actually operated - press shops, edition markets, privilege regimes, and
 *   the literacy base feeding them - assessed by this reading's own lights,
 *   which see a fixed material condition (the cost structure of mechanical
 *   reproduction) doing the enabling work that doctrine-centered and
 *   sovereignty-centered narratives attribute to their preferred causes.
 *   Family note: the sibling readings are separate constraint files with
 *   their own epsilon values over their own referents; this story neither
 *   hedges nor averages across them. KEY AGENTS (by structural relationship):
 *   - commercial_printers: Primary beneficiary with cost exposure
 *   (moderate/mobile) - operate the channel, collect edition revenue, bear
 *   capital and confiscation risk - reformist_pamphleteers: Primary
 *   beneficiary (moderate/constrained) - supply the content whose scale the
 *   channel makes possible - vernacular_lay_readers: Beneficiary base
 *   (powerless/mobile) - the demand side whose literacy and coin make the
 *   channel self-sustaining - territorial_authorities: Agenda-setter with
 *   incidental gain (institutional/constrained) - license, tax, and protect
 *   the trade inside their borders - catholic_censorial_apparatus: Excluded
 *   opponent (institutional/trapped) - holds condemnation powers but no seat
 *   in the channel it seeks to govern - manuscript_trade_custodians:
 *   Displaced cost-bearer (moderate/constrained) - lose their market to the
 *   channel without being extracted from by it - historians_of_the_book:
 *   Analytical observer (analytical/analytical) - sees the full structure
 *   across centuries
 *
 * KEY AGENTS:
 *   - - commercial_printers: Primary beneficiary with cost exposure (moderate/mobile) - operate presses, collect edition revenue, bear capital and confiscation risk
 *   - - reformist_pamphleteers: Primary beneficiary (moderate/constrained) - supply content whose continental scale only the channel makes possible
 *   - - vernacular_lay_readers: Beneficiary base (powerless/mobile) - demand side whose literacy and purchases sustain the trade
 *   - - territorial_authorities: Agenda-setter with incidental gain (institutional/constrained) - license, tax, and protect the trade territorially
 *   - - catholic_censorial_apparatus: Excluded opponent (institutional/trapped) - condemnation power without a seat in the channel
 *   - - manuscript_trade_custodians: Displaced cost-bearer (moderate/constrained) - outcompeted, not extracted from
 *   - - historians_of_the_book: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.12).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.14).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Print-Mediated Dissemination Channel of the Early Reformation").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical epistemology/religious history/political economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'b9274e9d-c478-4b23-b6ff-7ba3de6a9504').
narrative_ontology:cs_kernel_codification('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', distributed).
narrative_ontology:cs_authority_grounding('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', expertise).
narrative_ontology:cs_interpretation_layer_present('b9274e9d-c478-4b23-b6ff-7ba3de6a9504').
narrative_ontology:cs_reading_relation('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', foundational, dissemination_capacity_bounds_movement_scale).
narrative_ontology:cs_axiom_status(dissemination_capacity_bounds_movement_scale, holdable).
narrative_ontology:cs_axiom_grounding('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', dissemination_capacity_bounds_movement_scale, empirically_contingent).
narrative_ontology:cs_axiom('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', secondary, typographic_fixity_shapes_doctrinal_contestation).
narrative_ontology:cs_axiom_status(typographic_fixity_shapes_doctrinal_contestation, holdable).
narrative_ontology:cs_axiom_grounding('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', typographic_fixity_shapes_doctrinal_contestation, empirically_contingent).
narrative_ontology:cs_reference_frame('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', manuscript_dissemination_baseline).
narrative_ontology:cs_drift_state('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', post_augsburg_confessionalization, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b9274e9d-c478-4b23-b6ff-7ba3de6a9504', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, commercial_printers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, reformist_pamphleteers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, vernacular_lay_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, territorial_authorities).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, commercial_printers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, manuscript_trade_custodians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run workshops of presses, compositors, and warehouses in cities such as Basel, Strasbourg, Augsburg, and Antwerp. Revenue comes from edition runs sold at fairs and through colporteur networks; costs are paper, type metal, wages, and capital tied up in unsold stock. Some hold territorial privileges granting exclusive rights to lucrative texts; all face confiscation or expulsion when a magistrate turns hostile, and several moved shops across borders to keep printing. Leaving the trade means liquidating specialized equipment at a loss.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, commercial_printers, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, commercial_printers, payer).

% Write short vernacular tracts, sermons, and biblical commentaries whose reach now depends on print shops rather than pulpit appointment or university chair. A single quarto pamphlet can reach tens of thousands of readers in weeks; payment is irregular, often nothing beyond presentation copies and patronage favor. Personal security rests on a territorial lord's protection, so residence and publication are bound to jurisdictions where that protection holds.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, reformist_pamphleteers, beneficiary,
    moderate, biographical, constrained, continental).

% Urban artisans, merchants, parish clergy of modest rank, and educated women who buy or borrow pamphlets and New Testaments in their own language for a few coins. Access widens yearly as prices fall and town schooling raises literacy. Nothing binds them to a single author or confession; they stop buying titles that bore or frighten them, and their collective demand is the only pull the whole trade answers to.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, vernacular_lay_readers, beneficiary,
    powerless, biographical, mobile, continental).

% City councils and princes license presses, grant printing privileges, collect fees for them, and issue mandates on what may be printed or sold inside their jurisdiction. They weigh customs revenue and the propaganda value of a loyal press against diplomatic cost with the emperor and the bishoprics. Their regulatory grip stops at their own border, which makes uniform control of the trade impossible from any single seat.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, territorial_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, territorial_authorities, beneficiary).

% Papal legates, diocesan inquisitors, imperial diet majorities, and after 1559 the Index congregation, tasked with stopping proscribed books. They hold condemnation powers but no delivery infrastructure of their own: banning a title raises its clandestine value, and enforcement depends on magistrates who are often the very princes sheltering the presses. Withdrawing from the struggle would concede the field, so they cannot exit it.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, catholic_censorial_apparatus, excluded,
    institutional, generational, trapped, continental).

% Scriptoria, university stationers, and court copyshops whose livelihood rested on duplicating texts by hand. Each year of print expansion erodes their order books; their skills and equipment do not transfer to type-setting, and guild rules tie them to cities where demand for their product is collapsing.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, manuscript_trade_custodians, payer,
    moderate, biographical, constrained, regional).

% Modern scholars working from edition counts, library survival catalogs, and printer archives. They observe the whole structure across centuries, owe nothing to any sixteenth-century party, and revise their estimates as cataloguing projects digitize surviving inventories.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, historians_of_the_book, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, territorial_authorities).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Connected geographically dispersed critics of late-medieval religious practice into a synchronized discourse: identical texts reached hundreds of towns within weeks, turning a local university quarrel into a common reference point for artisans, clergy, and magistrates across language boundaries. Print also fixed doctrinal positions in citable, repeatable form, letting allies who never met coordinate on the same propositions and letting opponents attack the same wording.
% TRANSFER_FUNCTION: Moves textual content - and with it attention, legitimacy, and doctrinal authority - from a small Latin-literate clerical elite toward vernacular lay audiences; moves money from book buyers to printers, papermakers, and their investors; moves reputational capital toward authors whose names could travel on title pages; moves privilege fees from printers to the authorities who grant exclusive rights.
% ABSENT_VOICES: Pressroom wage laborers and piece-work authors left no guild seats or minutes; their grievances about wages and unpaid copy survive only incidentally in lawsuits and letters. On the opposing side, censorial authorities held diets and consistories but had no seat inside the mediation channel they sought to govern - their exclusion from effective control is structural, not incidental, and is what defines the 1517-1525 window.
% DISAPPEARANCE_RATIONALE: Remove the press in 1517 and the indulgence controversy stays a university quarrel: Luther's theses circulate in dozens of manuscript copies among academics instead of hundreds of thousands of pamphlets among laypeople. No vernacular Bible movement reaches scale, territorial princes lack the propaganda infrastructure that made confessional alignment durable, and the confessional map of Europe - which territories go Lutheran, Reformed, or stay Catholic - redraws almost entirely.
% FOUNDING_PROBLEM: Reproducing a text beyond a few dozen copies required months of scribal labor per hundred leaves; no message could outrun its own copying cost, so religious dissent stayed local or had to ride existing church machinery. Commercial printing was built to break exactly that reproduction bottleneck - among its first mass products were the very indulgence certificates the Wittenberg dispute attacked.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of print (the Febvre and Martin tradition; Pettegree's output estimates) attest from outside any benefiting party that the manuscript bottleneck was broken and never re-bound. Media economists counter that the underlying problem - cheap, faithful reproduction at scale - recurs with every medium shift, keeping the founding problem alive in transformed form. No account from inside the print trade is relied upon.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.12) because the channel's dominant flows are market payments for goods: readers pay for books, printers pay for paper and labor. The extractive residue is the privilege-and-licensing skim plus capital-gating returns, which thicken over the interval (series rises 0.05 to 0.12) as privileges and guild structures mature, but never dominate. Suppression (0.14) is the channel's OWN coercive force - guild gatekeeping, privilege exclusivity - deliberately not conflated with the censorship directed AT the channel, which belongs to the censorial apparatus's own story; suppression is a raw structural property and is not scaled by power or scope. Theater is minimal (0.10): output was overwhelmingly functional, with only paratextual puffery and patronage dedications as performance. Accessibility collapse is high (0.88) because for the specific function of continental-scale text dissemination, alternatives collapse almost completely once print is understood - no manuscript system matches its unit economics; this is the closure grade the mountain claim rests on. Resistance is near-zero (0.08): no constituency resisted the channel as such; even its enemies adopted it within a decade. Claim and metrics are authored independently: the reading claims a fixed physical condition; the metrics describe actual operation including its constructed fringes; where the engine's per-seat computation diverges from the mountain claim, that divergence is the datum. Both tracked series share one seven-point grid (t=0..60 maps 1500 to 1560) so every metric is authored at every examined time point. No suppression_requirement series is authored: the channel's internal enforcement picture is static across the interval, so the scalar in base_properties carries it.
 *
 * PERSPECTIVAL GAP:
 *   From the printer's bench the arrangement is opportunity: a new industry with fairs, privileges, and export markets. From the censor's office the same structure is an ungovernable leak: a condemnation machine with no delivery arm, facing opponents who own the delivery arm. From the pew it is cheap access to arguments in one's own language. From the scriptorium it is obsolescence arriving faster than skills can migrate. Four seats, one structure, four experienced realities; the engine computes these divergences from the power, exit, and role data, and the authored mountain claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place printers, pamphleteers, and readers near the beneficiary end of directionality: the channel subsidizes their reach, income, and access. No victims are declared, and this is deliberate: the manuscript trade's losses are competitive displacement, not transfer through the structure - nothing flows FROM scriptoria THROUGH the channel TO anyone. The censorial apparatus sits near the target end as an excluded opponent bearing heavy defensive costs it cannot exit. Territorial authorities derive a near-symmetric position from their dual agenda-setting and fee-collecting roles. No directionality overrides are used: the derivation from roles plus exit options captures these positions, and a single power-atom override would misapply across heterogeneous institutional seats (authorities and censors share the institutional atom but occupy opposite positions).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding bottleneck died quickly and completely, yet the arrangement persisted and expanded - the classic shape in which a coordination success is later misread in both directions: over-mountainized as immortal natural law when it is partly a maintainable network, or over-extractivized as hidden rent machinery when its dominant flows are market compensation. Keeping the claim (mountain, from the reading's own lights) and the metrics (low but nonzero extraction, drifting upward with privilege thickening) independent lets the FSM probe ask whether the beneficiary structure betrays a constructed regime beneath the natural-law presentation. The dead-or-contested founding problem combined with a world_rearranges verdict is here infrastructural completion rather than zombie capture: corroboration comes from economic historians outside the trade, and the persistence mechanism is capitalized equipment plus habituated demand, not enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_condition_vs_constructed_regime,
    'Is the print channel a fixed material condition (natural-law-like cost structure of mechanical reproduction) or a constructed deployment regime (privileges, capital barriers, network ownership) whose identifiable beneficiaries reveal design rather than nature?',
    'Decompose the observables: compare variance explained by the raw reproduction-cost curve (physics of movable type, paper supply) against regime variables (privilege grants, capital thresholds, guild restrictions). If regime variables dominate outcome variance, the mountain presentation is a false summit.',
    'If the constructed-regime component dominates, the FSM probe reclassifies toward tangled_rope and the ''fundamentally technological'' reading weakens materially; if the material component dominates, the mountain claim stands with beneficiaries as incidental riders on a fixed condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_condition_vs_constructed_regime, conceptual, 'Whether the press-as-constraint is natural law or constructed regime with beneficiaries.').

omega_variable(
    manuscript_counterfactual_ceiling,
    'Could the manuscript-and-pulpit system have carried the Wittenberg controversy to continental scale absent print, or was the reproduction bottleneck binding?',
    'Model manuscript-circulation ceilings from scriptoria output records and known copy-shop throughput against the observed 1517-1525 publication velocity (edition counts, surviving imprint data).',
    'If the manuscript ceiling plausibly suffices, the mediation reading loses causal centrality and weight shifts toward the theological and political siblings; if the ceiling binds by orders of magnitude, the technological layer is confirmed as the enabling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manuscript_counterfactual_ceiling, empirical, 'Counterfactual scale of the Reformation without print.').

omega_variable(
    literacy_causality_direction,
    'Did print create the vernacular reading public, or did a pre-existing literacy surge (urban schooling, devotional manuscript culture) create the demand that print served?',
    'Date literacy growth region-by-region against press density; use late-press regions (Scandinavia, parts of Italy) as comparison cases where demand-side conditions can be observed without supply.',
    'If demand led supply, the constraint is substantially social rather than technological, and classification shifts from a fixed material condition toward a maintained coordination network; if supply led, the reading''s supply-side framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_causality_direction, empirical, 'Direction of causation between print supply and literacy demand.').

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the technological_mediation_reading of kernel reformation_composite; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'No data resolves a framing choice; the disagreement is located at the causal-weight locus: this reading places it in the medium (dissemination capacity), theological_fragmentation_reading in the message (incompatible soteriological commitments), political_realignment_reading in the sovereign frame (fiscal and jurisdictional differentiation).',
    'Adopting a sibling relocates the beneficiary/victim structure onto different referents entirely - doctrinal offices and enforcement machinery for the theological sibling, princely fiscality and jurisdiction for the political sibling - producing different epsilon, different stakeholders, and different classifications; this file''s epsilon stays fixed over its own referent regardless.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings are separate constraints, not averaged alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_tech_med_tr_t0, reformation_composite__technological_mediation_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(reformation_tech_med_tr_t10, reformation_composite__technological_mediation_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(reformation_tech_med_tr_t20, reformation_composite__technological_mediation_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(reformation_tech_med_tr_t30, reformation_composite__technological_mediation_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(reformation_tech_med_tr_t40, reformation_composite__technological_mediation_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(reformation_tech_med_tr_t50, reformation_composite__technological_mediation_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement(reformation_tech_med_tr_t60, reformation_composite__technological_mediation_reading, theater_ratio, 60, 0.1).

% Extraction over time
narrative_ontology:measurement(reformation_tech_med_be_t0, reformation_composite__technological_mediation_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(reformation_tech_med_be_t10, reformation_composite__technological_mediation_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(reformation_tech_med_be_t20, reformation_composite__technological_mediation_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(reformation_tech_med_be_t30, reformation_composite__technological_mediation_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement(reformation_tech_med_be_t40, reformation_composite__technological_mediation_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(reformation_tech_med_be_t50, reformation_composite__technological_mediation_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(reformation_tech_med_be_t60, reformation_composite__technological_mediation_reading, base_extractiveness, 60, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(reformation_composite__technological_mediation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, global_infrastructure).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Reformation' conflates three structurally distinct claims and is decomposed into three stories. This file carries the technological-mediation layer with a low, stable epsilon (0.12) over the print-dissemination arrangement. The theological-fragmentation sibling carries its own epsilon over doctrinal-enforcement arrangements; the political-realignment sibling carries its own over fiscal-sovereignty arrangements. Edges run from this story to both siblings because the observables authored here (publication rates, literacy curves) are cited as evidence by the other two readings - upstream enables downstream, per the BGS family pattern. Each member keeps a single stable epsilon over its own referent; nothing is averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
