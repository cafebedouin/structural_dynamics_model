% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Early Modern Print Coordination Arrangement (Co-Constitution Reading)
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   Between the Mainz Bible (c. 1455) and the consolidated confessional
 *   churches (c. 1600), a communication technology and a religious movement
 *   co-evolved: the press supplied affordances (cheap reproduction,
 *   standardization, speed across distance), reformers supplied strategy
 *   (vernacular scripture, pamphlet polemic, woodcut satire timed to trade
 *   fairs), and neither side's contribution fixes the other's. This story
 *   instantiates the co-constitution reading of the
 *   technology_reformation_causality kernel: the press enabled without
 *   determining, and reformers shaped what the press produced while the
 *   medium reshaped what they could say. Epsilon's referent is the standing
 *   print-Reformation arrangement itself, assessed by this reading's lights:
 *   bidirectional causality, with costs arising from the interaction term
 *   (what the pamphlet economy did to discourse) rather than from the
 *   technology or the agency alone. Per the epsilon-invariance principle, the
 *   period's enforcement machinery (imperial bans, civic licensing, the
 *   Index) is excluded from this story's epsilon and reserved for a companion
 *   censorship-regime story; the reformers' post-consolidation
 *   print-dependence is flagged as a candidate piton-class sibling. The claim
 *   and the metrics are authored independently: if the engine computes a more
 *   extractive type from the victim declarations, that divergence measures
 *   exactly the interaction-term cost this reading acknowledges. KEY AGENTS
 *   (by structural relationship): - printers_publishers: commercial
 *   coordinator of the channel (organized/constrained) — captures the
 *   surplus, bears compliance and competition costs -
 *   reform_movement_leaders: primary content-side beneficiary
 *   (powerful/identity_locked) — shaped output to the medium and became
 *   dependent on it - literate_lay_readers: mass beneficiary with diffuse
 *   indirect costs (moderate/mobile) - territorial_rulers: dual-positioned
 *   administrator-beneficiary (institutional/arbitrage) — harnessed,
 *   licensed, and steered the channel - catholic_church_authorities:
 *   dispossessed gatekeeper turned counter-participant
 *   (institutional/constrained) - manuscript_scriptoria_workers: displaced
 *   predecessor economy (powerless/trapped) - radical_reformation_groups:
 *   excluded from the channel, persecuted through its products
 *   (powerless/trapped) - historians_of_the_transition: analytical observer —
 *   models the co-evolution five centuries on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.35).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.2).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Early Modern Print Coordination Arrangement (Co-Constitution Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history of technology / religious history / media studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '1d5a2a35-1acb-40cc-847f-1b9499656ea9').
narrative_ontology:cs_kernel_codification('1d5a2a35-1acb-40cc-847f-1b9499656ea9', distributed).
narrative_ontology:cs_authority_grounding('1d5a2a35-1acb-40cc-847f-1b9499656ea9', expertise).
narrative_ontology:cs_interpretation_layer_present('1d5a2a35-1acb-40cc-847f-1b9499656ea9').
narrative_ontology:cs_reading_relation('1d5a2a35-1acb-40cc-847f-1b9499656ea9', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('1d5a2a35-1acb-40cc-847f-1b9499656ea9', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_axiom('1d5a2a35-1acb-40cc-847f-1b9499656ea9', foundational, causal_weight_is_interaction_term).
narrative_ontology:cs_axiom_status(causal_weight_is_interaction_term, holdable).
narrative_ontology:cs_axiom_grounding('1d5a2a35-1acb-40cc-847f-1b9499656ea9', causal_weight_is_interaction_term, empirically_contingent).
narrative_ontology:cs_axiom('1d5a2a35-1acb-40cc-847f-1b9499656ea9', secondary, medium_affordances_shape_without_fixing_outcomes).
narrative_ontology:cs_axiom_status(medium_affordances_shape_without_fixing_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('1d5a2a35-1acb-40cc-847f-1b9499656ea9', medium_affordances_shape_without_fixing_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('1d5a2a35-1acb-40cc-847f-1b9499656ea9', mutual_shaping_equilibrium).
narrative_ontology:cs_drift_state('1d5a2a35-1acb-40cc-847f-1b9499656ea9', contemporary_revisionist_historiography, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('1d5a2a35-1acb-40cc-847f-1b9499656ea9', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printers_publishers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reform_movement_leaders).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, literate_lay_readers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, territorial_rulers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, manuscript_scriptoria_workers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, radical_reformation_groups).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, catholic_church_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, printers_publishers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, literate_lay_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invest capital in presses, type, paper, and apprentices; sell sheets and books into a rapidly widening market. Profits concentrate where demand runs hottest: controversy, scripture, scandal. Bear the risks of unsold stock, pirated editions, and official displeasure; pay licensing fees where authorities require them. A shop can move between cities, but the trade's capital costs and skill specificity tie its owners to the channel.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printers_publishers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, printers_publishers, payer).

% Write for the pamphlet and the cheap quarto, timing publications to trade-fair calendars and cultivating printer partnerships. Reach audiences no pulpit could hold. Shape doctrine to what the short vernacular format carries well, and discover that the same commercial logic which spreads their message also floods the market with rivals' replies and distortions no one can recall. Their movement's reach is now inseparable from channels they do not own; the movement's identity and the medium's rhythms have grown into each other.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reform_movement_leaders, beneficiary,
    powerful, generational, identity_locked, continental).

% Buy or borrow pamphlets, broadsheets, and Bibles at prices far below manuscript rates; read in taverns, workshops, and households; read aloud to neighbors who cannot. Pay book prices directly and, less visibly, absorb a public conversation increasingly organized around attack and reply rather than slow synthesis. What to read remains a genuine choice; oral and visual channels stay open alongside the printed word.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, literate_lay_readers, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, literate_lay_readers, payer).

% Recruit printers to their capitals with privileges and tax exemptions; run edicts, tax schedules, and confessional standards through the new channel; grant monopolies for revenue and leverage; ban titles when doctrine or diplomacy demands. Gain an administrative instrument earlier centuries lacked, and spend steady attention deciding which books may live. Sovereign discretion lets them play printers, rivals, and neighboring confessions against one another.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, territorial_rulers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, territorial_rulers, agenda_setter).

% Lose a communication monopoly held for centuries: rebuttals now arrive printed, cheap, and everywhere at once. Respond with bans, indexes, licensing regimes, and their own presses, fighting inside a medium they once gated. Carry the recurring cost of a permanent counter-propaganda establishment, and cannot withdraw from the channel without conceding it entirely.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, catholic_church_authorities, payer,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, catholic_church_authorities, agenda_setter).

% Watch orders for hand-copied books collapse as print undercuts price and matches fidelity. Monastic copyshops fall quiet; lay scriveners take what compositor and proofreader work they can find. Skills built over lifetimes price out of the market within a generation, and vows or geography pin many in place while the trade moves on.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, manuscript_scriptoria_workers, payer,
    powerless, biographical, trapped, regional).

% Find both major confessions willing to print against them and neither willing to print for them. Circulate doctrine by manuscript, hymn, and memorized text while official edicts citing printed arguments authorize their persecution. Exclusion from the channel is maintained by the same authorities who police what the channel carries.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, radical_reformation_groups, payer,
    powerless, biographical, trapped, regional).

% Reconstruct the interaction five centuries on from imprints, inventories, privileges, and correspondence; model how the medium's affordances and the actors' strategies shaped each other without assigning the whole outcome to either side alone.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, historians_of_the_transition, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, printers_publishers).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of reproducing and distributing identical texts across distance at collapsing marginal cost: doctrine, news, and polemic reach dispersed actors faster than authorities can respond, and standardized editions let scattered groups act on the same words.
% TRANSFER_FUNCTION: Moves cheap reproduced text, and the attention and legitimacy it carries, from printers and authors to dispersed readers; moves money from readers and patrons to printers; moves discursive initiative from clerical gatekeepers to whoever can write for the format.
% ABSENT_VOICES: Radical reformation groups would object loudest and are structurally outside the channel they are persecuted through; the illiterate oral majority is shaped by the printed conversation but seated nowhere in it; manuscript-trade workers had no voice in the economy displacing them.
% DISAPPEARANCE_RATIONALE: Remove the print coordination arrangement around 1520 and the reform movement's spread reverts to manuscript and pulpit speed: disputed theses circulate among clerics for decades instead of weeks, vernacular Bibles stay luxury objects, the pamphlet war never happens, and confessional boundaries harden along slower, more regional lines. Rulers lose an administrative instrument; the Church's rebuttal problem shrinks to manageable scale. Nearly every seat's strategy presupposed the channel, so its removal rearranges all of them.
% FOUNDING_PROBLEM: Reproducing a text beyond a few hundred copies required months of skilled scribal labor, with error compounding across copies; wide coordination on shared written formulations was bounded by copy speed, cost, and fidelity. The press arrangement formed to solve reproduction cost and fidelity at scale.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the direct beneficiary set: municipal councils that recruited printers with exemptions for civic record-keeping; Catholic authorities whose counter-Reformation print campaigns concede the channel's reach by participating in it; Erasmus's published praise of corrected editions attests the fidelity half. Contemporary dispute concerned control of the solution, not the reality of the problem.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.35 reflects the interaction term: the coordination itself ran near the inherent cost of continental-scale information infrastructure, but the pamphlet economy layered real costs on top — polemic premiums, crowded-out synthesis, attention levied on readers — peaking with the 1520s-1540s boom and plateauing as competition eroded printer rents. Suppression 0.20 is deliberately low because the enforcement layer (bans, licensing, the Index) is excluded from this story's referent per the epsilon-invariance principle; the coordination arrangement spread because it worked, not because it coerced, and the censorship regime is a companion constraint with its own epsilon. Theater_ratio 0.18: overwhelmingly functional throughout, with a late-interval rise as sanctioned republication rituals thickened around settled orthodoxies. Accessibility_collapse 0.55: manuscript book production collapsed commercially once print's advantage was legible, but oral, visual, and manuscript niches persisted — alternatives bent, they did not vanish. Resistance 0.45: resistance concentrated on content, which is the companion constraint's business, rather than on the channel itself; economic resistance from the manuscript trade was real but brief. All series share one eight-point grid, with every tracked metric authored at every point. No suppression_requirement series is authored because this story's enforcement picture is static by construction — the dynamic enforcement history belongs to the companion censorship story.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently from identical structural data. From the printer's bench the arrangement is opportunity wrapped in risk: the demand curve that enriches also punishes unsold orthodoxy. From the reformer's study it is providence with a bill attached: unprecedented reach, purchased with message control ceded to commercial print cycles. From the pew it is emancipation with a slant: scripture finally affordable, delivered increasingly as attack-and-reply. From the curia it is dispossession: a gate guarded for centuries now open by default. From the prince's chancery it is simply useful. Nothing in the structure changes across these seats; directionality and exit do.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for printers, reformers, readers, and rulers; victim declarations drive high d for scriptoria workers, radical groups, and Church authorities. Two overrides correct derivations the structural lists cannot see. reform_movement_leaders (powerful) derive near-full-beneficiary d from their beneficiary listing, but their atrophied pre-print repertoires and medium-fused identity give them real target-side exposure — commercial print logic disciplines what they can say — so d is overridden to 0.30. printers_publishers (organized) likewise derive near-full-beneficiary d, but compliance costs, piracy, and competitive erosion shave the subsidy, so d is overridden to 0.20. Catholic authorities stay derivation-driven: their victim listing yields high d, correctly capturing dispossession, and no single per-power-atom override could separate them from the equally institutional rulers without corrupting the rulers' beneficiary-side d.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the press as rope guards against the two mislabels the sibling readings invite. The determinism reading's implicit story — an irresistible force sweeping actors along — would dress a coordination success as natural law and excuse the era's winners from scrutiny; the pure-agency reading would dissolve the structure into biography and miss how the medium disciplined its own users. The rope claim keeps the coordination function visible while the interaction-term epsilon registers real costs, preventing coordination from being misread as pure extraction (which would erase why every seat stayed in) and extraction from being misread as coordination (which would erase the scribes, the silenced radicals, and the Church's dispossession). On obsolescence: the founding problem stayed live across the whole interval — no sunset applied, and the arrangement transformed rather than expired. The rising theater_ratio tail (0.04 to 0.18) is the early signature of the post-consolidation drift the reformer_dependency omega tracks; if that drift completes, the successor arrangement, not this one, is the inertial remnant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interaction_term_separability,
    'Can the interaction-term costs of the print-Reformation co-evolution — the pamphlet economy''s bias toward polemic, the crowding out of slow synthesis, the attention levied on readers — be analytically separated from the coordination benefit, or are they constitutive of how the medium coordinates?',
    'Comparative genre and tone analysis of print-era versus manuscript-era religious discourse; natural experiments where print arrived without reform uptake (much of Italy) or reform preceded dense print (early Swiss cantons), isolating what the interaction itself contributes.',
    'If separable, this story''s epsilon overstates the coordination arrangement''s own cost and the bias belongs to a rider constraint; if constitutive, epsilon properly sits inside this story and the rope reading must absorb it as inherent coordination cost above the Boltzmann floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interaction_term_separability, conceptual, 'Whether the interaction-term costs are separable from the coordination benefit.').

omega_variable(
    enablement_determination_boundary,
    'Where does enablement end and determination begin: is the tight coupling of print expansion and reform spread evidence of agency navigating an affordance space, or of structural determination that agency merely colored?',
    'Systematic comparison of regions matched on print penetration but divergent in reform adoption, and of adoption-lag distributions against press-arrival dates; adjudication draws on the sibling readings'' evidential bases as much as on this one.',
    'Resolution toward determination transfers causal weight to the technological_determinism_reading''s structure, whose inevitability claims behave mountain-like; resolution toward unconstrained agency collapses this reading toward beneficiary_agency_reading and strips the interaction term of independent weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enablement_determination_boundary, conceptual, 'The contested boundary between enablement and determination across the kernel''s readings.').

omega_variable(
    reformer_dependency_piton_candidate,
    'Does the reform movement''s print-dependence — atrophied pre-print repertoires, message discipline surrendered to commercial print cycles, late-century orthodoxy-reproduction rituals — constitute a distinct constraint warranting its own story?',
    'Trace post-1555 Wittenberg and Geneva print output for the shift from persuasive innovation to ritualized reproduction (catechism reprint cycles, anniversary editions, sanctioned-anthology dominance); test whether the reformers'' strategy space contracted measurably once territorial churches consolidated.',
    'If confirmed, decompose into an inertial sibling linked to this story; this story''s epsilon and theater_ratio drop accordingly, sharpening the coordination reading of the channel layer itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_dependency_piton_candidate, empirical, 'Candidate decomposition: reformer print-dependence as a separate inertial constraint.').

omega_variable(
    censorship_layer_boundary,
    'Is the enforcement layer around print — imperial bans, city licensing, the Index — part of this coordination arrangement or a distinct overlapping constraint?',
    'Apply the epsilon-invariance test directly: if assessing the arrangement with and without the enforcement layer yields materially different epsilon and different victim sets, they are two constraints and must be authored separately.',
    'This story already excludes enforcement (hence low suppression and no suppression_requirement series); confirmation mandates a companion censorship-regime story carrying the enforcement epsilon, with network edges running in both directions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(censorship_layer_boundary, conceptual, 'Boundary decision: enforcement layer inside or outside this constraint''s referent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trc_cocon_tr_t1450, technology_reformation_causality__co_constitution_reading, theater_ratio, 1450, 0.04).
narrative_ontology:measurement_basis(trc_cocon_tr_t1450, observed).
narrative_ontology:measurement(trc_cocon_tr_t1470, technology_reformation_causality__co_constitution_reading, theater_ratio, 1470, 0.05).
narrative_ontology:measurement_basis(trc_cocon_tr_t1470, observed).
narrative_ontology:measurement(trc_cocon_tr_t1490, technology_reformation_causality__co_constitution_reading, theater_ratio, 1490, 0.07).
narrative_ontology:measurement_basis(trc_cocon_tr_t1490, observed).
narrative_ontology:measurement(trc_cocon_tr_t1510, technology_reformation_causality__co_constitution_reading, theater_ratio, 1510, 0.08).
narrative_ontology:measurement_basis(trc_cocon_tr_t1510, observed).
narrative_ontology:measurement(trc_cocon_tr_t1530, technology_reformation_causality__co_constitution_reading, theater_ratio, 1530, 0.11).
narrative_ontology:measurement_basis(trc_cocon_tr_t1530, observed).
narrative_ontology:measurement(trc_cocon_tr_t1550, technology_reformation_causality__co_constitution_reading, theater_ratio, 1550, 0.13).
narrative_ontology:measurement_basis(trc_cocon_tr_t1550, observed).
narrative_ontology:measurement(trc_cocon_tr_t1570, technology_reformation_causality__co_constitution_reading, theater_ratio, 1570, 0.16).
narrative_ontology:measurement_basis(trc_cocon_tr_t1570, observed).
narrative_ontology:measurement(trc_cocon_tr_t1600, technology_reformation_causality__co_constitution_reading, theater_ratio, 1600, 0.18).
narrative_ontology:measurement_basis(trc_cocon_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(trc_cocon_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.14).
narrative_ontology:measurement_basis(trc_cocon_be_t1450, observed).
narrative_ontology:measurement(trc_cocon_be_t1470, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1470, 0.17).
narrative_ontology:measurement_basis(trc_cocon_be_t1470, observed).
narrative_ontology:measurement(trc_cocon_be_t1490, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1490, 0.21).
narrative_ontology:measurement_basis(trc_cocon_be_t1490, observed).
narrative_ontology:measurement(trc_cocon_be_t1510, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1510, 0.24).
narrative_ontology:measurement_basis(trc_cocon_be_t1510, observed).
narrative_ontology:measurement(trc_cocon_be_t1530, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1530, 0.34).
narrative_ontology:measurement_basis(trc_cocon_be_t1530, observed).
narrative_ontology:measurement(trc_cocon_be_t1550, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1550, 0.37).
narrative_ontology:measurement_basis(trc_cocon_be_t1550, observed).
narrative_ontology:measurement(trc_cocon_be_t1570, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1570, 0.35).
narrative_ontology:measurement_basis(trc_cocon_be_t1570, observed).
narrative_ontology:measurement(trc_cocon_be_t1600, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1600, 0.35).
narrative_ontology:measurement_basis(trc_cocon_be_t1600, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__co_constitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, global_infrastructure).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% Constraint family for the technology_reformation_causality kernel. This file instantiates co_constitution_reading with epsilon indexed to the interaction term (bidirectional causality). The censorship/licensing layer is deliberately excluded from this story's epsilon per the epsilon-invariance principle — measuring the coordination arrangement with enforcement included yields a different epsilon, hence a different constraint; a companion story (confessional censorship regime) should carry it. Likewise the reformers' post-consolidation print-dependence (atrophied pre-print repertoires, orthodoxy-reproduction cycles) is a candidate inertial sibling (see omega reformer_dependency_piton_candidate). Edges to the two sibling readings carry the kernel's logical structure: this reading's core premises negate both siblings' core premises within any single framework, while all three persist as live positions across the profession.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, powerful, 0.3).
constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
