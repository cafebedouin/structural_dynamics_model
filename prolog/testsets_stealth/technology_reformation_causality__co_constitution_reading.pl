% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Press-Reformation Co-Constitution Arrangement (Co-Constitution Reading)
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   Between 1517 and 1555 the printing trade and the evangelical movement
 *   locked into mutual formation. Presses gave dispersed critics a channel no
 *   previous dissenter had; the movement's demand gave printers the largest
 *   growth market in the history of the trade up to that point; each reshaped
 *   the other - reformers wrote for the format printers could sell, printers
 *   decided what circulated regardless of what reformers authorized, and
 *   authorities built enforcement machinery that trailed both. The
 *   co-constitution reading holds that neither pole suffices: the press
 *   enabled without determining, the reformers shaped without controlling.
 *   This story instantiates that reading as one clean constraint over the
 *   standing press-Reformation arrangement; the determinist and agency
 *   readings are separate files with their own epsilon values. KEY AGENTS (by
 *   structural relationship): - commercial_printer_publishers: Primary rent
 *   recipient (organized/arbitrage) - collects sales revenue from
 *   confessional controversy, exits by routing around mandates -
 *   magisterial_reformers: Dual-positioned channel-holder
 *   (organized/identity_locked) - gains the medium, pays in lost doctrinal
 *   control - territorial_princes: Secondary beneficiary (powerful/mobile) -
 *   converts the arrangement into property and sovereignty -
 *   literate_urban_public: Diffuse beneficiary (organized/constrained) - buys
 *   access, bounded by local licensing - catholic_church_hierarchy: Primary
 *   payer (institutional/trapped) - bears displacement of authority and
 *   revenue - radical_reformers_anabaptists: Severest payer
 *   (powerless/trapped) - destroyed through the same channels they used -
 *   imperial_and_municipal_censors: Agenda-setter (institutional/constrained)
 *   - administers rules the trade routes around - modern_historians_of_print:
 *   Analytical observer (analytical/analytical) - adjudicates the causal
 *   question
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.46).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.68).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Press-Reformation Co-Constitution Arrangement (Co-Constitution Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history of technology / religious history / media studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '1368f03c-ffc4-42b2-b576-a330459d034c').
narrative_ontology:cs_kernel_codification('1368f03c-ffc4-42b2-b576-a330459d034c', distributed).
narrative_ontology:cs_authority_grounding('1368f03c-ffc4-42b2-b576-a330459d034c', expertise).
narrative_ontology:cs_interpretation_layer_present('1368f03c-ffc4-42b2-b576-a330459d034c').
narrative_ontology:cs_reading_relation('1368f03c-ffc4-42b2-b576-a330459d034c', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('1368f03c-ffc4-42b2-b576-a330459d034c', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_axiom('1368f03c-ffc4-42b2-b576-a330459d034c', foundational, causation_is_reciprocal_not_unidirectional).
narrative_ontology:cs_axiom_status(causation_is_reciprocal_not_unidirectional, holdable).
narrative_ontology:cs_axiom_grounding('1368f03c-ffc4-42b2-b576-a330459d034c', causation_is_reciprocal_not_unidirectional, empirically_contingent).
narrative_ontology:cs_axiom('1368f03c-ffc4-42b2-b576-a330459d034c', foundational, neither_medium_nor_agency_sufficient_alone).
narrative_ontology:cs_axiom_status(neither_medium_nor_agency_sufficient_alone, holdable).
narrative_ontology:cs_axiom_grounding('1368f03c-ffc4-42b2-b576-a330459d034c', neither_medium_nor_agency_sufficient_alone, empirically_contingent).
narrative_ontology:cs_axiom('1368f03c-ffc4-42b2-b576-a330459d034c', secondary, gatekeeping_atrophies_once_infrastructure_matures).
narrative_ontology:cs_axiom_status(gatekeeping_atrophies_once_infrastructure_matures, holdable).
narrative_ontology:cs_axiom_grounding('1368f03c-ffc4-42b2-b576-a330459d034c', gatekeeping_atrophies_once_infrastructure_matures, empirically_contingent).
narrative_ontology:cs_reference_frame('1368f03c-ffc4-42b2-b576-a330459d034c', co_constitutive_reciprocity_frame).
narrative_ontology:cs_drift_state('1368f03c-ffc4-42b2-b576-a330459d034c', contemporary_historiography, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1368f03c-ffc4-42b2-b576-a330459d034c', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, commercial_printer_publishers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, magisterial_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, territorial_princes).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, literate_urban_public).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, radical_reformers_anabaptists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, magisterial_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs a print shop in an imperial free city or university town. Between 1518 and 1525 pamphlets sell faster than compositors can set them; a single Wittenberg tract can outsell everything else the shop has produced that year. Shops print for whichever client pays - evangelical tracts, Catholic rebuttals, princely declarations, sometimes both sides in the same month. When an imperial mandate names their city, they shift titles to a false imprint or a partner shop across the border; the trade's dispersion makes pursuit expensive. Some printers are prosecuted and a few executed; most grow rich.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, commercial_printer_publishers, beneficiary,
    organized, biographical, arbitrage, continental).

% Writes the tracts and translations that fill the pamphlet market, protected by sympathetic princes. Gains a channel no dissenter before 1517 ever had: arguments reproduced in tens of thousands of copies within weeks. Pays for it in lost control - printers reprint, abridge, illustrate, and extend their works without permission; crude satires circulate under their names; by the mid-1520s the volume of unauthorized print exceeds anything they can review, and public denunciations of the flood change nothing. Leaving the medium would mean abandoning the movement that now exists only through it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, magisterial_reformers, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, magisterial_reformers, payer).

% Decides which presses operate in their territory, which clergy preach, and eventually which confession is established. Confiscated monastic property and redirected church fees fund courts and armies; print supplies the legitimating pamphlets. Can protect or expel printers at will and shifts alignment when dynastic interest dictates.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, territorial_princes, beneficiary,
    powerful, generational, mobile, national).

% Buys pamphlets, hangs broadsheets, reads aloud to neighbors in guild halls and taverns. Gains direct access to scripture and argument previously mediated by clergy. What reaches them is bounded by local council licensing and preacher politics; price puts regular purchasing beyond the poorest households.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, literate_urban_public, beneficiary,
    organized, biographical, constrained, regional).

% Holds a continent-wide teaching office built on controlled transmission: ordination, licensure, uniform liturgy. After 1517 its pronouncements are answered within weeks by printed rebuttal it cannot match for speed or price; its disciplinary instruments - mandates, bans, index lists - trail the trade they target. It responds by building its own print apparatus and eventually its own reformed discipline, inside the medium it cannot leave.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).

% Uses the same pamphlet channels to argue for believers' baptism and congregational independence, and is destroyed partly through them: printed tracts identify teachers and networks, furnish prosecutable evidence, and mark whole communities for both Catholic and Lutheran authorities. Flight moves the problem rather than solving it - the print trail and confessional registers follow. By 1555 the movement survives mainly underground.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, radical_reformers_anabaptists, payer,
    powerless, biographical, trapped, continental).

% Issues mandates, licenses printers, and inspects shops on behalf of emperor or city council. Enforcement capacity never matches the trade's geography: banning a title raises its price and its clandestine circulation; inspection finds what shops choose to display. By the 1540s the office's main output is the announcement of rules the trade has already learned to route around.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, imperial_and_municipal_censors, agenda_setter,
    institutional, generational, constrained, continental).

% Studies the exchange from four centuries' distance using imprints, shop inventories, correspondence, and seizure records. Adjudicates how much weight belongs to the presses, to the organizers, and to the interaction between them, publishing into a literature whose factions correspond to the rival causal accounts.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, modern_historians_of_print, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, commercial_printer_publishers).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a real collective-action problem: how dispersed critics of a continent-spanning hierarchy could coordinate - identical arguments, in vernacular, reproduced in thousands of copies, moving faster than any countermand. It standardized doctrine across hundreds of towns whose representatives never met, and let a movement exist without a central organization.
% TRANSFER_FUNCTION: Moves money from pamphlet and book buyers to printer-publishers and authors; moves religious authority from a clerical monopoly to literate laypeople, preachers, and princes; moves legitimacy and attention to whichever positions the trade elects to carry.
% ABSENT_VOICES: The illiterate majority - oral culture carried much of what print started, but its bearers had no seat in what got printed or licensed. Radical reformers entered print only to be prosecuted through it. Women read and financed pamphlets but rarely authored them. All are four centuries dead; they object only through the record.
% DISAPPEARANCE_RATIONALE: Remove the press-movement coupling overnight and dissent stays regional and containable on the Hussite pattern: Wittenberg lectures without the 1520s pamphlet flood produce a university quarrel, not a continental break; princes lose the legitimating medium for confiscating church property; the trade loses its largest growth market. Every named seat's situation changes.
% FOUNDING_PROBLEM: How religious dissent could scale beyond local preaching networks against a hierarchy with continent-wide enforcement reach - posed acutely after earlier movements (Wycliffe, Hus) were contained or crushed.
% FOUNDING_PROBLEM_CORROBORATION: No original party survives to attest anything; attestation is historiographical and split. Eisenstein and Pettegree, working from the analytical seat, attest the problem and its press-borne solution; confessionalization historians (Schilling, Reinhard) attest that the durable achievement was the settlement, not the break. No source outside the benefiting parties defends the arrangement persisting past its function - the benefiting parties being four centuries dead, corroboration necessarily runs through archives rather than interested testimony.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

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
 *   Scores are descriptive of the arrangement's operation, authored independently of the claim. Extractiveness 0.46: rents are real - pamphlet margins, licensed monopolies, legitimation of confiscated property - but the arrangement simultaneously subsidized nearly every literate participant; the epsilon this reading asserts lives in the interaction term, not in either pole. Suppression 0.68: enforcement machinery (mandates, licensing, visitations, index lists) hardened steadily across the interval yet never matched the trade's dispersion - a high requirement with porous effect. Theater 0.38: most activity stayed functional, but gatekeeping denunciation and unenforceable proclamation grew as shares of total activity. Accessibility collapse 0.35: manuscript, pulpit, and oral channels persisted throughout; the press added a channel rather than closing alternatives. Resistance 0.60: counter-print, evasion, urban bargaining, and eventual armed settlement. All three series share one six-point grid (0, 8, 15, 22, 30, 38, approximating 1517-1555): extraction arcs upward and settles with the Peace of Augsburg, theater climbs as gatekeeping atrophies, and the suppression requirement ratchets with confessionalization. The suppression series is authored because this story specifically tracks enforcement-capacity buildup, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the printer's seat the arrangement is a windfall market; from the church's seat it is dispossession arriving faster than any countermeasure; from the reformer's seat it is simultaneously the only possible movement-medium and the loss of doctrinal control; from the censor's seat it is unenforceable law. Same structure, four different computed experiences - the engine derives divergent per-seat classifications from the declared positions rather than averaging them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to concrete flows. Printers collect sales revenue; arbitrage-grade exit (false imprints, border shops, serving both confessions) pushes their derived d toward the subsidized end. Princes collect property and legitimation with mobile exit - similarly low d. The urban public buys access cheaply relative to value - low-to-moderate d. The church pays in authority and revenue with no exit from the communicative environment - high d, tempered by institutional power. Radical communities pay with their lives through the same channels - trapped, powerless, nearest the full-target end. Magisterial reformers are the deliberately ambiguous seat: declared beneficiary because the channel-flow dominates their position, carrying payer as secondary role because message-control loss is real; the derivation will place them lower-d than their lived position warrants, and no directionality override is authored because overrides key on power atoms and would collide with the princes, who share the 'powerful'/'organized' neighborhood with genuinely different positions. The reformers' identity-lock is professional-relational fusion: the movement exists only in print, so abandoning the medium dissolves the public self that leads it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two misreadings apart. Reading the arrangement as pure coordination erases where the gains landed (printer rents, princely confiscations) and who was destroyed through the medium (radical communities) - the victim declarations block that. Reading it as pure extraction erases that nothing comparable coordinated dispersed dissent before 1517 and that no party coerced the coupling into existence - the beneficiary declarations and low accessibility-collapse block that. The atrophying element is real but seated: reformer gatekeeping and censorial proclamation decayed toward performance while the commercial core kept functioning, which is why theater_ratio rises across the interval without the whole becoming inertial - the cost-asymmetry test fails for a whole-system piton verdict because printers and princes captured too much to walk away. On the genealogy question the arrangement did not outlive its function; it completed it and dissolved into the confessional settlement, which is why the interval closes at Augsburg rather than trailing into the seventeenth century.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the technology_reformation_causality kernel - what would the sibling readings change structurally?',
    'Comparative classification across the three reading files: the determinism reading collapses epsilon toward a technology-side inevitability profile with universal scope; the agency reading relocates epsilon onto strategic rents of printers and reformers; the co-constitution reading holds epsilon on the interaction term.',
    'Where the disagreement is located is the causal-status variable itself: whether the press determines, serves as instrument, or co-constitutes. Resolving the kernel for one reading dissolves the other two as separate constraints rather than adjusting this one''s metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is the co-constitution reading of a three-way kernel contest.').

omega_variable(
    interaction_term_separability,
    'Is epsilon genuinely attributable to the interaction between press and reformers, or decomposable into independent technology-effect and agency-effect components?',
    'Counterfactual historiography on the natural experiments: Hussite Bohemia (organizing capacity without print), the Italian states (dense print culture without reformation uptake), Scandinavia (print plus rapid princely adoption). If regional outcomes factor into separable contributions, the interaction term is not load-bearing.',
    'If separable, this story violates epsilon-invariance and should decompose into two stories (infrastructure and agency) linked by network edges; if not, the single-story interaction epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interaction_term_separability, empirical, 'Whether the reading''s characteristic epsilon lives in the interaction term or factors out.').

omega_variable(
    theater_driver_attribution,
    'Is the rising theater_ratio driven by reformer gatekeeping atrophy, by censorial announcement-without-capacity, or by both?',
    'Compare enforcement outcomes against edict volume across the interval, and track the share of unauthorized imprints and false imprints in surviving bibliographies; distinguish performative denunciation (reformers against the flood) from performative prohibition (mandates nobody executes).',
    'Determines which seat carries the atrophying function - reformer gatekeeping versus censorial administration - and therefore where a future piton signature would sit if the commercial core ever wound down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_driver_attribution, empirical, 'Attribution of the rising performative share to specific seats.').

omega_variable(
    incumbent_displacement_vs_extraction,
    'Does the church hierarchy count as a victim of extraction through the arrangement, or as an incumbent outcompeted in open contention?',
    'Conceptual separation of rents transferred through the arrangement from competitive displacement: examine whether printer and princely gains came from church assets and licensed monopolies (extraction-shaped) or from newly created demand the church never served (competition-shaped).',
    'If pure displacement, the victim declaration weakens and the classification trends toward rope; if rents rode on seized property, printing privileges, and prosecution of rivals, the tangled-rope structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_displacement_vs_extraction, conceptual, 'Whether the displaced incumbent satisfies the victim gate or the arrangement is nearer pure coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_reformation_causality__co_constitution_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t8, technology_reformation_causality__co_constitution_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(tech_tr_t8, observed).
narrative_ontology:measurement(tech_tr_t15, technology_reformation_causality__co_constitution_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(tech_tr_t15, observed).
narrative_ontology:measurement(tech_tr_t22, technology_reformation_causality__co_constitution_reading, theater_ratio, 22, 0.28).
narrative_ontology:measurement_basis(tech_tr_t22, observed).
narrative_ontology:measurement(tech_tr_t30, technology_reformation_causality__co_constitution_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(tech_tr_t30, observed).
narrative_ontology:measurement(tech_tr_t38, technology_reformation_causality__co_constitution_reading, theater_ratio, 38, 0.38).
narrative_ontology:measurement_basis(tech_tr_t38, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_reformation_causality__co_constitution_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t8, technology_reformation_causality__co_constitution_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(tech_be_t8, observed).
narrative_ontology:measurement(tech_be_t15, technology_reformation_causality__co_constitution_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(tech_be_t15, observed).
narrative_ontology:measurement(tech_be_t22, technology_reformation_causality__co_constitution_reading, base_extractiveness, 22, 0.54).
narrative_ontology:measurement_basis(tech_be_t22, observed).
narrative_ontology:measurement(tech_be_t30, technology_reformation_causality__co_constitution_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(tech_be_t30, observed).
narrative_ontology:measurement(tech_be_t38, technology_reformation_causality__co_constitution_reading, base_extractiveness, 38, 0.46).
narrative_ontology:measurement_basis(tech_be_t38, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_reformation_causality__co_constitution_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t8, technology_reformation_causality__co_constitution_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(tech_su_t8, observed).
narrative_ontology:measurement(tech_su_t15, technology_reformation_causality__co_constitution_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(tech_su_t15, observed).
narrative_ontology:measurement(tech_su_t22, technology_reformation_causality__co_constitution_reading, suppression_requirement, 22, 0.55).
narrative_ontology:measurement_basis(tech_su_t22, observed).
narrative_ontology:measurement(tech_su_t30, technology_reformation_causality__co_constitution_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(tech_su_t30, observed).
narrative_ontology:measurement(tech_su_t38, technology_reformation_causality__co_constitution_reading, suppression_requirement, 38, 0.68).
narrative_ontology:measurement_basis(tech_su_t38, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the printing press caused the Reformation' decomposes into three structurally distinct claims per the epsilon-invariance principle: technological_determinism_reading (press as sufficient cause - near-zero extraction from the technology itself, inevitability framing), beneficiary_agency_reading (reformers as deploying agents, press as instrument - epsilon located in strategic rents), and this co_constitution_reading (bidirectional formation - epsilon located in the interaction term, moderate, with the reformer-gatekeeping function decaying toward performance). Each reading gets its own epsilon, its own beneficiary/victim structure, and its own classification; they are linked here as a constraint family, with the determinism reading upstream (its claim is cited as background by the other two) and the agency reading downstream-contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
