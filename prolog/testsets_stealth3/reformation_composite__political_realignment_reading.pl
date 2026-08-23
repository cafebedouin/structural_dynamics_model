% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Political Realignment Reading of the Reformation: Cuius Regio Sovereignty Arrangement
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'reformation_composite': the political_realignment_reading, under which
 *   the Reformation is fundamentally an event in sovereignty formation, with
 *   emerging territorial states using religious differentiation to break the
 *   fiscal, judicial, and legitimating grip of imperial and papal authority.
 *   The epsilon referent is the standing arrangement under contest: the cuius
 *   regio complex through which confession becomes an attribute of territory
 *   and an instrument of princely consolidation, assessed by this reading's
 *   own lights (jurisdictional transfer, property confiscation, coerced
 *   confessional assignment) and NOT the theological or technological
 *   arrangements the sibling readings would place at center. Structurally the
 *   arrangement carries both a genuine settlement function, converting an
 *   empire-wide zero-sum jurisdictional war into locally frozen territorial
 *   settlements, and asymmetric extraction, with princes and secularizing
 *   prelates collecting land, revenue, and jurisdiction while the
 *   papal-imperial apparatus pays, and subjects pay differently in assigned
 *   consciences. KEY AGENTS (by structural relationship):
 *   territorial_princes: primary beneficiary and agenda-setter
 *   (powerful/arbitrage); secularizing_prelates: secondary beneficiary
 *   (institutional/arbitrage); free_imperial_cities: dual beneficiary-payer
 *   (organized/constrained); wittenberg_reformers: captured dual
 *   beneficiary-payer (moderate/identity_locked); imperial_authority: primary
 *   payer (institutional/trapped); papal_curia: primary payer
 *   (institutional/trapped); territorial_subjects: payer (powerless/trapped);
 *   peasant_assemblies: payer, crushed third path (organized/trapped);
 *   anabaptist_congregations: excluded voice (powerless/trapped);
 *   confessionalization_historians: analytical observer. The story links to
 *   its two sibling readings and to the downstream Westphalian settlement via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.5).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.46).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Political Realignment Reading of the Reformation: Cuius Regio Sovereignty Arrangement").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, 'bd8e3998-ebf4-478b-85fc-00dfa32ca4c4').
narrative_ontology:cs_kernel_codification('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', distributed).
narrative_ontology:cs_authority_grounding('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', distributed).
narrative_ontology:cs_reading_relation('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', foundational, sovereignty_precedes_doctrine).
narrative_ontology:cs_axiom_status(sovereignty_precedes_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', sovereignty_precedes_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', secondary, confession_is_instrument_of_rule).
narrative_ontology:cs_axiom_status(confession_is_instrument_of_rule, holdable).
narrative_ontology:cs_axiom_grounding('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', confession_is_instrument_of_rule, instrumental).
narrative_ontology:cs_reference_frame('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', princely_sovereignty_consolidation).
narrative_ontology:cs_drift_state('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', post_confessionalization_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bd8e3998-ebf4-478b-85fc-00dfa32ca4c4', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, secularizing_prelates).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, free_imperial_cities).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, wittenberg_reformers).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, imperial_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_curia).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, territorial_subjects).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, peasant_assemblies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, free_imperial_cities).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, wittenberg_reformers).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, cuius_regio_eius_religio).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, westphalian_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Electors, dukes, and landgraves who adopt or reject reformed teaching by decision of their own court. Converting lets them dissolve monasteries into dynastic land, stop annate payments to Rome, take over episcopal courts and marriage jurisdiction, and appoint clergy answerable to them. Because confession is now a matter of princely decision, a ruler can also change course when land or dignity offers, as when Albertine Saxony joined the emperor's side in 1546 and received electoral rank. Their commitment runs dynasty-length, not lifetime.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_princes, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_princes, agenda_setter).

% Bishops and administrators who keep their offices while adopting the new settlement: Albert of Prussia turns the Teutonic Order's state into a hereditary duchy; cathedral chapters and administrators retain revenues under reformed auspices. Their position depends on remaining useful to the prince who protects the arrangement; departure means surrendering the office.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, secularizing_prelates, beneficiary,
    institutional, biographical, arbitrage, regional).

% City councils gain control of parish property, preaching appointments, and relief from episcopal jurisdiction when they align with the reformation. They also absorb heavy costs: imperial pressure, war levies, and the loss of trading partners across the confessional line. Leaving the alignment means facing both the emperor and neighboring princes alone.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, free_imperial_cities, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, free_imperial_cities, payer).

% University theologians supply the doctrinal case that rulers deploy. Electoral protection gives Luther and Melanchthon safety, pulpits, and a print platform; in exchange the movement's shape follows princely policy. The radicals who want congregational autonomy are driven out, and the theologians cannot recant or relocate without destroying the cause they embody. Their identity and their dependence on Saxon protection are the same fact.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, wittenberg_reformers, beneficiary,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, wittenberg_reformers, payer).

% The emperor's office rests on universalist claims: one Christendom, one empire, estates bound by ban and appeal. Each estate that settles religion unilaterally removes taxes, troops, and obedience from the imperial center. Charles V wins the Schmalkaldic War in 1547 and still cannot convert victory into restored jurisdiction; abandoning the universalist claim would dissolve the office he holds.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, imperial_authority, payer,
    institutional, generational, trapped, continental).

% Rome loses annates, dispensation fees, indulgence proceeds, and appellate jurisdiction territory by territory, and finally loses its seat at the peace table itself in 1648, when Innocent X protests the Westphalian settlement and is ignored by every signatory. The curia cannot abandon its universal jurisdiction claim without ceasing to be what it is.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_curia, payer,
    institutional, civilizational, trapped, continental).

% Villagers and townspeople receive whatever confession their ruler settles, on his schedule, with clergy appointed over them. The right to emigrate exists on paper but means forfeiting home, guild membership, and kin. Their consciences are allocated as part of the territorial settlement.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_subjects, payer,
    powerless, biographical, trapped, regional).

% Bands behind the Twelve Articles demand congregation-elected pastors and an end to dues justified by Roman law. Their program is a third path, neither Roman nor princely, and it is destroyed militarily in 1525 with roughly a hundred thousand deaths; surviving villages return to whichever confession their lord imposes.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, peasant_assemblies, payer,
    organized, immediate, trapped, regional).

% Congregations of adult baptism seek voluntary gathered churches outside both the princely and the episcopal systems. Neither the Augsburg nor the Westphalian settlement recognizes them; both confessional blocs hunt them, from the execution of Felix Manz in 1527 to the fall of Munster in 1535. They would object to every article of the territorial settlement and appear in none of its texts.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, anabaptist_congregations, excluded,
    powerless, immediate, trapped, regional).

% Modern scholars reconstruct the arrangement from visitation records, treaty texts, and correspondence. They sit outside every confessional bloc and can compare what each seat experienced against what the settlements say.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, confessionalization_historians, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the confessional-jurisdictional conflict territorially: each estate settles its own territory's religion once (cuius regio, eius religio), converting an empire-wide zero-sum contest over universal jurisdiction into a patchwork of locally frozen settlements, and holds general confessional warfare off the continent for roughly a century. Secondarily it coordinates ruler-subject administration through unified consistory and visitation structures.
% TRANSFER_FUNCTION: Moves ecclesiastical land, tithe streams, appointment rights, court jurisdiction, and cross-border revenue (annates, indulgence proceeds, Peter's pence) from the papal-imperial apparatus to territorial rulers; moves confessional self-determination from subjects and congregations to princes.
% ABSENT_VOICES: Parish congregations and the rural assemblies behind the Twelve Articles, who demanded elected pastors; Anabaptist and spiritualist radicals, hunted by both confessional blocs; and ordinary lay believers generally. None sat at Speyer, Augsburg, or Westphalia; the settlements allocated their consciences and property without them.
% DISAPPEARANCE_RATIONALE: Without the political-realignment arrangement, imperial estates remain inside universal papal-imperial jurisdiction: annates and appeals continue, no secularizations occur, the confessional map never freezes, and the Westphalian sovereignty order, built on the precedent that religious allegiance follows territorial allegiance, never forms. Dynastic revenue, landholding, and the entire early-modern state-formation trajectory rearrange.
% FOUNDING_PROBLEM: How can a territorial ruler consolidate taxation, law, appointment, and military power over his territory when legitimation, judicial appeal, and revenue all route through transnational papal and imperial institutions?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Peace of Westphalia's own text (the Instrumentum Pacis Osnabrugensis granting estates territorial rights and admitting Calvinism) records the problem's resolution; Pope Innocent X's bull declaring the peace null was ignored by every signatory, and the defeated universal authority's impotent protest attests that the jurisdictional contest was decided against it; seventeenth-century publicists such as Pufendorf treat territorial sovereignty as settled fact.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are end-state (1648) assessments on a single shared time grid (1517, 1525, 1531, 1546, 1555, 1585, 1618, 1648) with all three tracked metrics authored at every point, so no metric row borrows another's end-state values. Extractiveness 0.50: the great transfers (monastic dissolution, annates abolition, episcopal jurisdiction) are complete by mid-interval and the residual burden, frozen confessional assignment and contested chapter property, is real but diminished; the series peaks at 0.72 in 1555 when Augsburg codifies the arrangement. Suppression 0.46 is the end-state of a series that peaked at 0.72 during the Thirty Years War and fell sharply with Westphalia's demilitarized enforcement and toleration articles; suppression is authored as a raw structural property and is nowhere scaled by power or scope in this story. Theater 0.48 rises monotonically: doctrinal language progressively serves territorial policy, through staged colloquies and confessions deployed as diplomatic documents, until the settlement's own categories (the normative year) function as legal fictions. Accessibility_collapse 0.62: within a territory confessional alternatives collapse for subjects almost completely, but rulers retain conversion arbitrage and middle paths (Gallican-style, concordat) persist, so alternatives are narrowed rather than eliminated. Resistance 0.60: the Peasants' War, Anabaptist persistence, league militarization, and the Thirty Years War are the measurable peaks. The claimed type is tangled_rope on structural grounds independent of these numbers: the arrangement solves a real collective problem while transferring property, jurisdiction, and conscience asymmetrically, and it holds only under active enforcement (imperial bans, defensive leagues, visitation commissions, war). Where the engine's computed types diverge from this claim, particularly at the payer seats and in the late-interval drift toward inertial persistence, that divergence is the datum the corpus exists to take, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the princes' seat the arrangement is the liberation of governance from transnational toll-keepers: they experience settlement, not extraction. From the curia's and the emperor's seats the identical structure is dispossession: property, courts, and obedience removed estate by estate, ending with exclusion from the peace itself. From the subjects' seat it is neither liberation nor dispossession but substitution of masters: the same sermon, tax, and consistory under new ownership. The reformers occupy a captured middle: protected, published, and subordinated at once. The engine derives these divergent experiences from the declared roles, exits, and horizons; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (territorial_princes, secularizing_prelates, free_imperial_cities, wittenberg_reformers) drive d toward the beneficiary end for those seats; victim declarations (imperial_authority, papal_curia, territorial_subjects, peasant_assemblies) drive d toward the target end, amplified by trapped exits for the curia, the emperor, and the subjects. One override is declared: the derivation receives conflicting signals for the reformers, a beneficiary role pushing d down and an identity_locked exit pushing d up, and the operative fact is capture, since the movement's doctrinal output was requisitioned by princely policy, its radical wing destroyed, and its leaders unable to recant or relocate. The override sets the moderate-power seat to d = 0.45, near-symmetric leaning target. Free imperial cities are left to derivation: their dual beneficiary/payer position nets out mildly favorable, moderated by the constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, how a ruler consolidates taxation, law, appointment, and military power when legitimation, appeal, and revenue route through Rome and the emperor, is dead by 1648: Westphalia records the victory, and the defeated authority's protest bull is ignored by every signatory. Yet the arrangement persists past its victory as frozen legal machinery: the normative year, the reservation disputes, the paper right of emigration. The R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) flags precisely this zombie tendency, and the rising theater series corroborates it. The classification discipline prevents two opposite mislabels: reading the arrangement as pure extraction erases the genuine settlement function that kept a general confessional war off the continent for a century; reading it as pure coordination erases the confiscations, the crushed third path, and the assigned consciences. Tangled rope holds both facts, and the late-interval decay toward theatrical maintenance is the mandatrophy signature arriving on schedule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading of the reformation_composite kernel; would instantiating the theological_fragmentation_reading or the technological_mediation_reading instead produce a different beneficiary/victim structure and a different epsilon?',
    'Author the two sibling stories and compare computed classifications: the delta specification predicts rulers-as-beneficiaries holds here, while the theological reading should seat confessing communities as the acting parties and the technological reading should seat printers, publishers, and censorial authorities.',
    'Classification of this file is valid only for this reading; under the theological reading the victim set shifts toward enforced-conformity targets and under the technological reading toward information-gatekeeping rents, changing both epsilon and the computed per-seat types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is the political reading of a three-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    causality_direction_politics_doctrine,
    'Does religious differentiation drive sovereignty assertion, or do pre-existing sovereignty projects select religious differentiation as an available instrument?',
    'Comparative timing analysis across cases: rulers who adopted reformed teaching without prior jurisdictional grievance versus rulers who pursued sovereignty while remaining Catholic (Bavaria, the French concordat path, the Habsburg hereditary lands) isolate whether confession leads or follows.',
    'If selection dominates, the arrangement is instrument-choice within an existing state-building project and epsilon attribution shifts from the religious mechanism to the underlying sovereignty competition; if differentiation leads, the religious form carries causal weight of its own.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_direction_politics_doctrine, empirical, 'Direction of causation between confessional differentiation and sovereignty consolidation.').

omega_variable(
    westphalia_completion_or_repudiation,
    'Is the Westphalian settlement a completion, a mutation, or a repudiation of the cuius regio arrangement?',
    'Legal-historical analysis of the normative-year clause, the toleration articles, and the admission of Calvinism against the 1555 text.',
    'Completion keeps the arrangement''s structure alive past 1648; repudiation realizes a de facto sunset and dates obsolescence at the peace; mutation splits the difference and leaves the classification interval-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(westphalia_completion_or_repudiation, conceptual, 'Whether 1648 finishes, transforms, or overturns the 1555 settlement.').

omega_variable(
    subject_conscience_share_of_extraction,
    'How much of the measured burden falls on subjects'' coerced confessional assignment versus inter-elite jurisdictional and property transfer?',
    'Microhistorical visitation records, emigration registers under the ius emigrandi, and consistory disciplinary archives quantify the population-side burden.',
    'A large population share makes subjects co-primary victims and raises effective extraction at the powerless seat; a small share confines the arrangement to an elite rent-transfer story and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subject_conscience_share_of_extraction, empirical, 'Distribution of the arrangement''s costs between elites and populations.').

omega_variable(
    ecclesiastical_reservation_cap,
    'Did the ecclesiastical reservation materially cap the arrangement''s extractive reach after 1555?',
    'Compare trajectories of prince-bishoprics under the reservation against fully secularized territories; the Cologne War and the Strasbourg succession test whether the cap held under pressure.',
    'An effective cap bounds post-1555 extraction and supports the plateau in the measurement series; a porous cap, as the wars of the 1580s suggest, means the reservation was largely performative and extraction continued climbing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_reservation_cap, empirical, 'Whether the reservation clause was a binding limit or a paper guarantee.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1525, reformation_composite__political_realignment_reading, theater_ratio, 1525, 0.22).
narrative_ontology:measurement_basis(refo_tr_t1525, observed).
narrative_ontology:measurement(refo_tr_t1531, reformation_composite__political_realignment_reading, theater_ratio, 1531, 0.28).
narrative_ontology:measurement_basis(refo_tr_t1531, observed).
narrative_ontology:measurement(refo_tr_t1546, reformation_composite__political_realignment_reading, theater_ratio, 1546, 0.33).
narrative_ontology:measurement_basis(refo_tr_t1546, observed).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.35).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).
narrative_ontology:measurement(refo_tr_t1585, reformation_composite__political_realignment_reading, theater_ratio, 1585, 0.4).
narrative_ontology:measurement_basis(refo_tr_t1585, observed).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.44).
narrative_ontology:measurement_basis(refo_tr_t1618, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.48).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.38).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1525, reformation_composite__political_realignment_reading, base_extractiveness, 1525, 0.52).
narrative_ontology:measurement_basis(refo_be_t1525, observed).
narrative_ontology:measurement(refo_be_t1531, reformation_composite__political_realignment_reading, base_extractiveness, 1531, 0.6).
narrative_ontology:measurement_basis(refo_be_t1531, observed).
narrative_ontology:measurement(refo_be_t1546, reformation_composite__political_realignment_reading, base_extractiveness, 1546, 0.67).
narrative_ontology:measurement_basis(refo_be_t1546, observed).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.72).
narrative_ontology:measurement_basis(refo_be_t1555, observed).
narrative_ontology:measurement(refo_be_t1585, reformation_composite__political_realignment_reading, base_extractiveness, 1585, 0.69).
narrative_ontology:measurement_basis(refo_be_t1585, observed).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.61).
narrative_ontology:measurement_basis(refo_be_t1618, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.5).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement_basis(refo_su_t1517, observed).
narrative_ontology:measurement(refo_su_t1525, reformation_composite__political_realignment_reading, suppression_requirement, 1525, 0.45).
narrative_ontology:measurement_basis(refo_su_t1525, observed).
narrative_ontology:measurement(refo_su_t1531, reformation_composite__political_realignment_reading, suppression_requirement, 1531, 0.52).
narrative_ontology:measurement_basis(refo_su_t1531, observed).
narrative_ontology:measurement(refo_su_t1546, reformation_composite__political_realignment_reading, suppression_requirement, 1546, 0.6).
narrative_ontology:measurement_basis(refo_su_t1546, observed).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.63).
narrative_ontology:measurement_basis(refo_su_t1555, observed).
narrative_ontology:measurement(refo_su_t1585, reformation_composite__political_realignment_reading, suppression_requirement, 1585, 0.66).
narrative_ontology:measurement_basis(refo_su_t1585, observed).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.72).
narrative_ontology:measurement_basis(refo_su_t1618, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.46).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, resource_allocation).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, westphalian_sovereignty_settlement).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Reformation' decomposes, per the epsilon-invariance principle, into three structurally distinct claims sharing one kernel. This file authors the political reading: its epsilon attaches to the jurisdictional-and-property transfer complex (cuius regio, secularization, annates abolition). The theological_fragmentation_reading's epsilon would attach to enforced doctrinal conformity and the destruction of the radical third path; the technological_mediation_reading's epsilon would attach to information-gatekeeping rents (licensing, censorship, printer patronage). The upstream/downstream ordering runs political -> theological (the settlement decided which fragmentations were institutionalized) and political -> westphalian_sovereignty_settlement (the arrangement's completion-or-repudiation point). Each sibling file must reciprocate these links to keep the family connected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
