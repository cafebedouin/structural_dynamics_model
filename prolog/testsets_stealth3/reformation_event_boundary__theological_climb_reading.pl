% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation Event Boundary — Theological Climb Reading (Justification by Faith Alone as Required Breakthrough)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This story instantiates the theological-climb reading of the
 *   Reformation's event boundary: between 1517 and 1555, a doctrinal recovery
 *   — justification by faith alone, preached from the Wittenberg lectern and
 *   carried through the vernacular press — broke the Latin church's monopoly
 *   on Christian teaching and made institutional separation from Rome
 *   necessary, because the medieval authority structure rested on the
 *   penitential-and-indulgence apparatus the recovered teaching dissolved. On
 *   this reading the Catholic Church is the arrangement's principal loser
 *   (the victim of theological correction), believing laity are its intended
 *   beneficiaries, and the period is tight: the arrangement is built between
 *   the Ninety-Five Theses and the Peace of Augsburg. The epsilon referent is
 *   the standing confessional-separation arrangement itself, assessed by this
 *   reading's own lights — the reading scores the separation that actually
 *   stood at interval end, not any preferred alternative. Claim and metrics
 *   are authored independently: the claimed type (tangled_rope) states what
 *   this seat believes structurally true — a genuine doctrinal coordination
 *   function carrying real asymmetric costs through the same structure —
 *   while the metrics describe the arrangement's actual operation, including
 *   the coercion the reading's necessity framing is tempted to discount.
 *   Sibling readings (political_swap_reading,
 *   composite_overdetermination_reading) are separate constraint files linked
 *   through the network section; nothing from them is averaged into this one.
 *
 * KEY AGENTS:
 *   - lutheran_reformers: agenda-setting doctrinal leadership (organized/identity_locked) — articulates the teaching, builds the churches, cannot recant without self-annihilation
 *   - catholic_church_hierarchy: principal payer (institutional/constrained) — loses obedience, revenue, and property across northern Europe; cannot concede the correction without dissolving its own authority claim
 *   - evangelical_territorial_rulers: administering enforcers with material gain (powerful/mobile) — issue church orders, hold custody of transferred property, revise alignments when the balance shifts
 *   - charles_v_imperial_authority: counter-enforcement agenda-setter (institutional/constrained) — bans, wars, the Interim; forced by 1555 to accept the partition he resisted
 *   - evangelical_laity: declared beneficiaries bearing the coercive price (moderate/constrained) — receive vernacular teaching and discipline alike; bound by ruler's decree in mixed territories
 *   - anabaptists_radical_reformers: doubly prosecuted payers (powerless/trapped) — hunted by every confessional authority; no chartering patron anywhere
 *   - peasants_league_1525: crushed petitioners (organized/trapped) — evangelical-plus-social demands broken at Frankenhausen; postwar rural discipline tightened
 *   - printing_networks: incidental beneficiaries (organized/mobile) — the controversy builds the mass vernacular print market; movable when markets close
 *   - erasmian_humanists: excluded voice (moderate/mobile) — the reform-without-separation position never seated at any diet, colloquy, or peace
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.72).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.74).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation Event Boundary — Theological Climb Reading (Justification by Faith Alone as Required Breakthrough)").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '6d3546e2-0305-4a88-a1bd-40ec5b4e542e').
narrative_ontology:cs_kernel_codification('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', fixed_text).
narrative_ontology:cs_authority_grounding('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', lineage).
narrative_ontology:cs_interpretation_layer_present('6d3546e2-0305-4a88-a1bd-40ec5b4e542e').
narrative_ontology:cs_reading_relation('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', foundational, sola_fide_recovery_is_genuine_breakthrough).
narrative_ontology:cs_axiom_status(sola_fide_recovery_is_genuine_breakthrough, holdable).
narrative_ontology:cs_axiom_grounding('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', sola_fide_recovery_is_genuine_breakthrough, theological).
narrative_ontology:cs_axiom('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', foundational, separation_required_by_conscience_before_institution).
narrative_ontology:cs_axiom_status(separation_required_by_conscience_before_institution, holdable).
narrative_ontology:cs_axiom_grounding('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', separation_required_by_conscience_before_institution, deontological).
narrative_ontology:cs_reference_frame('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', apostolic_scripture_norm).
narrative_ontology:cs_drift_state('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', contemporary_ecumenical_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6d3546e2-0305-4a88-a1bd-40ec5b4e542e', '2026-08-04T09:15:00Z').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, evangelical_laity).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, printing_networks).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, anabaptists_radical_reformers).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, peasants_league_1525).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, evangelical_territorial_rulers).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, justification_by_faith_alone).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_scriptura_authority).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University theologians and preachers led by Martin Luther who articulate the recovered teaching on justification, translate scripture into the vernacular, write catechisms and church orders, and train the pastors who staff the new territorial churches. Their standing rests entirely on the doctrinal cause: recanting at Worms or at any later imperial diet would have ended their careers, their safety, and their life's work at a stroke. They live inside the movement they lead and cannot step out of it without ceasing to be themselves.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, lutheran_reformers, agenda_setter,
    organized, generational, identity_locked, continental).

% The papacy, curia, bishops, and monastic orders of the Latin obedience. Between 1517 and 1555 they lose obedience, revenue, and property across northern Europe: kingdoms and imperial cities repudiate Roman jurisdiction, monasteries are dissolved, annates and indulgence income cease, and roughly half of Christendom's adherents pass to the separated churches. Their countermeasures — bans, prohibited-book lists, the expanded Roman Inquisition, and finally the Council of Trent — defend doctrine and discipline but recover little territory within the interval. Exit would mean conceding the doctrinal correction, which their office cannot do without dissolving the authority claim on which the office rests.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, global).

% Princes and imperial city councils that adopt the evangelical confession, issue church orders in their own name, appoint superintendents, and take custody of former church property. They host and protect the reformers against imperial bans and negotiate collectively in the Schmalkaldic League. They also receive monastic lands, benefices, and parish revenues into their own administration, funding courts and debts from the transfer. Their alignment is revisable in ways doctrine is not: some shift confession or alliance when the military balance moves, and after 1555 the settlement fixes their confession by treaty while leaving their administrative grip intact.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, evangelical_territorial_rulers, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, evangelical_territorial_rulers, beneficiary).

% The emperor and his officers, who prosecute the old unity: the Worms ban, the Turkish wars that consume every year a German settlement might use, the campaign against the Schmalkaldic League, the Augsburg Interim, and finally the negotiated Peace of Augsburg. Charles commands the largest military-financial complex in Europe but is chronically overextended — France, the Ottomans, Italy — and must purchase religious peace he personally abhors. His room to impose a settlement shrinks each decade until 1555 forces acceptance of the partition he spent thirty years resisting.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, charles_v_imperial_authority, agenda_setter,
    institutional, generational, constrained, continental).

% Townsmen, artisans, and rural households that receive the vernacular scripture, catechism, and reformed liturgy, and whose parishes pass to the new churches. They gain the teaching the movement exists to deliver, and they pay for it: war levies in the 1540s, conscription, confessional discipline through consistories and visitations, and in mixed territories a ruler's decree binding their conscience without their consent. Leaving a hostile territory means abandoning land, guild standing, and kin; staying under a hostile ruler means conforming outwardly.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, evangelical_laity, beneficiary,
    moderate, biographical, constrained, continental).

% Swiss Brethren, Hutterites, and related congregations demanding adult baptism and a gathered church independent of any territorial establishment. Every confessional authority prosecutes them: imperial mandates after 1525, Lutheran and Catholic territories alike execute their leading teachers (Felix Manz drowned 1527, Michael Sattler burned 1527), and the collapse of the Munster kingdom in 1535 licenses a generation of extermination campaigns. No territory, city, or patron will charter them; survival means concealment or flight eastward.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, anabaptists_radical_reformers, payer,
    powerless, immediate, trapped, continental).

% The Swabian and Franconian peasant bands of 1524-25, whose Twelve Articles couple evangelical demands with the abolition of serfdom and church dues. Their uprising is broken at Frankenhausen and elsewhere within months; tens of thousands die; the defeat hands their lords both the confiscated-commons question and a precedent for governing through emergency. After 1525 the rural estates are taxed and disciplined more tightly than before the uprising.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, peasants_league_1525, payer,
    organized, immediate, trapped, regional).

% Printers, publishers, and pamphleteers centered on Wittenberg, Strasbourg, Basel, and Augsburg. The controversy makes vernacular print a mass market: Luther's tracts sell in the tens of thousands, Flugschriften multiply into thousands of editions, and presses retool around doctrinal debate, Bible translation, and polemic. Their fortunes rise with the controversy's intensity and fall when censorship or war closes markets; they can move presses and stock to friendlier cities.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, printing_networks, beneficiary,
    organized, immediate, mobile, continental).

% The international republic of letters around Erasmus, which sought moral and textual reform of the church without rupture: philological scripture editions, satire of clerical abuse, and a middle way between Wittenberg and Rome. Erasmus breaks with Luther publicly in 1524-25 over free will, refuses every settlement table thereafter, and dies in 1536 estranged from both camps. Their position — reform without separation — is argued in correspondence and dedications but is never seated at any diet, colloquy, or peace negotiation in the interval.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, erasmian_humanists, excluded,
    moderate, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, evangelical_territorial_rulers).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Once communities accepted the recovered teaching, something had to supply what Roman jurisdiction had supplied: ordained clergy, a catechism, a liturgy, schools and visitation systems, and a common confession binding scattered congregations into one teachable body. The territorial church orders and the Augsburg Confession solve that problem for the evangelical territories.
% TRANSFER_FUNCTION: Moves jurisdiction, property, and allegiance: monastic lands, benefices, and parish revenues pass from Roman-controlled corporations to princely and civic custody; tithe and visitation obligations are re-pointed to territorial churches; and the confessional allegiance of subjects passes from Rome to whichever settlement each ruler adopts — after 1555 by legal decree.
% ABSENT_VOICES: The radical adult-baptism congregations object that every settlement, evangelical or Roman, welds conscience to territory; their testimony survives in martyrologies and interrogation records, not at any negotiating table. The Erasmian humanists object that reform never required separation; they are excluded from every diet and colloquy after 1530. Catholic laity in converting territories are bound by their ruler's decree without consultation; the settlement's legal form assumes a consent that was never asked.
% DISAPPEARANCE_RATIONALE: Overnight removal of the separation would restore a single western jurisdiction on paper only: the vernacular scriptures, the trained evangelical clergy, the princely custodianship of church property, and the confessional identities of hundreds of communities were all rebuilt around the division and would immediately be re-contested. The fiscal basis of the north European states, the imperial constitution's religious clauses, and the map of Europe itself depend on the arrangement.
% FOUNDING_PROBLEM: The medieval penitential-and-indulgence apparatus: a system in which assurance of forgiveness was mediated by purchased certificates, pilgrimages, and clerical absolution, which the reformers held obscured the teaching that justification is received by faith alone.
% FOUNDING_PROBLEM_CORROBORATION: Half-corroborated from outside the benefiting parties. The grievance is attested across party lines: Catholic contemporaries and the Council of Trent itself acknowledge the indulgence traffic's abuses (Trent's 1563 decree curtails exactly the practices Luther attacked), and imperial diet records treat the complaint as a public fact. What is NOT corroborated outside the benefiting parties is the further claim that the abuse expressed doctrinal error requiring separation — Catholic authorities locate the defect in administration and discipline, not in the doctrine of merit. The genealogy's grievance is externally attested; its doctrinal-error reading is attested only by the movement's own heirs.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction ends the interval at 0.72: even assessed by this reading's own lights, the arrangement that carried the doctrinal correction moved enormous value — monastic and benefice property into princely custody, tithe and levy obligations re-pointed, war finance across the 1540s — and the church that lost it had no compensating channel. Suppression (0.74 scalar; the suppression_requirement series traces its build-up) reflects the arrangement's dependence on legal-military enforcement: the Worms ban, the crushing of the peasant bands, the imperial mandates against the radicals, the Schmalkaldic War, and the Augsburg freeze that bound subjects to their ruler's confession. Theater ratio climbs from 0.15 to 0.42 as confessional positioning — polemic, patristic florilegia, ceremonial assertion of identity — takes a growing share of activity beside the continuing real work of catechesis, translation, and pastoral training, easing slightly after 1555 as the settlement replaces open positioning with fixed legal form. Accessibility_collapse is 0.58: within a territory the cuius regio principle closed alternatives almost completely, but border-crossing migration kept alternatives alive at the continental scale, so collapse is real but incomplete. Resistance is 0.70: Catholic reform and counterattack, radical dissent, peasant insurrection, and imperial arms all pressed against the arrangement across the interval. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. All three tracked series run on one shared eight-point grid (1517, 1521, 1525, 1530, 1534, 1539, 1546, 1555) so no metric's row is backfilled from another's; the suppression_requirement series is authored because enforcement capacity visibly builds across the interval — this is an enforcement-history story, not a static-suppression picture.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the catholic_church_hierarchy seat the arrangement is confiscation joined to heresy: property, jurisdiction, and members taken under doctrinal cover, with no exit that does not dissolve its own authority claim — high effective extraction from a constrained institutional target. From the evangelical_laity seat the same arrangement delivers the teaching it exists to deliver while taxing, conscripting, and disciplining its recipients — beneficiary-side but materially discounted. The two agenda-setter seats diverge from each other: the reformers are identity-locked to the cause they embody (exit equals self-annihilation, which pushes their experienced position toward the target end despite building the arrangement), while the territorial rulers hold arbitrage-grade mobility and custody of the transferred property, pulling their seat toward the beneficiary end. The engine computes these per-seat classifications from the structural data; this commentary explains why they must diverge, not which is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. catholic_church_hierarchy is declared a victim with institutional power and constrained exit — near the full-target end. anabaptists_radical_reformers and peasants_league_1525 are declared victims with trapped exit — effectively full targets. evangelical_laity is declared a beneficiary, which alone would derive a low directionality near the subsidy end; the explicit override (power_atom moderate, d 0.30) corrects this, because the laity's lived position includes war levies, conscription, consistorial discipline, and decree-bound conscience in mixed territories — beneficiary-side, but far from pure subsidy. evangelical_territorial_rulers hold the agenda_setter role with a beneficiary secondary role and mobile exit; their arbitrage-grade realignment capacity (Maurice of Saxony's double switch is the type case) pulls their derived directionality toward the beneficiary end despite their enforcement work. printing_networks, beneficiaries with mobile exit, sit nearest the subsidy end. No override is applied to the institutional power atom because two institutionally-powered seats (the hierarchy and the emperor) hold opposed relationships that the derivation already separates through their victim and agenda-setter declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification does double duty against both mislabeling directions. Against the political-swap sibling's reduction: it preserves the genuine coordination function — vernacular scripture, catechism, trained clergy, common confession — without which the swap account's seized assets have no story of why communities followed. Against this reading's own temptation: it refuses to book the arrangement's costs as the necessary price of truth, keeping identifiable payers (the hierarchy, the radicals, the peasants) on the books where a triumphalist climb account would dissolve them into transition costs. Mandatrophy is not yet in play inside the interval: the founding problem (the penitential-indulgence apparatus) is contested rather than dead, and the arrangement's original function is still being performed at 1555, so no resolved-mandate flag is authored; the atrophy question belongs to later centuries and, if authored, to a successor story with a later interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is the Reformation''s primary driver theological innovation — this reading''s claim — or princely political realignment (political_swap_reading), or an irreducible overdetermined composite admitting no single driver or tight periodization (composite_overdetermination_reading)?',
    'Comparative causal analysis across the three sibling stories: counterfactual tests on doctrinal content (does separation occur where doctrinal uptake is strong but princely interest weak, and vice versa?), dating of doctrinal commitment versus asset transfer in territorial archives, and the handling of drivers the tight window excludes.',
    'Adopting political_swap_reading converts the Catholic hierarchy''s losses from costs of doctrinal correction into seizure proceeds and re-reads believer gains as manipulation effects; adopting composite_overdetermination_reading dissolves the tight 1517-1555 window, multiplies the victim and beneficiary sets, and forbids this story''s single-driver epsilon. This file''s classification is valid only under the climb commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this constraint is one reading of kernel reformation_event_boundary; sibling readings change victim set, periodization, and epsilon.').

omega_variable(
    separation_necessity_counterfactual,
    'Was institutional separation genuinely required by the doctrinal breakthrough, as this reading''s core claim holds, or was a reform-without-rupture accommodation structurally available?',
    'Close reading of the failed accommodation track: the Leipzig disputation''s escalation logic, the Colloquy of Marburg''s 1529 near-agreement and its lord''s-supper breakdown, the Regensburg Colloquy of 1541 where justification language nearly converged, and Rome''s pre-Trent flexibility limits.',
    'If accommodation was structurally available and refused for non-doctrinal reasons, the separation carries avoidable-cost weight, the reading''s necessity claim weakens toward choice, and measured extraction shifts from necessary-price to discretionary — raising effective extraction and opening a scaffold-like transitionality the tight-window account forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_necessity_counterfactual, empirical, 'Whether the separation was structurally necessary or a refused accommodation.').

omega_variable(
    discipline_substitution_question,
    'Were the believers this reading counts as freed from false doctrine net beneficiaries, or did confessionalization substitute one discipline regime (penitential-indulgence) for another (consistory, visitation, catechetical examination)?',
    'Compare pre- and post-conversion discipline intensity in matched territories: visitation records, consistory minutes, fine and penance frequencies, and catechetical examination practice before 1517 and after each territory''s church order.',
    'If discipline intensified, the beneficiary seat''s directionality rises toward symmetry, the liberation framing discounts, and the arrangement''s coordination function reads increasingly as discipline-transfer — converging this story toward the political_swap_reading''s victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discipline_substitution_question, empirical, 'Net-benefit status of the declared beneficiary population under confessional discipline.').

omega_variable(
    extraction_handoff_question,
    'Is the Catholic Church''s victim status clean, or did the arrangement merely hand the same revenue streams to new custodians — indulgence and annate income replaced by territorial church levies and confiscated-property yields?',
    'Territorial fiscal reconstruction: trace benefice, tithe, and monastic income in converting territories across the 1520s-1540s, comparing pre-separation Roman-directed flows with post-separation princely and consistorial flows.',
    'If flows merely changed hands, the correction framing overstates liberation, the reading''s climb signature weakens toward the swap reading''s seizure account, and the Catholic hierarchy''s victim status becomes partial rather than clean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_handoff_question, empirical, 'Whether the arrangement transferred extraction systems or ended one.').

omega_variable(
    periodization_window_commitment,
    'Does the tight 1517-1555 window capture the arrangement, or does the confessional boundary''s consolidation — and its coercive cost — continue unbroken to 1648?',
    'Test whether the enforcement and extraction series continue past 1555 without structural break: the Augsburg settlement''s freeze, the Thirty Years'' War''s confessional violence, and the Westphalian settlement''s finalization of the map.',
    'Extending the window raises cumulative extraction and enforcement totals substantially and pressures this story''s classification toward the snare end; holding the window tight preserves the climb reading''s signature but leaves post-1555 costs unattributed to any story in the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_window_commitment, conceptual, 'Periodization commitment: tight window versus extended confessionalization era.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1521, reformation_event_boundary__theological_climb_reading, theater_ratio, 1521, 0.18).
narrative_ontology:measurement_basis(refo_tr_t1521, observed).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__theological_climb_reading, theater_ratio, 1525, 0.25).
narrative_ontology:measurement_basis(refo_tr_t1525, observed).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__theological_climb_reading, theater_ratio, 1530, 0.32).
narrative_ontology:measurement_basis(refo_tr_t1530, observed).
narrative_ontology:measurement(refo_tr_t1534, reformation_event_boundary__theological_climb_reading, theater_ratio, 1534, 0.35).
narrative_ontology:measurement_basis(refo_tr_t1534, observed).
narrative_ontology:measurement(refo_tr_t1539, reformation_event_boundary__theological_climb_reading, theater_ratio, 1539, 0.4).
narrative_ontology:measurement_basis(refo_tr_t1539, observed).
narrative_ontology:measurement(refo_tr_t1546, reformation_event_boundary__theological_climb_reading, theater_ratio, 1546, 0.45).
narrative_ontology:measurement_basis(refo_tr_t1546, observed).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.42).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.3).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1521, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1521, 0.38).
narrative_ontology:measurement_basis(refo_be_t1521, observed).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.48).
narrative_ontology:measurement_basis(refo_be_t1525, observed).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1530, 0.55).
narrative_ontology:measurement_basis(refo_be_t1530, observed).
narrative_ontology:measurement(refo_be_t1534, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1534, 0.58).
narrative_ontology:measurement_basis(refo_be_t1534, observed).
narrative_ontology:measurement(refo_be_t1539, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1539, 0.62).
narrative_ontology:measurement_basis(refo_be_t1539, observed).
narrative_ontology:measurement(refo_be_t1546, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1546, 0.68).
narrative_ontology:measurement_basis(refo_be_t1546, observed).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.72).
narrative_ontology:measurement_basis(refo_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement_basis(refo_su_t1517, observed).
narrative_ontology:measurement(refo_su_t1521, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1521, 0.35).
narrative_ontology:measurement_basis(refo_su_t1521, observed).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.5).
narrative_ontology:measurement_basis(refo_su_t1525, observed).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1530, 0.58).
narrative_ontology:measurement_basis(refo_su_t1530, observed).
narrative_ontology:measurement(refo_su_t1534, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1534, 0.65).
narrative_ontology:measurement_basis(refo_su_t1534, observed).
narrative_ontology:measurement(refo_su_t1539, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1539, 0.68).
narrative_ontology:measurement_basis(refo_su_t1539, observed).
narrative_ontology:measurement(refo_su_t1546, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1546, 0.78).
narrative_ontology:measurement_basis(refo_su_t1546, observed).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.74).
narrative_ontology:measurement_basis(refo_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: reformation_event_boundary decomposes into three sibling readings of one kernel. This file (theological_climb_reading) instantiates the climb account: doctrinal breakthrough primary, tight 1517-1555 window, Catholic hierarchy as victim of correction, believers as beneficiaries. political_swap_reading instantiates the seizure account over the same years (theology as post-hoc rationalization; believers as manipulated; princes as the receiving seat). composite_overdetermination_reading refuses single-driver attribution and any tight window, multiplying victim and beneficiary sets and loosening periodization toward 1648. The epsilon values differ by construction: the same historical arrangement is scored by each reading's own lights, so the files are separate constraints linked here rather than one constraint with a measurement parameter. Evidential structure: the climb reading supplies the doctrinal-priority evidence base the swap reading must answer; the composite reading consumes both as strands.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__theological_climb_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
