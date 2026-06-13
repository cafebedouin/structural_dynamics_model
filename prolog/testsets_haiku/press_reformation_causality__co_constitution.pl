% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print Economy and Reformation Co-Constitution: Feedback Loops Between Technology and Religious Controversy
 *   domain: history/technology/religion
 *
 * SUMMARY:
 *   This reading models the Reformation as an emergent outcome of reciprocal
 *   feedback loops between printing technology and religious controversy,
 *   with neither technology nor human agency as the sole determining force.
 *   Printers invested in capacity because controversy created demand;
 *   controversy intensified because printing could propagate theological
 *   positions at unprecedented scale and speed; reformers gained platform
 *   reach without bearing printing costs; and the Church lost its monopoly on
 *   textual authority as soon as printing became economically viable. The
 *   constraint operates as a tangled rope because genuine coordination
 *   emerges (mass distribution of ideas across fragmented authorities)
 *   alongside substantive extraction (manuscript scribes' displacement,
 *   Church's authority erosion, guild control of printing access). The
 *   co-constitution reading specifically rejects both technological
 *   determinism (the press caused the Reformation) and strategic determinism
 *   (reformers weaponized the press to achieve a predetermined goal).
 *   Instead, technology and agency mutually shaped outcomes through economic
 *   feedback loops: each side constrained and enabled the other.
 *
 * KEY AGENTS:
 *   - Printer guilds: control production capacity, set market standards, profit from controversy-driven demand
 *   - Reformed clergy: gain platform reach, drive demand through theological challenges, benefit from printer investment
 *   - Church hierarchy: lose interpretive monopoly, must respond through print economy they cannot control
 *   - Manuscript scribes: lose economic livelihood as hand-copy is displaced
 *   - Secular nobility: use reformation texts to justify church property seizure
 *   - Urban merchants and readers: create demand through literacy and participation, drive printing volume
 *   - Women readers: foundational to demand but excluded from formal theological participation
 *   - Analytical observer: measures causal structure without privileging technology or human intent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.58).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.71).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print Economy and Reformation Co-Constitution: Feedback Loops Between Technology and Religious Controversy").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history/technology/religion").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, 'e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803').
narrative_ontology:cs_kernel_codification('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', distributed).
narrative_ontology:cs_authority_grounding('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', diffuse_epistemic).
narrative_ontology:cs_reading_relation('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', foundational, mutual_constitution_of_technology_and_agency).
narrative_ontology:cs_axiom_status(mutual_constitution_of_technology_and_agency, holdable).
narrative_ontology:cs_axiom_grounding('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', mutual_constitution_of_technology_and_agency, empirically_contingent).
narrative_ontology:cs_axiom('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', foundational, feedback_loops_prevent_causal_reduction).
narrative_ontology:cs_axiom_status(feedback_loops_prevent_causal_reduction, holdable).
narrative_ontology:cs_axiom_grounding('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', feedback_loops_prevent_causal_reduction, instrumental).
narrative_ontology:cs_reference_frame('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', reciprocal_feedback_causality).
narrative_ontology:cs_drift_state('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', nineteenth_century_print_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e81bd0b8-94fa-4f5c-92da-6ddd6b5ac803', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printer_guilds).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformed_clergy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, church_manuscript_scribes).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_orthodoxy_defenders).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, unauthorized_printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, secular_nobility).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, urban_merchants_readers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, rome_church_hierarchy).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, human_agency_and_technology_mutually_constitutive).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, media_economy_drives_religious_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control printing technology and production capacity. Set standards for typeface, paper, binding. Profit directly from high-volume religious text production (Bibles, commentary, polemics). Their business model depends on continuous demand for new editions and competing interpretations. They make licensing decisions about what gets printed and distribute through trade networks.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printer_guilds, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printer_guilds, beneficiary).

% Gain direct access to the printing apparatus to spread vernacular scripture and polemical tracts attacking church hierarchy. Their theological positions reach mass audiences in ways manuscript culture made impossible. They benefit from the printer's investment in distribution and marketing, and they drive demand for new printed editions by issuing theological challenges and responses.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformed_clergy, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reformed_clergy, agenda_setter).

% Lose economic livelihood as manuscript copying is displaced by mass printing. Their skill set becomes obsolete within a generation. They have no organized exit option—their labor is tied to institutional ecclesiastical employment, and the shift from hand-copy to print is irreversible. Some transition to proofing or editorial roles, but at lower status and pay.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, church_manuscript_scribes, payer,
    moderate, biographical, trapped, local).

% Face coordinated textual assault from reformed positions, each backed by printing volume and repetition that manuscript distribution could never sustain. Their counter-arguments must be published to be heard at scale, drawing them into the same competitive print economy that favors controversy and novelty. Their institutional authority is eroded by the democratization of textual production.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_orthodoxy_defenders, payer,
    institutional, generational, constrained, regional).

% Attempt to enter printing production but face guild restrictions on typeface, paper sourcing, and market access. Licensed printers and established houses enforce quality standards and copyright-like control over texts, locking out smaller operations. Unauthorized reprinting of popular reformist texts is profitable but legally dangerous and subject to seizure.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, unauthorized_printers, payer,
    moderate, biographical, constrained, local).

% Use printed reformation texts to challenge church land ownership and clerical immunity. Printing enables coordinated political messaging across their territories. Their patronage of reformist printers creates a parallel power base to ecclesiastical authority, and the printed controversy justifies their seizure of church property.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, secular_nobility, beneficiary,
    powerful, generational, mobile, regional).

% Cannot prevent the printing of theology it opposes without suppressing printing itself—a remedy worse than the disease. Must respond through printing and counter-printing, which amplifies the controversy it is trying to contain. Authority depends on textual control and interpretive monopoly, both of which print economy undermines. Enforcement mechanisms (papal ban, Index) are reactive rather than constitutive.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, rome_church_hierarchy, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, rome_church_hierarchy, agenda_setter).

% Gain access to religious texts in vernacular languages and can form reading groups independent of parish clergy. They become a new constituency for reformed theology, their literacy and disposable income creating demand that drives printing volume. Their literacy and participation in the feedback loop—reading, discussing, creating demand for more texts—is constitutive of the entire system's operation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, urban_merchants_readers, beneficiary,
    organized, biographical, mobile, regional).

% Excluded from formal theological training but increasingly appear as readers of printed vernacular scripture. They are neither licensed nor celebrated in the constraint's official operation, yet their demand for religious reading material (testified in correspondence and reading group records) drives a significant portion of printing volume. Their participation is foundational but officially unacknowledged.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, women_as_readers, excluded,
    powerless, biographical, trapped, local).

% Measures the causal structure: whether technology determined the Reformation, whether actors strategically wielded technology, or whether the two co-constituted through reciprocal feedback. This reading holds that neither technology nor human agency can be isolated as primary cause—the constraint emerges from their mutual shaping.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, printer_guilds).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a self-amplifying cycle between theological controversy and printing capacity: reformist texts drive demand for printed responses; responses generate demand for counter-responses; printers invest in capacity and distribution networks to capture the growing market; each new text feeds the controversy that justifies the next printing run. The cycle solves a genuine coordination problem—how to propagate new ideas at scale across fragmented regional authorities—but without a single designer or unified plan.
% TRANSFER_FUNCTION: Moves labor value from manuscript copyists to printers and typeface makers; moves ecclesiastical textual authority to whoever commands printing capacity and distribution networks; moves reading-time and literacy focus from Latin-literate clergy to vernacular-reading urban merchants and emerging reading publics. The constraint also moves economic rents: printers capture value from controversy, reformers gain platform reach without bearing printing costs, and secular nobility gain legitimacy for property seizure through reformist justifications.
% ABSENT_VOICES: Women readers (foundational to demand but excluded from formal participation), manuscript-dependent scribal communities (whose obsolescence is rapid and undocumented), illiterate rural populations (whose religious experience is shaped by printed texts but who never read), Jewish communities (whose printing networks are parallel and suppressed), and heterodox reform movements (whose texts are printed but later erased from the approved historical record).
% DISAPPEARANCE_RATIONALE: If the printing economy collapsed overnight—through guild suppression or technology failure—theological controversy would narrow to Latin-literate clergy, vernacular scripture would revert to manuscript production (slow and expensive), and the reform movement would fragment into regional isolation. The Church would recover its interpretive monopoly. Conversely, if controversy ended, printer capacity would collapse within a generation, and printing would revert to liturgical and administrative documents.
% FOUNDING_PROBLEM: How can new theological positions reach mass audiences across fragmented regional authorities? How can reformers challenge church orthodoxy when church controls manuscript production and clerical reading? How do printers profit sustainably when demand for religious texts is new and unproven?
% FOUNDING_PROBLEM_CORROBORATION: Reformers (Luther, Calvin, Zwingli) attest that printing made their theology viable at scale—documented in their own published defenses of printing as a tool. Printers' business records (Froben, Plantin, Gutenberg's successors) show the profitability turn coincides with religious controversy. Church historians outside the defending parties (Lucien Febvre, Elizabeth Eisenstein) corroborate that manuscript culture could NOT have sustained the speed and volume of the Reformation. However, NO external party corroborates that technology ALONE determined the outcome—that claim comes only from technological-determinist historiography, not from participants.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__co_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 to 0.58 over the first 30 time points (representing the 15th–16th centuries), then plateaus: the feedback loops solidify, guild control hardens, and the extraction becomes normalized rather than expanding. Theater ratio climbs from 0.12 to 0.44, indicating that as the Reformation becomes institutionalized, performative elements increase—doctrinal disputation becomes theatrical, printers compete on sensationalism rather than innovation, the constraint persists partly through narrative maintenance. Suppression rises from 0.45 to 0.71 as enforcement mechanisms harden: Church censorship (Index, book bans), guild licensing restrictions, noble authority over printing privileges all intensify. The leveled coercion grid shows asymmetric pressure: accessibility collapse is higher at the organizational level (printers, clergy) than individual level (readers), indicating the constraint operates through institutional chokepoints rather than individual coercion. Resistance falls from 0.55 to 0.44 as the constraint becomes normalized—what begins as an open conflict becomes an internalized structural fact. The shared time grid ensures every metric is authored at every examined point, avoiding misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The printer guilds and reformed clergy perceive this as enabling coordination that solves a real problem (propagating ideas at scale); the Church and manuscript scribes perceive it as extractive suppression of their authority and livelihood. An analytical observer at the structural level might classify the constraint as rope (genuine coordination problem solved) while observers from the victim seats classify it as snare (coordinated suppression of manuscript culture and Church textual authority). The engine computes per-seat classifications from directionality: beneficiaries (printers, reformed clergy) get low d and low χ; victims (scribes, Church) get high d and high χ. The co-constitution reading deliberately refuses to pick a side—both the coordination and the extraction are real, and neither can be subtracted from the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Printer guilds are structural beneficiaries (d ~ 0.15): they control capacity, set standards, capture rents from controversy-driven demand, and have high exit options through geographic mobility and alliance switching. Reformed clergy are partial beneficiaries (d ~ 0.25): they gain platform reach at minimal cost (printers bear investment), but they are also constrained by printer gatekeeping and commercial demands for sensationalism over precision. Manuscript scribes are clear victims (d ~ 0.85): their labor becomes obsolete, they have no organized exit (trapped, identity-locked to clerical employment), and their displacement is irreversible. The Church sits at d ~ 0.65: it loses authority but retains institutional power and can respond through the print economy itself, though each response feeds the cycle. Secular nobility are partial beneficiaries (d ~ 0.30): they gain justification for property seizure, but they must continuously engage the print economy to maintain legitimacy claims. Urban merchant readers sit near symmetric (d ~ 0.50): genuine coordination benefit (access to texts), but diffuse cost (time spent reading, exposure to controversial ideas that destabilize social hierarchy). Women readers are excluded rather than classified: their participation is foundational but officially unacknowledged, so they appear as neither full beneficiaries nor victims in the constraint's official structure—this is an exclusion mechanism that the omega variable addresses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to propagate new theology across fragmented authorities) is live and contested: reformers attest it remains live (schism and theological diversity persists); the Church attests the problem should be dead (orthodoxy should prevail) but must constantly defend against printed challenge; secular authorities attest the problem is instrumentally live (enables property seizure). The disappearance verdict is world_rearranges: if printing collapsed, the Reformation fragments, the Church recovers textual authority, and manuscript scribalism partially revives. The mandatrophy question is whether the constraint persists because the founding problem remains unsolved or because the constraint itself became valuable to its beneficiaries independent of its original function. The measurement series suggests partial mandatrophy: extractiveness plateaus while theater rises, indicating the constraint persists partly through narrative/institutional maintenance once the primary coordination function is established. However, no single axis shows the constraint's function completely inverted (theater reaching 0.9+, extractiveness plummeting), so mandatrophy is partial rather than complete. The co-constitution reading explicitly rejects the claim that either component (technology or agency) can be removed and the outcome remains stable—both are constitutive, which means the mandatrophy question cannot be resolved by isolating one variable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_agency_causal_direction,
    'Does the Reformation emerge from technology causing human behavior (technological determinism), human strategy wielding technology as a tool (strategic deployment), or reciprocal feedback loops where technology enables strategies that drive demand for more printing capacity (co-constitution)?',
    'Counterfactual analysis: (a) remove printing capacity at specific points and trace what reformers could accomplish; (b) analyze printer investment decisions and whether they track theological controversy or precede it; (c) examine whether reformers had pre-printed strategies that printing merely scaled, or whether printing forced strategic innovations on them.',
    'Technology-first would shift classification toward Mountain (inevitable natural outcome of technical capability); strategy-first would shift toward Snare (deliberate coordinated extraction); co-constitution stays Tangled Rope (mutual shaping, genuine coordination + distributed extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_agency_causal_direction, empirical, 'Whether causality is technology → agency → outcome, agency → technology → outcome, or recursive feedback.').

omega_variable(
    excluded_women_readers_structural_role,
    'Are women readers foundational to the constraint''s operation (their demand drives printing volume) or incidental participants (printing creates an audience but women''s participation is not constitutive)?',
    'Quantitative analysis of printing output correlated with evidence of women''s reading (wills, correspondence, reading group records, book ownership); counterfactual: if women readers had been legally barred from reading, would printing volume and profitability have collapsed?',
    'If foundational, women readers should be reclassified from excluded to beneficiary (though still officially unacknowledged), making the constraint''s equity profile worse: core contribution without recognition. If incidental, the constraint''s beneficiary/victim structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_women_readers_structural_role, empirical, 'Whether women readers are foundational to demand or incidental participants.').

omega_variable(
    extraction_vs_coordination_separability,
    'Is the extraction component (scribal displacement, Church authority erosion, guild monopoly control) structurally inseparable from the coordination component (mass distribution of theology across fragmented authorities), or could the same coordination be achieved without the extraction?',
    'Analyze whether printers and reformers REQUIRED guild monopoly control and scribal displacement to achieve scale, or whether they simply benefited from it opportunistically. Would a competitive, non-monopoly printing market have achieved the same Reformation outcomes at lower extraction cost?',
    'If separable, the measured extractiveness includes rent-seeking unrelated to the genuine coordination function—true coordination cost is lower than 0.58. If inseparable, the extraction is the price of the coordination and cannot be subtracted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally entangled or contingently paired.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the co-constitution reading logically foreclose the technological determinism reading, or do they occupy different epistemic frames that could both be true simultaneously?',
    'Examine whether the core premises contradict: does affirming ''technology and agency co-constitute'' logically deny ''technology determines''? Or can a historian simultaneously accept that technology was necessary and that human agency was also constitutive?',
    'If foreclosed, the relationship is competitive and one reading must yield. If coexistent, both readings remain live and the constraint system contains irreducible ambiguity about causality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between co-constitution and technological determinism readings.').

omega_variable(
    printer_reform_alliance_mutual_constraint,
    'Do printers and reformed clergy constitute a coalition with aligned interests, or a mutual constraint relationship where each party depends on the other but neither controls the outcome?',
    'Analyze printer business records and reformer correspondence: do they coordinate strategy, or do they respond opportunistically to each other''s actions? Who initiated the alliance—printers seeking profitable theology or reformers seeking printing capacity?',
    'If coalition, reformers should be reclassified as agenda_setter alongside printers (shared agenda). If mutual constraint, the dual role authoring is correct and neither party has strategic dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_reform_alliance_mutual_constraint, empirical, 'Whether printer-reformer relationship is strategic coalition or mutual constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causality__co_constitution, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(pres_tr_t0, projected).
narrative_ontology:measurement(pres_tr_t15, press_reformation_causality__co_constitution, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(pres_tr_t15, observed).
narrative_ontology:measurement(pres_tr_t30, press_reformation_causality__co_constitution, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(pres_tr_t30, observed).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causality__co_constitution, theater_ratio, 50, 0.44).
narrative_ontology:measurement_basis(pres_tr_t50, observed).
narrative_ontology:measurement(pres_tr_t75, press_reformation_causality__co_constitution, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(pres_tr_t75, observed).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causality__co_constitution, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(pres_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causality__co_constitution, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(pres_be_t0, projected).
narrative_ontology:measurement(pres_be_t15, press_reformation_causality__co_constitution, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(pres_be_t15, observed).
narrative_ontology:measurement(pres_be_t30, press_reformation_causality__co_constitution, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(pres_be_t30, observed).
narrative_ontology:measurement(pres_be_t50, press_reformation_causality__co_constitution, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(pres_be_t50, observed).
narrative_ontology:measurement(pres_be_t75, press_reformation_causality__co_constitution, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(pres_be_t75, observed).
narrative_ontology:measurement(pres_be_t100, press_reformation_causality__co_constitution, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(pres_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causality__co_constitution, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(pres_su_t0, projected).
narrative_ontology:measurement(pres_su_t15, press_reformation_causality__co_constitution, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(pres_su_t15, observed).
narrative_ontology:measurement(pres_su_t30, press_reformation_causality__co_constitution, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(pres_su_t30, observed).
narrative_ontology:measurement(pres_su_t50, press_reformation_causality__co_constitution, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(pres_su_t50, observed).
narrative_ontology:measurement(pres_su_t75, press_reformation_causality__co_constitution, suppression_requirement, 75, 0.71).
narrative_ontology:measurement_basis(pres_su_t75, observed).
narrative_ontology:measurement(pres_su_t100, press_reformation_causality__co_constitution, suppression_requirement, 100, 0.71).
narrative_ontology:measurement_basis(pres_su_t100, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=100
narrative_ontology:measurement(pres_grid_01, press_reformation_causality__co_constitution, accessibility_collapse(class), 0, 0.38).
narrative_ontology:measurement(pres_grid_02, press_reformation_causality__co_constitution, accessibility_collapse(class), 100, 0.65).
narrative_ontology:measurement(pres_grid_03, press_reformation_causality__co_constitution, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(pres_grid_04, press_reformation_causality__co_constitution, accessibility_collapse(individual), 100, 0.72).
narrative_ontology:measurement(pres_grid_05, press_reformation_causality__co_constitution, accessibility_collapse(organizational), 0, 0.42).
narrative_ontology:measurement(pres_grid_06, press_reformation_causality__co_constitution, accessibility_collapse(organizational), 100, 0.78).
narrative_ontology:measurement(pres_grid_07, press_reformation_causality__co_constitution, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(pres_grid_08, press_reformation_causality__co_constitution, accessibility_collapse(structural), 100, 0.72).
narrative_ontology:measurement(pres_grid_09, press_reformation_causality__co_constitution, resistance(class), 0, 0.62).
narrative_ontology:measurement(pres_grid_10, press_reformation_causality__co_constitution, resistance(class), 100, 0.48).
narrative_ontology:measurement(pres_grid_11, press_reformation_causality__co_constitution, resistance(individual), 0, 0.58).
narrative_ontology:measurement(pres_grid_12, press_reformation_causality__co_constitution, resistance(individual), 100, 0.38).
narrative_ontology:measurement(pres_grid_13, press_reformation_causality__co_constitution, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(pres_grid_14, press_reformation_causality__co_constitution, resistance(organizational), 100, 0.42).
narrative_ontology:measurement(pres_grid_15, press_reformation_causality__co_constitution, resistance(structural), 0, 0.55).
narrative_ontology:measurement(pres_grid_16, press_reformation_causality__co_constitution, resistance(structural), 100, 0.44).
narrative_ontology:measurement(pres_grid_17, press_reformation_causality__co_constitution, stakes_inflation(class), 0, 0.32).
narrative_ontology:measurement(pres_grid_18, press_reformation_causality__co_constitution, stakes_inflation(class), 100, 0.58).
narrative_ontology:measurement(pres_grid_19, press_reformation_causality__co_constitution, stakes_inflation(individual), 0, 0.28).
narrative_ontology:measurement(pres_grid_20, press_reformation_causality__co_constitution, stakes_inflation(individual), 100, 0.64).
narrative_ontology:measurement(pres_grid_21, press_reformation_causality__co_constitution, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(pres_grid_22, press_reformation_causality__co_constitution, stakes_inflation(organizational), 100, 0.72).
narrative_ontology:measurement(pres_grid_23, press_reformation_causality__co_constitution, stakes_inflation(structural), 0, 0.42).
narrative_ontology:measurement(pres_grid_24, press_reformation_causality__co_constitution, stakes_inflation(structural), 100, 0.68).
narrative_ontology:measurement(pres_grid_25, press_reformation_causality__co_constitution, suppression(class), 0, 0.42).
narrative_ontology:measurement(pres_grid_26, press_reformation_causality__co_constitution, suppression(class), 100, 0.72).
narrative_ontology:measurement(pres_grid_27, press_reformation_causality__co_constitution, suppression(individual), 0, 0.38).
narrative_ontology:measurement(pres_grid_28, press_reformation_causality__co_constitution, suppression(individual), 100, 0.65).
narrative_ontology:measurement(pres_grid_29, press_reformation_causality__co_constitution, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(pres_grid_30, press_reformation_causality__co_constitution, suppression(organizational), 100, 0.78).
narrative_ontology:measurement(pres_grid_31, press_reformation_causality__co_constitution, suppression(structural), 0, 0.48).
narrative_ontology:measurement(pres_grid_32, press_reformation_causality__co_constitution, suppression(structural), 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, resource_allocation).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__co_constitution, 0.18).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, vernacular_scripture_accessibility).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, guild_monopoly_printing_control).

% DUAL FORMULATION NOTE:
% This constraint decomposes the contested kernel 'press_reformation_causality' into three distinct constraints corresponding to three incompatible causal narratives: (1) co_constitution (this story) frames causality as reciprocal feedback loops; (2) technological_determinism frames causality as technology determining human behavior; (3) strategic_deployment frames causality as strategic human agency wielding technology. These are not three perspectives on one constraint—they are three structurally distinct claims with different ε values, different beneficiary/victim structures, and different classifications. The sibling readings are linked via network.affects_constraints so the corpus captures the full terrain of the dispute. No single reading should be preferred a priori; the engine's classification for each reading will show which structural model best fits the metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__co_constitution, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
