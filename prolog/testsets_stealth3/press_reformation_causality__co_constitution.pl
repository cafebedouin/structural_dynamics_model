% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-21
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Controversy Feedback Loop of the Early Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between Luther's 1517 intervention and the consolidating confessional
 *   order circa 1600, religious change and the commercial print trade drove
 *   one another in a closed feedback circuit: controversy created urgent
 *   demand that financed industrial-scale printing, and cheap print delivered
 *   each new round of argument to the streets faster than church or empire
 *   could answer it. This file instantiates the co_constitution reading of
 *   the press_reformation_causality kernel: neither the press as autonomous
 *   artifact nor any strategist's plan accounts for the pattern - the
 *   constitutive unit is the loop itself, in which printers' commercial
 *   calculations, reformers' doctrinal commitments, authorities'
 *   countermeasures, and readers' purchases jointly produced the
 *   Reformation's shape. Per the epsilon-invariance rule, the sibling
 *   readings are separate constraints with their own epsilon: the
 *   technological_determinism reading reads the same diffusion record as
 *   artifact autonomy (no human beneficiary structure at all), and the
 *   strategic_deployment reading concentrates benefit on strategizing actors;
 *   this reading distributes both benefit and cost across seats, which is why
 *   its epsilon sits moderate and its receipt surface comes out diffuse. KEY
 *   AGENTS (by structural relationship): - commercial_printers: primary
 *   beneficiary-operator (organized/constrained) - runs the presses, bears
 *   capital and censorship risk, collects controversy-spike margins -
 *   reformist_leaders: primary beneficiary (organized/identity_locked) -
 *   supplies the content whose circulation constitutes the movement -
 *   ecclesiastical_hierarchy: primary target (institutional/trapped) - loses
 *   the information monopoly the loop dismantles - manuscript_trades_workers:
 *   secondary target (powerless/constrained) - absorbs displacement costs
 *   with no seat in the decisions causing them - territorial_magistrates:
 *   dual-positioned administrator (institutional/constrained) - licenses and
 *   taxes the trade while absorbing its conflicts - literate_urban_readers:
 *   distributed beneficiary (moderate/mobile) - buys the output, gains
 *   unmediated access - reformation_historians: analytical observer - sees
 *   the full loop structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.32).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.3).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.32).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, scaffold).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Controversy Feedback Loop of the Early Reformation").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

narrative_ontology:has_sunset_clause(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, 'ea912c14-cc74-4b95-b4f9-75977c1cb92a').
narrative_ontology:cs_kernel_codification('ea912c14-cc74-4b95-b4f9-75977c1cb92a', distributed).
narrative_ontology:cs_authority_grounding('ea912c14-cc74-4b95-b4f9-75977c1cb92a', distributed).
narrative_ontology:cs_reading_relation('ea912c14-cc74-4b95-b4f9-75977c1cb92a', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('ea912c14-cc74-4b95-b4f9-75977c1cb92a', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('ea912c14-cc74-4b95-b4f9-75977c1cb92a', foundational, reformation_outcomes_emergent_from_feedback_loops).
narrative_ontology:cs_axiom_status(reformation_outcomes_emergent_from_feedback_loops, holdable).
narrative_ontology:cs_axiom_grounding('ea912c14-cc74-4b95-b4f9-75977c1cb92a', reformation_outcomes_emergent_from_feedback_loops, empirically_contingent).
narrative_ontology:cs_axiom('ea912c14-cc74-4b95-b4f9-75977c1cb92a', foundational, technological_effects_contingent_on_human_choices).
narrative_ontology:cs_axiom_status(technological_effects_contingent_on_human_choices, holdable).
narrative_ontology:cs_axiom_grounding('ea912c14-cc74-4b95-b4f9-75977c1cb92a', technological_effects_contingent_on_human_choices, empirically_contingent).
narrative_ontology:cs_reference_frame('ea912c14-cc74-4b95-b4f9-75977c1cb92a', reciprocal_medium_agency_feedback).
narrative_ontology:cs_drift_state('ea912c14-cc74-4b95-b4f9-75977c1cb92a', contemporary_post_eisenstein_synthesis, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('ea912c14-cc74-4b95-b4f9-75977c1cb92a', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, commercial_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformist_leaders).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, literate_urban_readers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, territorial_magistrates).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, ecclesiastical_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, manuscript_trades_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, commercial_printers).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, vernacular_religious_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate presses in imperial free cities and university towns; buy paper and type on credit and gamble capital on editions whose demand spikes with each controversy wave. Margins concentrate during pamphlet booms; bankruptcies cluster when authorities seize shipments or demand shifts. Relocation is possible but costly - Basel and Geneva absorbed printers fleeing Paris and Antwerp - and leaving means abandoning supplier credit, learned networks, and city printing privileges.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, commercial_printers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, commercial_printers, payer).

% Produce the sermons, tracts, translations, and polemics that feed each controversy round; their names sell editions and their positions harden into movements with every publication cycle. Public retraction would destroy the authority their circulated writings built - the Worms stand is the template - so stepping out of the public fight is effectively unavailable once committed; their personal trajectories are fused with the circulation they depend on.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformist_leaders, beneficiary,
    organized, generational, identity_locked, continental).

% Buy pamphlets and vernacular Bibles at prices falling through the century, gaining direct access to scriptural and polemical text without clerical mediation. Choice of what to read remains theirs, bounded by literacy, price, and local confessional policing; in mixed territories they read across the confessional divide, and their purchasing decisions are the demand signal that finances the next printing run.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, literate_urban_readers, beneficiary,
    moderate, biographical, mobile, regional).

% City councils and princes license printers, grant publishing privileges, tax the trade, and intermittently ban titles under imperial or papal pressure. They harness the same press for their own proclamations, church orders, and confessional consolidation, and they absorb the consequences - diplomatic pressure, peasant-war fallout, armed confessional conflict - of the circulation they host. Their regulatory choices set the terms under which the trade operates without determining what it carries.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, territorial_magistrates, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, territorial_magistrates, beneficiary).

% Rome and the bishops lose the information monopoly through which doctrine, indulgence revenue, and clerical authority were managed; every containment attempt - indexing, banning, burning, the execution of printers - confirms the scale of the loss. They cannot withdraw from the communicative field they are losing, since abandoning it concedes it entirely; their censorship machinery (culminating in the 1559 Index) is reaction to the circulation, not participation in it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, ecclesiastical_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Scribes, scriveners, illuminators, and parchment makers watch the market for hand-copied text collapse within a generation. Some transition into print shops as compositors or correctors; many cannot - their skills, guild positions, and urban niches depreciate faster than they can retrain, and they hold no seat in the privilege-granting decisions accelerating their displacement. Their costs are borne silently and leave the thinnest trace in the record.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, manuscript_trades_workers, payer,
    powerless, immediate, constrained, regional).

% Reconstruct the circuit from imprints, privilege registers, confiscation lists, and correspondence centuries later. They take no part in the circulation and bear none of its costs, but the causal framing they adopt determines which structure the surviving record is read through - which is precisely what the contested kernel turns on.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformation_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Linked dispersed critics, printers, and readers into a self-reinforcing circuit: cheap standardized reproduction let religious argument appear in the streets faster than authorities could answer it, and each answered controversy enlarged the market financing the next round. It also standardized vernacular religious vocabularies across political and linguistic borders, giving scattered communities a common textual reference.
% TRANSFER_FUNCTION: Moved money from readers to printers, authors, and papermakers along controversy-driven demand; moved religious authority from clerical mediation into vernacular text in lay hands; moved doctrinal initiative toward whichever polemic reached the street first; and shifted commercial risk onto printer capital while reputational credit accrued to reformist authors.
% ABSENT_VOICES: Illiterate majorities, above all rural populations, experienced the loop's outcomes through preaching, visitations, and war but had no voice in it; manuscript artisans being displaced had no seat in the privilege decisions destroying their market; women worked presses and read pamphlets but were excluded from guild governance and theological deliberation; parish clergy caught between Rome and territorial churches had no forum in which either hierarchy heard them.
% DISAPPEARANCE_RATIONALE: Remove the loop circa 1520 and Luther's protest remains a university dispute answered in Latin by appointed disputants; vernacular scripture reaches readers decades later through manuscript channels slow enough for authorities to intercept; the pamphlet publics, the rapid doctrinal escalation, and the confessional map that crystallized by 1555 do not form on schedule. The subsequent European order - territorial churches, catechisms, confessional schooling - reorganizes around whatever slower communication regime replaces the missing circuit.
% FOUNDING_PROBLEM: The arrangement ran on a double problem: reformist argument needed transmission faster than manuscript and pulpit networks allowed, and the new presses needed a product whose demand was urgent enough to fund industrial-scale output. Open religious controversy supplied both at once.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: imperial and municipal police ordinances, confiscation registers, and the Roman Index attest contemporaneously that dissemination outran containment while the open window stood; the confessionalization historiography - church-order and visitation records, licensing archives - attests from outside the print-celebration literature that by the century's end licensed, policed print markets had closed that window. No party that profited from the loop is relied upon for this attestation.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).
:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.32 (end-state) because the loop's costs were real but distributed: the manuscript trades' displacement, the church hierarchy's lost monopoly rents, and the risk premiums printers priced into controversy editions - with no seat capturing the flow (see gain_flow). The temporal series shows a mid-century peak (0.46 at 1545) when entrenched printer positions, risk premiums, and completed displacement coincided, declining afterward as confessional licensing compressed controversy rents; the end-state scalar matches the final measurement. Suppression is 0.30 and deliberately modest: the coercion vivid in the record (executions of printers, the 1559 Index, imperial mandates) was counter-force aimed AT the loop, not force wielded BY it; the loop's own pressure on alternatives was structural displacement, which is captured in accessibility_collapse (0.60 - manuscript and oral channels became uncompetitive but preaching and clandestine manuscript circulation persisted). Resistance is high (0.65) because the loop met sustained, organized, ultimately failed censorial opposition. Theater is low throughout (0.18 end-state): nearly all recorded activity was functional printing, selling, banning, and buying. The measurement series runs on one shared six-point grid so every tracked metric is authored at every examined time point; short-run pamphlet booms and busts oscillate below this grid's resolution and are noted here rather than fabricated as measured oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent classifications from identical structural data. From the printer and reformer seats the loop is emancipatory enabling infrastructure - the thing that made their projects possible; from the trapped institutional seat of the church hierarchy the same loop is dispossession of a centuries-old information monopoly; from the powerless manuscript-trades seat it is silent economic destruction with no recourse and no voice; from the magistrate seat it is an ambivalent instrument they license, tax, fear, and use. The engine derives these per-seat directionalities from the declared roles, power atoms, and exit options; the divergence between the beneficiary seats' low extraction experience and the trapped payer's full-target experience is the measurement, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: literate_urban_readers (mobile exit) nearest zero; commercial_printers slightly above them (they collect margins but bear capital and censorship risk, and their constrained exit keeps them partly exposed); reformist_leaders are identity-locked beneficiaries - their fusion with the movement makes exit unthinkable, which pins them as committed insiders rather than targets. Payers sit near the full-target end: ecclesiastical_hierarchy combines institutional power with trapped exit and civilizational stakes, placing it near maximal effective extraction; manuscript_trades_workers combine powerlessness with constrained exit, so their displacement costs register at high effective extraction despite their inability to resist. territorial_magistrates derive ambiguously from their dual agenda_setter/beneficiary position - they collect fiscal and propaganda returns while absorbing conflict costs - and no directionality_override is authored because the derivation from declared roles already places them mid-range; refining their exact d is left to the engine's structural computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim prevents two symmetrical mislabelings. Reading the loop as pure coordination (a rope) erases the real asymmetric costs - the manuscript trades' extinction and the church's dispossession were borne by identifiable seats while others gained. Reading it as pure extraction (a snare) fails because there is no capturer: gain_flow is affirmatively diffuse, the coercion in the archive aimed at stopping the loop rather than enforcing it, and the church's loss is incumbent-displacement, not targeted rent collection. The transitional characterization fits because the loop's justification was the window it opened - carrying reformist argument past the containment capacity of the old order - and that window closed as confessional states internalized print control. On the R5 mismatch: founding_problem_status is dead while disappearance_verdict is world_rearranges, which routes a capture/zombie flag; the cross-check against the theater path shows theater_ratio at 0.18 with no performative maintenance, so the flag resolves as infrastructure-dependence (the world had been rebuilt on cheap print) rather than zombie persistence - the arrangement did not atrophy in place, it dissolved into licensed confessional print markets, which is why no mandatrophy_resolved declaration is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_diffusion_without_print,
    'How much of the Reformation''s spread structurally required print, versus what slower manuscript-and-pulpit networks had already achieved for heterodox movements?',
    'Comparative diffusion and suppression analysis of pre-print heterodoxies (Waldensian, Lollard, Hussite) against reform-era propagation and containment rates.',
    'Comparable diffusion rates would show this reading overweights print''s constitutive role; markedly slower pre-print propagation confirms print''s enabling-scaffolding contribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_diffusion_without_print, empirical, 'Whether print was constitutive or merely accelerative for the Reformation''s spread.').

omega_variable(
    kernel_reading_locus_of_efficacy,
    'This file instantiates only the co_constitution reading of kernel press_reformation_causality; the disagreement with sibling readings is located at the locus of causal efficacy - systemic feedback versus artifact autonomy versus actor strategy. Which locus does the shared evidence actually support?',
    'Adjudication by comparative explanatory power on shared evidence: diffusion timing, printer bankruptcy patterns, retraction episodes, and containment failures, scored against each reading''s predictions.',
    'Adopting the technological_determinism reading deletes the human beneficiary/victim structure entirely (an artifact-level claim with no parties); adopting strategic_deployment concentrates gain_flow on strategist seats and raises measured extraction. This file''s distributed structure and diffuse receipt hold only under the co-constitution locus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_efficacy, conceptual, 'Committer-frame omega: reading-choice underdetermination within the press-Reformation causality kernel.').

omega_variable(
    sunset_versus_transformation,
    'Did the loop sunset (its controversy-carrying function atrophied while the print trade persisted in form) or transform into routine licensed confessional infrastructure?',
    'Compositional analysis of imprint output 1550-1600 (collapsing share of unlicensed controversy versus catechism, liturgy, and official print) alongside the evolution of privilege and licensing registers.',
    'Genuine transformation confirms the transitional character and the functional sunset; persistence of controversy-form with atrophied function would indicate inertial drift toward a degraded classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_versus_transformation, empirical, 'Whether the arrangement ended by transformation or by atrophy-in-place.').

omega_variable(
    printer_profit_capture_degree,
    'Did controversy-spike profits concentrate durably in a small printer oligopoly sufficiently to constitute a named capture seat, contradicting the diffuse receipt asserted here?',
    'Reconstruction of printer finances and edition runs across boom-bust cycles (Froben, Estienne-class firms versus aggregate shop mortality and bankruptcy records).',
    'Demonstrated durable concentration would rename gain_flow to commercial_printers and push the structural reading toward hybrid extraction; dispersion across a high-mortality trade sustains the diffuse receipt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(printer_profit_capture_degree, empirical, 'Degree of profit concentration in the print trade during controversy booms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1517, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prc_cocon_tr_t1517, press_reformation_causality__co_constitution, theater_ratio, 1517, 0.08).
narrative_ontology:measurement_basis(prc_cocon_tr_t1517, observed).
narrative_ontology:measurement(prc_cocon_tr_t1530, press_reformation_causality__co_constitution, theater_ratio, 1530, 0.1).
narrative_ontology:measurement_basis(prc_cocon_tr_t1530, observed).
narrative_ontology:measurement(prc_cocon_tr_t1545, press_reformation_causality__co_constitution, theater_ratio, 1545, 0.13).
narrative_ontology:measurement_basis(prc_cocon_tr_t1545, observed).
narrative_ontology:measurement(prc_cocon_tr_t1560, press_reformation_causality__co_constitution, theater_ratio, 1560, 0.15).
narrative_ontology:measurement_basis(prc_cocon_tr_t1560, observed).
narrative_ontology:measurement(prc_cocon_tr_t1580, press_reformation_causality__co_constitution, theater_ratio, 1580, 0.17).
narrative_ontology:measurement_basis(prc_cocon_tr_t1580, observed).
narrative_ontology:measurement(prc_cocon_tr_t1600, press_reformation_causality__co_constitution, theater_ratio, 1600, 0.18).
narrative_ontology:measurement_basis(prc_cocon_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(prc_cocon_be_t1517, press_reformation_causality__co_constitution, base_extractiveness, 1517, 0.28).
narrative_ontology:measurement_basis(prc_cocon_be_t1517, observed).
narrative_ontology:measurement(prc_cocon_be_t1530, press_reformation_causality__co_constitution, base_extractiveness, 1530, 0.38).
narrative_ontology:measurement_basis(prc_cocon_be_t1530, observed).
narrative_ontology:measurement(prc_cocon_be_t1545, press_reformation_causality__co_constitution, base_extractiveness, 1545, 0.46).
narrative_ontology:measurement_basis(prc_cocon_be_t1545, observed).
narrative_ontology:measurement(prc_cocon_be_t1560, press_reformation_causality__co_constitution, base_extractiveness, 1560, 0.44).
narrative_ontology:measurement_basis(prc_cocon_be_t1560, observed).
narrative_ontology:measurement(prc_cocon_be_t1580, press_reformation_causality__co_constitution, base_extractiveness, 1580, 0.37).
narrative_ontology:measurement_basis(prc_cocon_be_t1580, observed).
narrative_ontology:measurement(prc_cocon_be_t1600, press_reformation_causality__co_constitution, base_extractiveness, 1600, 0.32).
narrative_ontology:measurement_basis(prc_cocon_be_t1600, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causality__co_constitution, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, resource_allocation).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'print caused the Reformation' decomposes into three structurally distinct causal claims, each with its own epsilon, beneficiary structure, and classification. This file (co_constitution) treats the feedback loop itself as the constitutive arrangement: benefits and costs distribute across printers, reformers, readers, magistrates, the church hierarchy, and the manuscript trades, yielding moderate epsilon and diffuse receipt. The technological_determinism sibling attributes the same diffusion record to artifact autonomy - it has no human beneficiary or victim structure to declare, and its epsilon concerns the claim's epistemic operation rather than any historical transfer. The strategic_deployment sibling concentrates benefit on strategizing actors (reformers exploiting printers' capital, printers exploiting reformers' content), which raises its measured extraction and names a capturer seat. Upstream/downstream: the determinism reading consumes the same spread-timing evidence this reading explains mechanistically, and the deployment reading shares this reading's actor-level evidence base; each sibling can cite this file as the integrating 'balanced' account, which is itself a structural influence edge. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
