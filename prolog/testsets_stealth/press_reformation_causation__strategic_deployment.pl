% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Strategic Print Deployment by Reformers and Printers (1517-1555)
 *   domain: history of technology/religious history/media studies
 *
 * SUMMARY:
 *   This story instantiates the strategic_deployment reading of the
 *   press_reformation_causation kernel: the claim that reformers and printers
 *   deliberately and strategically exploited the printing press as an
 *   instrument, and that the technology itself was neutral capacity awaiting
 *   purposeful use. The standing arrangement under contest — and therefore
 *   the epsilon referent — is the 1517-1555 print-deployment economy:
 *   reformer-authored vernacular pamphlets, treatises, and scripture
 *   translations commissioned into a commercial print network that amortized
 *   expensive presses on speculative editions. The reading holds that human
 *   purposes drove the outcome; the press supplied capacity, not causation.
 *   Per the epsilon-invariance principle, the sibling readings
 *   (technological_determinism, mutual_shaping) are separate constraints in
 *   separate files with their own epsilon values, linked through the network
 *   section; the contest between readings is routed to omega variables, not
 *   folded into this classification. The claim/metric gap is deliberate and
 *   small here: the reading is CLAIMED as a coordination arrangement (agents
 *   deploying a tool for mutual gain), while the authored metrics record
 *   modest extraction that accumulated over the interval as successful
 *   printers consolidated and obtained exclusive privileges.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Primary beneficiary (organized/identity_locked) — engineered the deployment, composing for the medium and timing releases against suppression cycles
 *   - master_printers: Primary beneficiary and operational agenda-setter (organized/arbitrage) — ran the presses, speculated on editions, captured the monetary gains
 *   - literate_urban_readers: Net gaining audience with secondary cost-bearing (moderate/mobile) — bought, shared, and read aloud the deployed texts
 *   - catholic_church_hierarchy: Cost-bearing displaced incumbent (institutional/identity_locked) — lost de facto control of doctrinal dissemination and funded counter-measures
 *   - imperial_censorship_authorities: Formal agenda-setter whose enforcement persistently failed (institutional/constrained) — issued edicts that lagged the network they policed
 *   - manuscript_scribes_and_scriptoria: Collateral cost-bearers (moderate/trapped) — lost commissioned copying work to competitive displacement
 *   - historians_of_the_book: Analytical observer (analytical/analytical) — reconstructs deployment decisions from colophons, contracts, and correspondence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.45).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.16).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.16).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Print Deployment by Reformers and Printers (1517-1555)").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history of technology/religious history/media studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, 'a87bdf93-7c96-407a-bf99-14eb949606e7').
narrative_ontology:cs_kernel_codification('a87bdf93-7c96-407a-bf99-14eb949606e7', distributed).
narrative_ontology:cs_authority_grounding('a87bdf93-7c96-407a-bf99-14eb949606e7', expertise).
narrative_ontology:cs_interpretation_layer_present('a87bdf93-7c96-407a-bf99-14eb949606e7').
narrative_ontology:cs_reading_relation('a87bdf93-7c96-407a-bf99-14eb949606e7', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('a87bdf93-7c96-407a-bf99-14eb949606e7', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('a87bdf93-7c96-407a-bf99-14eb949606e7', foundational, human_agency_upstream_of_media_effects).
narrative_ontology:cs_axiom_status(human_agency_upstream_of_media_effects, holdable).
narrative_ontology:cs_axiom_grounding('a87bdf93-7c96-407a-bf99-14eb949606e7', human_agency_upstream_of_media_effects, empirically_contingent).
narrative_ontology:cs_axiom('a87bdf93-7c96-407a-bf99-14eb949606e7', foundational, press_capacity_neutral_until_deployed).
narrative_ontology:cs_axiom_status(press_capacity_neutral_until_deployed, holdable).
narrative_ontology:cs_axiom_grounding('a87bdf93-7c96-407a-bf99-14eb949606e7', press_capacity_neutral_until_deployed, empirically_contingent).
narrative_ontology:cs_reference_frame('a87bdf93-7c96-407a-bf99-14eb949606e7', agent_driven_instrumental_adoption).
narrative_ontology:cs_drift_state('a87bdf93-7c96-407a-bf99-14eb949606e7', contemporary_affordance_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a87bdf93-7c96-407a-bf99-14eb949606e7', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, master_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, literate_urban_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, literate_urban_readers).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, imperial_censorship_authorities).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, manuscript_scribes_and_scriptoria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Composed vernacular treatises, pamphlets, and scripture translations engineered for the medium: short formats, cheap quartos, woodcut illustration, release timing matched to feast-day markets and imperial diet schedules. Cultivated working relationships with print shops across multiple cities so that suppression in any one jurisdiction could not interrupt circulation. Gained reach, legitimacy, and movement cohesion far beyond what university and pulpit networks alone could deliver. Personal recantation was the only available exit and was existentially unavailable to them.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, protestant_reformers, agenda_setter).

% Invested in presses, type, and paper stock, and capitalized speculative editions on the explosive demand for reformist and counter-reformist print. Decided what to print on market signal, followed demand across confessional lines — several prominent shops served both sides at different moments — and obtained imperial and civic privileges granting temporary exclusivity on lucrative titles. Captured the monetary gains of the arrangement. Exit was unusually good for the era: capital and skills redeployed readily to Bibles, schoolbooks, official orders, or rival confessions.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, master_printers, beneficiary,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, master_printers, agenda_setter).

% Bought inexpensive pamphlets and tracts, passed them hand to hand, and read them aloud to illiterate kin and neighbors, multiplying each edition's audience. Gained direct access to scripture and argument previously mediated by a Latin-educated clergy. Paid pamphlet prices, and afterward lived through the confessional polarization the pamphlet wars helped produce. Could ignore the new medium, revert to manuscripts, or rely on preachers — the choice set stayed open.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, literate_urban_readers, beneficiary,
    moderate, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, literate_urban_readers, payer).

% Lost de facto control over doctrinal dissemination as vernacular print outran every supervisory mechanism it possessed. Responded with book bans, indices, licensed-printing schemes, and its own counter-print campaigns, all funded from its own revenues. Could not withdraw from the contest without surrendering its constitutive claim to teach and govern doctrine, so it bore the costs of defending a position it could not abandon.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_church_hierarchy, payer,
    institutional, civilizational, identity_locked, continental).

% Issued successive edicts mandating the seizure of presses and the burning of proscribed books, and administered a licensing regime intended to govern the print trade. The regime persistently lagged the decentralized network it policed: edicts were undercut by jurisdictional fragmentation, cross-border printing, and the sheer speed of reprinting. Bore escalating enforcement expenditures that never achieved closure, and could not relax enforcement without conceding the information terrain.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, imperial_censorship_authorities, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, imperial_censorship_authorities, payer).

% Watched commissioned copying work migrate to print shops across the interval as patrons redirected spending toward cheaper printed editions. Some senior scribes adapted into proofreading, compilation, and publishing-house roles; others, whose decades of training were bound to the manuscript trade, lost their livelihoods outright. Nothing was taken from them by force — demand simply moved — and their specialized skills offered few adjacent landing places.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, manuscript_scribes_and_scriptoria, payer,
    moderate, biographical, trapped, regional).

% Reconstruct deployment decisions from colophons, printer contracts, privilege grants, and reformer correspondence, and adjudicate between rival causal accounts of the press-Reformation relationship. Hold no position inside the arrangement under study; their outputs feed the scholarly process that carries authority over the kernel this story reads.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, historians_of_the_book, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__strategic_deployment, master_printers).
narrative_ontology:fixing_cost_class(press_reformation_causation__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reproduced identical texts at scale and at falling marginal cost, letting geographically dispersed allies act on the same arguments, liturgies, and translations without central logistics — solving the many-hands-one-message problem faster than any authority could respond, and giving scattered communities confirmation, through shared printed formulas, that they belonged to a single movement.
% TRANSFER_FUNCTION: Moves money from readers and subscribers to master printers and their creditors; moves attention and legitimacy toward reformer-authors; moves doctrinal content from movement centers (Wittenberg, Basel, Geneva, Strasbourg) outward to dispersed urban audiences; and moves enforcement burden onto imperial and ecclesial authorities obliged to police what they can no longer contain.
% ABSENT_VOICES: Illiterate rural majorities consumed the arrangement's consequences through pulpit, rumor, and eventually war, but were never addressed by or consulted in the print discourse. Women were largely excluded from the print trades and from theological authorship while being enrolled as readers and household transmitters. Dispossessed scribes had no seat in the councils deciding the medium's future. Catholic laypeople watched their devotional inheritance contested without their participation. All stood outside the conversation that redistributed their informational world.
% DISAPPEARANCE_RATIONALE: If the deployment practice vanished overnight — printers refusing reformist commissions and reformers reverting to manuscript and pulpit — the movement's spread would have run at manuscript speed, no mass public would have crystallized before imperial suppression caught up, printers would have lost the demand boom that kept their presses solvent, and readers would have lost the cheap access that restructured lay religiosity. The confessional map of Europe would look materially different; the arrangement organizes real, load-bearing dependencies.
% FOUNDING_PROBLEM: Two converging problems circa 1517: reformers needed to circulate their case faster than authorities could suppress it and wider than universities and parishes reached; printers, facing saturated post-incunabula markets and expensive idle capacity, needed high-volume recurring demand to amortize their capital. Strategic deployment joined the two.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of the book document the shift from crisis pamphleteering to routine confessional publishing by the 1530s-1540s (establishment of territorial church orders, official presses, and consistories administering settled doctrine), and surviving printer ledgers show the trade migrating from speculative polemic to steady-state products such as Bibles, schoolbooks, and authorized orders. Contemporary institutional evidence — the Peace of Augsburg's regularization of confessional print landscapes — attests that the emergency the arrangement was built to fight had been resolved, while the publishing economy it created plainly persisted. No party inside the arrangement disputes the resolution; the beneficiaries' own successors treated the founding emergency as closed.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).
:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45 (interval end): real gains flowed to identifiable seats — printers captured the monetary surplus, reformers converted reach into movement power — but the reading assesses most of the flow as return on capital risk, editorial labor, and coordination service, with rents entering mainly through imperial and civic printing privileges and late-interval consolidation. Suppression is low (0.16) and its temporal series is deliberately near-flat: the arrangement was not held in place by an enforcement ratchet but by profitability and network momentum, which distinguishes its trajectory from extractive arrangements whose enforcement machinery hardens over time. Theater ratio is low throughout (0.06 rising to 0.14): the press's function was performed, not performed-at; the slight rise reflects routinized confessional publishing (official church orders, anniversary editions) displacing crisis pamphleteering after the 1530s. Accessibility collapse is low (0.22) because alternatives persisted and thrived alongside the arrangement — manuscript circulation, oral preaching, broadsheet song, and sermon networks all remained viable channels; the press added capacity rather than foreclosing exits. Resistance is moderate (0.38): book bans, press seizures, and prosecutions were real and recurrent (Worms 1521, Speyer 1529, proliferating municipal ordinances) but largely ineffective against the deployment. All three tracked series share one eight-point time grid (1517-1555) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent classifications from identical structural data. From the printer and reformer seats, the arrangement appears as opportunity and instrument: a coordination capacity they consciously harnessed, with gains proportional to initiative and risk. From the church-hierarchy and censorship seats, the same arrangement appears as siege: a hostile displacement of their constitutive function that they were obliged to resist at escalating cost. From the reader seat it appears as mixed gain — cheap access purchased with confessional strife. From the scribal seat it appears as quiet competitive extinction. The engine computes these per-seat divergences from power, exit, and directional position; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The three declared beneficiary groups anchor the low-directionality end: reformers (gains in reach and legitimacy, identity-bound to the cause), printers (gains in revenue, with arbitrage-grade exit — many shops printed for multiple confessional clients and switched product lines freely), and readers (gains in access, mobile across media). No victims array is declared, and this omission is the reading's defining structural claim, not an oversight: the costs borne by the church hierarchy, the censorship authorities, and the scribes are displacement-and-defense costs — losses of market position and enforcement expenditure — not transfers routed to the beneficiaries through the arrangement. Nobody was compelled into the new channel; the incumbent lost position to a superior deployment. The omega variables route the challenge to precisely this claim: if printer gains prove to be privilege-backed rents, or readers prove net-worse-off, or censorship proves structurally impossible rather than outmaneuvered, victim classes emerge or causal credit migrates, and the computed classification should drift accordingly. No directionality overrides are used: the derivation from declared beneficiaries and exit options captures the seat structure without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — circulate the reformers' case faster than authorities could suppress it, and give printers volume demand to amortize idle presses after the post-incunabula slump — was resolved by success: by the 1540s the emergency pamphlet phase had given way to routine confessional publishing under territorial church orders. The founding-problem status is therefore authored dead while the disappearance verdict remains world_rearranges, because the routinized publishing economy the arrangement became is load-bearing for everything downstream. That mismatch combination flags the capture/zombie check against the computed path; with theater_ratio held low and no concentrated seat maintaining performance in place of function, no degraded-institution signature is expected — the arrangement transformed rather than atrophied. The classification discipline cuts both ways: against the determinism sibling, which would naturalize the press into an unstoppable structural force (a false summit over what was a chosen deployment), and against a pure-predation reading, which would mistake printers' profits for extraction through coercion when the arrangement ran on voluntary purchase and repeatable arbitrage. The omegas on rent-versus-return and reader welfare are the tripwires: if either resolves adversely, the hybrid or extractive territory opens and this story's descendants should be re-authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the press_reformation_causation kernel, instantiating the strategic_deployment reading; what would the sibling readings (technological_determinism, mutual_shaping) change structurally if adopted?',
    'Historiographical adjudication: the determinism sibling relocates causation into the technology itself (making the press an inevitable structural force rather than a deployed instrument); the mutual_shaping sibling distributes causation bidirectionally (press affordances shaping reformer strategy and vice versa). Each adoption changes the beneficiary structure, the epsilon referent''s assessment, and the classification.',
    'If determinism prevails, this story''s agency-centered beneficiary structure dissolves and the press itself becomes the operative constraint; if mutual_shaping prevails, the clean beneficiary/payer separation softens into co-evolution and the measured extraction redistributes across seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame position: this file is the strategic_deployment reading of a three-reading kernel.').

omega_variable(
    neutrality_axiom_tenability,
    'Is the reading''s foundational premise that the press was neutral capacity awaiting purposeful use tenable, or does affordance and material-culture evidence override it?',
    'Comparative analysis of the same print technology under different deployment regimes (commercial, devotional, governmental) across the fifteenth and sixteenth centuries: if outcomes systematically track deployer intent regardless of medium properties, neutrality holds; if medium properties constrain which deployments succeed, neutrality fails.',
    'If the neutrality axiom fails, this reading collapses toward mutual_shaping, the axioms lose holdable status, and the classification migrates away from a pure coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_axiom_tenability, conceptual, 'Status of the technology-neutrality premise distinguishing this reading from its siblings.').

omega_variable(
    printer_rent_or_return,
    'Were printer gains economic rents (privilege-backed exclusivity capturing value above cost and risk) or legitimate returns on capital risk and coordination service?',
    'Printer account books, edition economics, and the terms of imperial and civic printing privileges: compare realized margins against prevailing commercial returns for comparable speculative ventures.',
    'Substantial rents would indicate asymmetric accrual riding on the coordination function, drifting the computed classification toward a hybrid coordination/extraction shape; returns-consistent margins would stabilize the pure coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_rent_or_return, empirical, 'Whether the monetary gains flowing to master printers constitute rent or compensation.').

omega_variable(
    reader_welfare_position,
    'Did literate urban readers sit net on the gaining side, or were they mobilized as instruments and financiers of an elite confessional struggle they did not choose?',
    'Reading-market data: pamphlet prices against day wages, literacy growth, private library inventories, and testimony of reading practices; weigh access gains against the confessional strife readers subsequently endured.',
    'If readers are net losers, a victim class emerges inside the arrangement and the computed classification drifts away from pure coordination toward a hybrid or extractive shape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_welfare_position, empirical, 'Whether the reader seat belongs among the gaining parties or bears hidden costs.').

omega_variable(
    censorship_failure_attribution,
    'Did censorship fail because deployers strategically outmaneuvered it (this reading''s claim) or because the print system made suppression structurally impossible (the determinism sibling''s claim)?',
    'Compare jurisdictions where enforcement capacity and deployer networks varied independently: if determined authorities with dense networks still failed uniformly, structural impossibility gains weight; if failure correlates with deployer counter-tactics (decentralized printing across borders, format agility, timed releases), strategic deployment gains weight.',
    'Structural-impossibility findings would transfer causal credit from the deployer seats to the technology, eroding this reading''s agency-first structure and moving the story toward its determinism sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_failure_attribution, empirical, 'Attribution of the enforcement failure that this reading credits to deployer strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__strategic_deployment, theater_ratio, 1517, 0.06).
narrative_ontology:measurement(pres_tr_t1521, press_reformation_causation__strategic_deployment, theater_ratio, 1521, 0.07).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causation__strategic_deployment, theater_ratio, 1525, 0.08).
narrative_ontology:measurement(pres_tr_t1529, press_reformation_causation__strategic_deployment, theater_ratio, 1529, 0.09).
narrative_ontology:measurement(pres_tr_t1534, press_reformation_causation__strategic_deployment, theater_ratio, 1534, 0.1).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__strategic_deployment, theater_ratio, 1540, 0.11).
narrative_ontology:measurement(pres_tr_t1546, press_reformation_causation__strategic_deployment, theater_ratio, 1546, 0.13).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causation__strategic_deployment, theater_ratio, 1555, 0.14).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__strategic_deployment, base_extractiveness, 1517, 0.28).
narrative_ontology:measurement(pres_be_t1521, press_reformation_causation__strategic_deployment, base_extractiveness, 1521, 0.33).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causation__strategic_deployment, base_extractiveness, 1525, 0.37).
narrative_ontology:measurement(pres_be_t1529, press_reformation_causation__strategic_deployment, base_extractiveness, 1529, 0.39).
narrative_ontology:measurement(pres_be_t1534, press_reformation_causation__strategic_deployment, base_extractiveness, 1534, 0.41).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__strategic_deployment, base_extractiveness, 1540, 0.43).
narrative_ontology:measurement(pres_be_t1546, press_reformation_causation__strategic_deployment, base_extractiveness, 1546, 0.44).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causation__strategic_deployment, base_extractiveness, 1555, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causation__strategic_deployment, suppression_requirement, 1517, 0.1).
narrative_ontology:measurement(pres_su_t1521, press_reformation_causation__strategic_deployment, suppression_requirement, 1521, 0.12).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__strategic_deployment, suppression_requirement, 1525, 0.12).
narrative_ontology:measurement(pres_su_t1529, press_reformation_causation__strategic_deployment, suppression_requirement, 1529, 0.13).
narrative_ontology:measurement(pres_su_t1534, press_reformation_causation__strategic_deployment, suppression_requirement, 1534, 0.13).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causation__strategic_deployment, suppression_requirement, 1540, 0.14).
narrative_ontology:measurement(pres_su_t1546, press_reformation_causation__strategic_deployment, suppression_requirement, 1546, 0.15).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causation__strategic_deployment, suppression_requirement, 1555, 0.16).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the printing press and the Reformation' decomposes into three structurally distinct constraints sharing one kernel. This file (strategic_deployment) authors epsilon for the deployment arrangement as the agency-first reading assesses it: moderate extraction concentrated in printer gains, low suppression, alternatives intact. The technological_determinism sibling authors epsilon for the claim that the medium itself was the operative cause — a structurally different constraint in which the deployer seats dissolve into carriers of an impersonal force. The mutual_shaping sibling authors epsilon for the co-evolutionary arrangement, in which beneficiary and payer positions blur. The upstream/downstream pressure runs from this reading toward the determinism sibling: every documented instance of deployer choice (Luther's format strategy, printers' cross-confessional arbitrage) is cited as evidence against medium-level inevitability, so this file's empirical content conditions its sibling's legitimacy conditions without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
