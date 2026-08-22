% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Prohibition as Sovereign-Revocable Positive Law (Pragmatic Sanction Reading)
 *   domain: constitutional/political-historical
 *
 * SUMMARY:
 *   Under this reading, the exclusion of women from succession is not natural
 *   or divine law but enacted positive law: it binds because a competent
 *   sovereign authority laid it down, and it remains within that authority's
 *   gift to amend or revoke. The paradigm instrument is the Pragmatic
 *   Sanction of 1713, by which the Habsburg head of house secured his
 *   daughter's eventual succession across his scattered crowns, obtained
 *   foreign guarantees for it over two decades, and — when challengers
 *   treated the settlement as negotiable by force — saw it defended by
 *   coalition war until the Treaty of Aix-la-Chapelle confirmed it. On this
 *   reading the challengers are rebels against legitimate authority, and the
 *   war that defends a sanctioned female succession is defensive. CONSTRAINT
 *   FAMILY NOTE: the colloquial label 'Salic Law' decomposes into three
 *   structurally distinct constraints linked by network.affects_constraints.
 *   This file authors epsilon for the standing arrangement as the
 *   sovereign-override reading assesses it: a positive-law exclusion,
 *   legitimate while unrevoked, dispossessing female dynastic lines,
 *   enforceable at war cost. The immutable_mandate_reading sibling authors a
 *   materially higher suppression and near-total accessibility collapse (no
 *   legislative exit exists at all); the cognatic_reversion_reading sibling
 *   shifts the cost-bearing set toward territorial estates and lowers
 *   extraction at the dynastic core. The epsilon values differ because the
 *   constraints differ — not because one arrangement is measured with
 *   different observables. CLAIM/METRIC INDEPENDENCE: the claimed type and
 *   the metric values below are independently authored facts; the engine
 *   computes per-seat classifications from the structural data, and any
 *   divergence between the claim and computed types is the measurement the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - sovereign_legislator: Agenda-setter (institutional/arbitrage) — administers the councils, parlements, and chanceries that draft, register, and proclaim succession acts; holds the act that maintains or lifts the exclusion
 *   - male_agnate_dynasts: Principal beneficiary (powerful/constrained) — ranks ahead of every daughter or sister while the male-only ordering stands
 *   - cadet_branch_claimants: Secondary beneficiary (organized/constrained) — their standing exists only because senior-line females are ranked out
 *   - noble_fief_holders: Beneficiary with payer exposure (organized/constrained) — royal precedent shores up male-preference estate settlements; their taxes and levies fund the resulting wars
 *   - excluded_female_heirs: Principal bearer of costs (moderate/identity_locked) — barred from succeeding in their own right; remedy runs only through persuading the sovereign to legislate
 *   - female_line_descendants: Secondary bearer of costs (powerless/constrained) — claims extinguished at birth through no act of their own
 *   - succession_war_populations: Diffuse bearer of costs (powerless/trapped) — pays, quarters, and absorbs the devastation of the wars the settlements provoke
 *   - challenger_powers: Excluded rival (powerful/arbitrage) — shut out of the guarantee negotiations, litigates the settlement by arms
 *   - international_law_publicists: Analytical observer (analytical/analytical) — adjudicates in print whether one generation's act can bind the next and whether guarantees extorted under duress oblige
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.66).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.68).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Prohibition as Sovereign-Revocable Positive Law (Pragmatic Sanction Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/political-historical").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, 'b6a1c700-48bd-4f80-81cb-998d34651394').
narrative_ontology:cs_kernel_codification('b6a1c700-48bd-4f80-81cb-998d34651394', distributed).
narrative_ontology:cs_authority_grounding('b6a1c700-48bd-4f80-81cb-998d34651394', lineage).
narrative_ontology:cs_interpretation_layer_present('b6a1c700-48bd-4f80-81cb-998d34651394').
narrative_ontology:cs_reading_relation('b6a1c700-48bd-4f80-81cb-998d34651394', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('b6a1c700-48bd-4f80-81cb-998d34651394', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('b6a1c700-48bd-4f80-81cb-998d34651394', foundational, succession_binding_force_is_sovereign_enactment).
narrative_ontology:cs_axiom_status(succession_binding_force_is_sovereign_enactment, holdable).
narrative_ontology:cs_axiom_grounding('b6a1c700-48bd-4f80-81cb-998d34651394', succession_binding_force_is_sovereign_enactment, conventional).
narrative_ontology:cs_axiom('b6a1c700-48bd-4f80-81cb-998d34651394', secondary, defense_of_sanctioned_succession_is_defensive_war).
narrative_ontology:cs_axiom_status(defense_of_sanctioned_succession_is_defensive_war, holdable).
narrative_ontology:cs_axiom_grounding('b6a1c700-48bd-4f80-81cb-998d34651394', defense_of_sanctioned_succession_is_defensive_war, instrumental).
narrative_ontology:cs_reference_frame('b6a1c700-48bd-4f80-81cb-998d34651394', sovereign_enacted_succession_order).
narrative_ontology:cs_drift_state('b6a1c700-48bd-4f80-81cb-998d34651394', post_aix_la_chapelle_settlement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b6a1c700-48bd-4f80-81cb-998d34651394', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, male_agnate_dynasts).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, cadet_branch_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, noble_fief_holders).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, excluded_female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_line_descendants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, succession_war_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, noble_fief_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the crown's legislative and registrative machinery — privy councils, parlements, chanceries — that drafts, registers, and proclaims acts touching the succession. Used it in 1713 to secure his several crowns for his daughter's line by the Pragmatic Sanction, then spent two decades buying foreign guarantees for the act. Can in principle re-enact or revoke the succession rule by the same route; in practice each such act obliges him to defend it against every dynast the act disadvantages, and the defense bill is paid by others.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, sovereign_legislator, agenda_setter,
    institutional, generational, arbitrage, continental).

% Brothers, uncles, and nephews of reigning princes. While the male-only ordering stands they rank next in line ahead of any daughter or sister, and several houses owe their elevation entirely to that ordering. When a sanction admits a woman, their claims convert into grounds for war or for negotiated compensation; they lobby councils, contract strategic marriages, and wait for openings. Leaving the game means surrendering the position the ordering alone gives them.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, male_agnate_dynasts, beneficiary,
    powerful, generational, constrained, continental).

% Distant male cousins whose lines would rank behind senior-line daughters under equal primogeniture. The male-only ranking is the sole source of their standing; without it their claims lapse into courtesy titles. They cultivate juristic argument and foreign patronage to keep dormant claims alive, and their fortunes rise and fall with each redrawing of the succession map.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, cadet_branch_claimants, beneficiary,
    organized, generational, constrained, continental).

% Territorial nobility whose own estates descend by male-preference customs mirroring the royal rule; royal precedent shores up their family settlements against daughters' claims. The same households fund and man the wars fought over successions — paying extraordinary taxes, quartering troops, and sending sons as officers when dynastic contests turn violent.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, noble_fief_holders, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, noble_fief_holders, payer).

% Daughters and sisters of sovereigns. Barred from succeeding in their own right while the ordering stands; their marriages are instruments of alliance policy, and their children rank through their fathers. A princess cannot resign the house she is born into — her dynastic identity is fixed at birth and constituted by membership in the order that ranks her out. Her only remedy runs through persuading the sovereign to legislate, as Maria Theresa ultimately succeeded under her father's sanction and then spent a war defending it.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, excluded_female_heirs, payer,
    moderate, biographical, identity_locked, national).

% Children and grandchildren who trace claims through mothers or grandmothers. Their succession rights are extinguished at birth regardless of merit or proximity of blood; most pursue ecclesiastical, military, or marital careers in other houses, carrying grievances that surface whenever the settlement weakens.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_line_descendants, payer,
    powerless, biographical, constrained, continental).

% Peasants and townsfolk of the contested territories — Silesia, Bohemia, Bavaria, the Austrian Netherlands during 1740-1748. They pay the extraordinary taxes, quarter the marching armies, absorb requisitions and devastation, and die in sieges, without any voice in the councils or congresses that dispose of them.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, succession_war_populations, payer,
    powerless, biographical, trapped, regional).

% Neighboring crowns — Prussia, Bavaria, Spain, Saxony — holding claims or ambitions disadvantaged by the sanctioned order. Shut out of the guarantee negotiations that settled the Sanction, they declined to treat its terms as final: Frederick II occupied Silesia within weeks of the emperor's death in 1740, and the Elector of Bavaria pressed a claim to the Habsburg inheritance through his wife. Their recourse is arms and shifting alliances, and they bear the costs of the wars they initiate.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, challenger_powers, excluded,
    powerful, biographical, arbitrage, continental).

% Jurists and publicists in the republic of letters who assess whether a sovereign's act can bind third parties and unborn generations, whether guarantees extracted under duress oblige, and whether a daughter's succession under a registered sanction outweighs an agnate's customary claim. They collect no revenue from the arrangement and bear none of its levies; their treatises supply the arguments every other seat borrows.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, international_law_publicists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, sovereign_legislator).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a publicly knowable order of succession in advance: anyone can compute who reigns next without every death reopening a contest among all plausible claimants, and the realm avoids partition among an heiress's husband's lands or absorption by a foreign crown through a queen's marriage.
% TRANSFER_FUNCTION: Moves succession eligibility — and with it sovereignty over territories, revenues, and offices — exclusively along male agnatic lines, diverting it away from female lines; moves the authority to redefine eligibility into the sovereign's legislative act; and, once a sanction is proclaimed, moves the costs of defending it (taxation, conscription, devastation) onto the populations of the contested territories.
% ABSENT_VOICES: The women whose claims were disposed of sat outside every council that drafted, registered, and guaranteed the settlements; so did the taxpayers and conscripts of the ensuing wars. The Sanction's guarantees were negotiated among male sovereigns and their ministers; the estates most affected learned the terms as subjects, and the rival powers learned them as a fait accompli they then refused.
% DISAPPEARANCE_RATIONALE: Overnight repeal would reopen every dynastic position at once: senior-line daughters and their sons would outrank cadet males, existing land settlements keyed to male preference would face challenge, and the 1748 map — Maria Theresa's crowns, Silesia's annexation, the Bourbon-Habsburg marriage geometry — would lose its legal floor and be renegotiated by treaty or by force. Arrangements across the continent depend on the ordering having been fixed and defended.
% FOUNDING_PROBLEM: The Capetian succession crises of 1316-1328: three kings died in quick succession without surviving sons, and the kingdom faced partition among heiresses' husbands — including the king of England, claimant through Isabella. The exclusion of female transmission was assembled to keep the realm intact and out of foreign hands.
% FOUNDING_PROBLEM_CORROBORATION: The chroniclers of the 1316-1328 assemblies and the recorded deliberations of the prelates and barons attest the anti-partition, anti-foreign-heir motive from outside any dynastic beneficiary; modern legal historians corroborate it while disputing how deliberately the doctrine was assembled. On current liveness the parties split: dynastic houses that lost by the rule attest its obsolescence from the losing side, while the guarantor chancelleries of the eighteenth century acted on the problem as live enough to fight over.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.66 at interval end) because the ordering divests an entire class of dynasts of succession rights at birth, redirects crowns and revenues along male lines, and — after a sanction — adds wartime fiscal extraction to defend the redirected order; it stops short of snare-level because the governed class is narrow and the arrangement carries a real settlement function. Suppression (0.68) is authored as a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness passes through directionality and scope modifiers in the engine's computation. Theater is low-moderate (0.25): the registrative and juristic machinery performed real settlement work, with a performative hump in the humanist era when advocates dressed an admittedly enacted rule as immemorial custom. Accessibility collapse is moderate (0.45): alternatives — cognatic succession, testamentary disposition, electoral arrangement — remained visible and were in fact used, which is precisely what distinguishes this reading from the immutable sibling. Resistance is high (0.65): parlementary refusals to register, challenger coalitions, and the Silesian seizure. TEMPORAL GRID: one shared grid serves all three tracked metrics (points 0, 7, 14, 21, 28, 35, 42; T0 anchors 1328 when the assemblies' exclusion rulings hardened into standing practice; T14 approximates the era when the 'Salic' label entered juristic circulation; T42 pins 1748, Aix-la-Chapelle). The suppression_requirement series is authored because enforcement capacity visibly changed over the interval — from customary acquiescence, through registrative and oath-based machinery, to coalition warfare in defense of the Sanction, with partial relaxation after the 1748 confirmation; the rise-then-ease shape records that ratchet and release. Extractiveness accumulates slowly as the rule outlived its anti-partiment origin and increasingly operated as an instrument for disposing of crowns.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit incompatible worlds under the same rule. From the sovereign seat the arrangement is ordinary legislation and dynastic prudence: an act is drafted, registered, proclaimed, guaranteed — and obedience is simply what law is. From the female-line seats the identical structure is dispossession by other people's acts: rights extinguished at birth, marriages converted into instruments of someone else's alliance policy. From the challenger seats it is usurpation by paperwork — a private family arrangement dressed as public law and then enforced with armies. From the publicists' seat it is the standing question of whether one generation can bind the next. The engine computes this divergence per seat from the power and exit asymmetries in the structural data; the authored claim does not adjudicate it. COALITION NOTE: the principal cost-bearing seats had latent coalition potential (dispossessed heiresses plus their descendants plus war-weary estates) that was structurally blocked — marriage politics dispersed each princess's interests into her husband's house, and the war-bearing populations had no forum in which to combine.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign_legislator sits nearest the beneficiary end: the arrangement subsidizes the seat that controls it, converting succession disputes into acts of will (low d). Male_agnate_dynasts and cadet_branch_claimants are declared beneficiaries with constrained exit — their positions exist only inside the ordering — placing them near the beneficiary end, though neither collects the arrangement's concentrated gains. Noble_fief_holders benefit indirectly (precedent reinforcement for parallel estate customs) while paying war taxes, so the derivation would place them too close to the full-beneficiary end; a directionality override for the organized power atom (d = 0.22) moderates both organized beneficiary seats toward their true, derivative position. Excluded_female_heirs and female_line_descendants are declared victims: d sits near the target end, amplified for the heiress seat by identity-lock — a princess cannot resign the house she is born into, and her dynastic self-concept is constituted by membership in the very order that ranks her out; if that identity frame broke (as it partially did for ruling queens who governed successfully), the effective extraction on the seat would drop sharply. Succession_war_populations are victims with trapped exit: high d, no mobility. Challenger_powers carry no beneficiary or victim declaration — they neither systematically pay nor collect under the arrangement, they contest it — so their d is left to the engine's canonical fallback rather than forced by an override that, being keyed to the power atom alone, would also strike the primary beneficiary seat sharing that atom; this residual is accepted rather than papered over.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping the realm intact and out of foreign hands when a king died without sons — was substantially obsolete by the eighteenth century: realms no longer fragmented on successions, and the live danger had inverted (the fear was now that a female succession would import a foreign consort's ambition, which the Sanction's marriage diplomacy addressed directly). Yet the arrangement not only persisted, it gained fresh work: the sovereign seat converted the succession rule into an instrument for disposing of crowns, and the guarantee-and-war machinery gave it enforcement teeth the original custom never had. The R5 interview surfaces exactly this mismatch — a founding problem in contested retirement attached to a very much alive redistributive machine. Classifying the arrangement as tangled_rope is what prevents both failure modes: rope-laundering (calling it pure coordination, which would erase the female lines and war populations who pay for the settlement) and snare-flattening (calling it pure extraction, which would erase the genuine settlement function that made every succession computable in advance). The engine's per-seat computation keeps both truths on the books: the seat that legislates experiences coordination; the seats that are legislated-about experience extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salic_kernel_reading_location,
    'This constraint is one reading (sovereign_override_reading) of the salic_prohibition kernel: the exclusion of female succession binds as enacted positive law, revocable by sovereign legislative act. Sibling readings instantiate different constraints from the same kernel: immutable_mandate_reading (binding force is natural/divine and irrevocable) and cognatic_reversion_reading (the rule was never properly received outside Frankish custom). Where exactly is the disagreement located?',
    'Comparative classification of the sibling stories plus reception history: the dispute turns on the source of binding force (sovereign enactment vs. divine/natural mandate vs. failed reception), resolvable by examining which authorities each reading recognizes as competent to speak for the rule.',
    'Each reading yields a different constraint with a different victim set and different epsilon: the immutable sibling authors higher suppression and near-total accessibility collapse (no legislative exit exists); the cognatic sibling shifts costs toward territorial estates and lowers dynastic-core extraction. Classifying this file''s arrangement under a sibling''s premises would misstate who is bound and who may be released.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(salic_kernel_reading_location, conceptual, 'Committer-frame routing: one kernel, three readings, disagreement located in the source of the rule''s binding force.').

omega_variable(
    revocability_in_practice,
    'Was the prohibition actually revocable by legislative act alone, or did every real-world override require winning a war (as the 1740-1748 contest over the Pragmatic Sanction suggests)?',
    'Comparative case study of succession changes enacted by legislative bodies elsewhere and elsewhen (e.g., parliamentary alterations of succession in maritime kingdoms) versus dynastic overrides defended by arms: count how many overrides took effect without major war.',
    'If peaceful overrides are routine, the positive-law frame is substantively real and the measured lock-in is overstated; if every override required victory in the field, the revocability premise is largely nominal and this arrangement behaves closer to the immutable sibling than its own theory admits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revocability_in_practice, empirical, 'Whether the reading''s defining feature (legislative revocability) operated in practice or only on paper.').

omega_variable(
    override_power_capture,
    'Was the override power exercised as impartial legislation about the succession, or as personal dynastic disposition — a sovereign converting the public law of succession into an instrument for settling crowns on his own issue?',
    'Examine the drafting and guarantee-seeking record of the 1713 Sanction and comparable acts: whether stated public reasons (general settlement of the succession) track the actual distribution of advantages produced.',
    'If capture is established, the coordination-function weight of the arrangement drops, the concentration of gains in the sovereign seat hardens, and the classification drifts toward extraction-dominant; if the acts show genuine general-rule character, part of the measured extraction is the price of the settlement function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_power_capture, conceptual, 'Whether the sovereign''s legislative override served the office or the officeholder''s bloodline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_sov_read_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(salic_sov_read_tr_t7, salic_prohibition__sovereign_override_reading, theater_ratio, 7, 0.23).
narrative_ontology:measurement(salic_sov_read_tr_t14, salic_prohibition__sovereign_override_reading, theater_ratio, 14, 0.27).
narrative_ontology:measurement(salic_sov_read_tr_t21, salic_prohibition__sovereign_override_reading, theater_ratio, 21, 0.3).
narrative_ontology:measurement(salic_sov_read_tr_t28, salic_prohibition__sovereign_override_reading, theater_ratio, 28, 0.29).
narrative_ontology:measurement(salic_sov_read_tr_t35, salic_prohibition__sovereign_override_reading, theater_ratio, 35, 0.26).
narrative_ontology:measurement(salic_sov_read_tr_t42, salic_prohibition__sovereign_override_reading, theater_ratio, 42, 0.25).

% Extraction over time
narrative_ontology:measurement(salic_sov_read_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(salic_sov_read_be_t7, salic_prohibition__sovereign_override_reading, base_extractiveness, 7, 0.59).
narrative_ontology:measurement(salic_sov_read_be_t14, salic_prohibition__sovereign_override_reading, base_extractiveness, 14, 0.6).
narrative_ontology:measurement(salic_sov_read_be_t21, salic_prohibition__sovereign_override_reading, base_extractiveness, 21, 0.61).
narrative_ontology:measurement(salic_sov_read_be_t28, salic_prohibition__sovereign_override_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement(salic_sov_read_be_t35, salic_prohibition__sovereign_override_reading, base_extractiveness, 35, 0.64).
narrative_ontology:measurement(salic_sov_read_be_t42, salic_prohibition__sovereign_override_reading, base_extractiveness, 42, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(salic_sov_read_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(salic_sov_read_su_t7, salic_prohibition__sovereign_override_reading, suppression_requirement, 7, 0.51).
narrative_ontology:measurement(salic_sov_read_su_t14, salic_prohibition__sovereign_override_reading, suppression_requirement, 14, 0.57).
narrative_ontology:measurement(salic_sov_read_su_t21, salic_prohibition__sovereign_override_reading, suppression_requirement, 21, 0.62).
narrative_ontology:measurement(salic_sov_read_su_t28, salic_prohibition__sovereign_override_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(salic_sov_read_su_t35, salic_prohibition__sovereign_override_reading, suppression_requirement, 35, 0.7).
narrative_ontology:measurement(salic_sov_read_su_t42, salic_prohibition__sovereign_override_reading, suppression_requirement, 42, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% Constraint family from kernel decomposition (epsilon-invariance principle): the colloquial label 'Salic Law' covers three structurally distinct claims, written as three stories. The immutable_mandate_reading is historically upstream — its divine/natural framing was the ground earlier jurists invoked, and its near-zero accessibility collapse makes it the strongest form of the prohibition. This sovereign_override_reading is downstream: the positive-law premise is what made the Pragmatic Sanction legally thinkable, and the Sanction's success pressures the immutable sibling by demonstrating that crowned heads themselves treated the rule as revisable. The cognatic_reversion_reading runs parallel rather than upstream/downstream: it attacks reception, not revocability, and coexists jurisdictionally with this reading (a rule can bind as revocable positive law where received and never have bound where not). Each member links to the others via affects_constraints; each carries its own epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
