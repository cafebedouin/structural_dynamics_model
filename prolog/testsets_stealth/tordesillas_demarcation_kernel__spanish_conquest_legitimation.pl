% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant as License for Territorial Conquest and Indigenous Subjugation West of the Line
 *   domain: international law/colonial history/sovereignty theory
 *
 * SUMMARY:
 *   The 1493 papal bulls and the 1494 Luso-Castilian treaty drew a line
 *   through the Atlantic and assigned its western side to Castile. This story
 *   instantiates ONE reading of that instrument complex: the
 *   spanish_conquest_legitimation reading, in which the western grant
 *   operates as a license for territorial conquest and the subjugation of the
 *   peoples living there — operationalized through the Requerimiento's
 *   ultimatum ritual, the encomienda labor-and-tribute grants, forced
 *   resettlement, and conversion under duress. The constraint is the
 *   legitimation-plus-enforcement structure binding crown, settlers, clergy,
 *   and treasury into a single authorized machine. Claim and metrics are
 *   independent authored facts: the claimed_type (snare) states my structural
 *   belief; the metrics state what I judge descriptively true of the
 *   arrangement's operation. The sibling reading
 *   (portuguese_exploration_legitimation) is a DIFFERENT constraint with its
 *   own epsilon, authored separately and linked via
 *   network.affects_constraints — per the epsilon-invariance principle, the
 *   colloquial label 'Treaty of Tordesillas' decomposes into two stories, not
 *   one. KEY AGENTS (by structural relationship): - spanish_crown: Primary
 *   beneficiary and agenda-setter (institutional/arbitrage) — collects the
 *   fiscal yield and controls the legitimacy frame - encomendero_class:
 *   Concentrated settler beneficiary (powerful/constrained) — holds the
 *   labor-and-tribute grants - indigenous_nations_west_of_line: Primary
 *   target (powerless/trapped) — bears conquest, land loss, and labor
 *   exaction - indigenous_commoner_tributaries: Diffuse household-level
 *   target (powerless/trapped) - papacy: Original issuing authority turned
 *   dependent principal (institutional/constrained) - rival_european_powers:
 *   Excluded rivals (powerful/arbitrage) — barred access, reject the premise
 *   - lascasian_dominican_advocates: Internal dissent, structurally
 *   overridden (moderate/constrained) - salamanca_school_jurists: Analytical
 *   observer (moderate/analytical) — contests the title's validity -
 *   cacique_intermediaries: Dual-positioned intermediaries
 *   (moderate/constrained) - missionary_orders: Institutional beneficiary
 *   housing internal critics (organized/constrained)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.9).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.86).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as License for Territorial Conquest and Indigenous Subjugation West of the Line").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international law/colonial history/sovereignty theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4').
narrative_ontology:cs_kernel_codification('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', fixed_text).
narrative_ontology:cs_authority_grounding('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', lineage).
narrative_ontology:cs_interpretation_layer_present('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4').
narrative_ontology:cs_reading_relation('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', foundational, papal_donation_confers_dominion_title).
narrative_ontology:cs_axiom_status(papal_donation_confers_dominion_title, holdable).
narrative_ontology:cs_axiom_grounding('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', papal_donation_confers_dominion_title, conventional).
narrative_ontology:cs_axiom('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', foundational, infidel_polities_lack_lawful_dominion).
narrative_ontology:cs_axiom_status(infidel_polities_lack_lawful_dominion, holdable).
narrative_ontology:cs_axiom_grounding('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', infidel_polities_lack_lawful_dominion, theological).
narrative_ontology:cs_reference_frame('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', papal_plenitude_partition_order).
narrative_ontology:cs_drift_state('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', late_sixteenth_century_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fed28ffe-32a2-4a14-a9e7-e8b2ffdb6aa4', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomendero_class).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_nations_west_of_line).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_commoner_tributaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, viceregal_administrators).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, cacique_intermediaries).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, cacique_intermediaries).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_plenitudo_potestatis_doctrine).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, discovery_doctrine).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, requerimiento_consent_fiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1493 bulls drawing a north-south line through the Atlantic and assigning lands west of it to the Castilian crown under its spiritual authority, and adjudicated subsequent disputes over the line's placement. Its ability to police the arrangement depended on the crowns it licensed; once the Iberian crowns secured rights of presentation over colonial church offices, it could discipline the arrangement only by persuasion.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papacy, agenda_setter,
    institutional, civilizational, constrained, universal).

% Holds the western grant as its title of possession. Receives the royal fifth on precious metals, shares of head-tribute, customs, and later direct taxation of the mining economy. Issues cedulas regulating labor and settlement, alternately restraining and extending settler prerogatives as fiscal needs and dynastic politics dictate, and can reframe its title (just war, occupation, purchase) whenever the original instrument becomes diplomatically costly.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% Metropolitan body that drafts colonial legislation, audits officials through residencia trials, licenses passage and printing, and hears appeals from the Americas. Administers the arrangement on the crown's behalf; its members serve at royal pleasure and carry no independent constituency.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, council_of_the_indies, agenda_setter,
    institutional, generational, constrained, global).

% Viceroys, audiencia judges, and governors who execute royal orders in the Americas, apportion labor drafts, collect quotas, and command garrisons. They draw salaries, fees, and perquisites from the offices they hold and rotate back to Spain at the end of fixed terms.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, viceregal_administrators, agenda_setter,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, viceregal_administrators, beneficiary).

% Settlers holding royal grants entitling them to labor and tribute from designated indigenous communities in exchange for nominal protection and religious instruction. Their wealth and local standing derive from the grants; when reforms threatened grant inheritance they raised armed resistance, as in the Peruvian civil wars of the 1540s. Leaving means abandoning the source of their rank.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomendero_class, beneficiary,
    powerful, biographical, constrained, regional).

% Franciscan, Dominican, and Augustinian houses staffing doctrinas and parishes, baptizing and instructing converts, and receiving tithes, alms, and assigned community labor for church construction. Conversion totals drive their institutional growth; their houses rest on the same labor arrangements some of their members denounce.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders, beneficiary,
    organized, generational, constrained, continental).

% Friars, from the 1511 Advent sermon through Las Casas' campaigns and the Valladolid disputation, who argue the conquest unlawful and the labor regimes ruinous. They preach, petition, and publish inside a system whose printing and preaching licenses the crown controls; their flagship reform passed once and was substantially withdrawn within three years under settler pressure.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, lascasian_dominican_advocates, excluded,
    moderate, biographical, constrained, continental).

% Polities from the Caribbean to Mesoamerica and the Andes that held the land before contact. They lose territory, accumulated wealth, and population to invasion, epidemic, and resettlement decrees, and owe labor and tribute under terms they were never party to setting. Their options are submission, flight beyond patrol reach, or revolt, each punished; recourse to Spanish courts exists but runs on the conquerors' procedures and language.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_nations_west_of_line, payer,
    powerless, generational, trapped, continental).

% Household-level farmers and weavers who pay head-tribute in coin or kind, rotate through labor drafts, and meet fixed textile and crop quotas. Exposure is direct and immediate: a bad harvest meets an unchanged quota.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_commoner_tributaries, payer,
    powerless, immediate, trapped, local).

% Hereditary local lords who collect tribute and organize labor drafts for delivery upward, retaining a portion and customary perquisites. They preserve local standing through collaboration, shield their communities where they can, and answer to both superiors and neighbors when quotas fail.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, cacique_intermediaries, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, cacique_intermediaries, beneficiary).

% French, English, and Dutch crowns and their sea captains, to whom the line assigns nothing. Lawful access west of the line is closed to them; they answer with contraband trade, privateering against treasure fleets, and settlements planted without regard to the demarcation, and they never accept the instrument's premise.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, rival_european_powers, excluded,
    powerful, generational, arbitrage, global).

% Theologians and jurists, notably Francisco de Vitoria, who examine whether the papal instrument can convey dominion over non-Christian polities and conclude it cannot, relocating title questions to the law of peoples. Their lectures circulate through councils and universities; they change arguments at court more readily than conduct in the field.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, salamanca_school_jurists, observer,
    moderate, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocated newly encountered territories between two Catholic crowns without war between them, and aligned crown, settlers, clergy, and financiers behind a single authorization structure for overseas venture: one title chain, one licensing path, one division of spoils.
% TRANSFER_FUNCTION: Moves labor, tribute, land, and mineral wealth from indigenous populations west of the line to the Spanish crown, encomenderos, and the settler-church economy; moves a nominal conversion obligation outward toward the conquered as the stated price of the arrangement.
% ABSENT_VOICES: The indigenous nations themselves were never party to the grant, the treaty, or the terms read to them; their consent was simulated by a proclamation delivered in a language they did not speak. Rival European powers were likewise absent from the allocation that barred them. Both groups would have rejected the premises outright.
% DISAPPEARANCE_RATIONALE: Every colonial title, encomienda grant, episcopal jurisdiction, and treasury claim traced to the papal donation and its demarcation. Overnight disappearance would dissolve the legal basis of possession, strand settler property claims, reopen inter-crown rivalry, and remove the authorization under which the labor regimes operated.
% FOUNDING_PROBLEM: How could Christian princes lawfully claim dominion over newly encountered non-Christian lands, divide them between themselves without fighting each other, and hold a title that Christendom's arbiter recognized?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Salamanca jurists (Vitoria, Soto) concluded from within Spain's own universities that the papal instrument conveyed no dominion over infidel polities; non-party powers (France, England, the Netherlands) refused the premise entirely and acted on that refusal; indigenous testimony recorded in Spanish court proceedings attests no assent was ever given. No source outside the benefiting parties attests the founding problem remained live after the mid-sixteenth century.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.9, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9 at interval end) because the yield — labor, tribute, land, mineral wealth, and lives — is decoupled from any service rendered to those who pay it, and the demographic catastrophe of the sixteenth century is the direct operating cost of the machine. Suppression (0.86) reflects the arrangement's dependence on active force: invasion armies, the Requerimiento's submit-or-be-attacked ultimatum, garrisoned labor regimes, idolatry-extirpation campaigns, and inquisitorial tribunals from 1571. Theater (0.60) is substantial and rising: the Requerimiento was performed in a language its audience could not understand; the reform cycle (Burgos 1512, New Laws 1542, gutted by 1545) redistributed nothing durably; the 1573 ordinances renamed conquest 'pacification' while the extraction continued. Accessibility_collapse (0.72) is high but short of natural-law completeness: indigenous alternatives narrowed to submission, flight, or revolt, and Spanish elites who dissented faced license controls — yet maroonage, frontier flight, and open revolt remained real channels, which is why the value sits below mountain range. Resistance (0.58) is heavy and continuous: Enriquillo's fourteen-year insurgency, the Mixton and Arauco wars, maroon communities, the Las Casas campaign, and Vitoria's lecture-hall assault on the title itself. The measurement series run on ONE shared nine-point grid (1493-1600) so every tracked metric is authored at every examined time point; all three trajectories rise together — extraction accumulates, enforcement machinery hardens (audiencias, viceregal bureaucracies, tribunals), and the legitimation layer grows more theatrical as the doctrine-practice gap widens. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity intensification, not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically differently. From the crown's seat the arrangement is a lawful title chain it inherited, administered, and taxed — an ordered allocation it can reframe at will (arbitrage exit). From the encomendero seat it is earned reward under threat of confiscation — hence armed defense of grant inheritance. From the indigenous seats the same structure is annihilation licensed by a ritual performed on a distant island in an unread language — total exposure with no exit. The papacy's seat shifts across the interval: issuing principal, then dependent spectator once the patronato real placed colonial church appointment in royal hands. The engine derives this divergence from the declared roles, power atoms, and exit options; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (spanish_crown, encomendero_class, missionary_orders) place those seats near the beneficiary pole: the grant subsidizes them, and their exits (arbitrage for the crown, constrained-but-defended position for encomenderos) dampen effective pressure further. Victim declarations (indigenous_nations_west_of_line, indigenous_commoner_tributaries) place those seats near the full-target pole, amplified by trapped exit — flight and revolt are punished, courts run on the conqueror's terms. Cacique_intermediaries sit mid-range through their dual payer/beneficiary position. Rival_european_powers appear in no beneficiary or victim list: the derivation gives them a mid-range d — the arrangement blocks their opportunity rather than taxing their holdings, and their arbitrage exit (contraband, privateering, ignoring the line) keeps them from the target end. No directionality overrides are needed: the only shared power atom among differently-positioned agents ('powerful': encomendero_class vs rival_european_powers) is differentiated by role and exit declarations, which the derivation reads alongside power. Receipt-of-gain is concentrated: over the interval the fiscal yield (royal fifths, tribute shares, composition payments for grant confirmation) accrues demonstrably to the spanish_crown, which is why gain_flow names that seat even though encomenderos skim locally — receipt is not the same fact as beneficiary-role.
 *
 * MANDATROPHY ANALYSIS:
 *   The decomposition is the mandatrophy safeguard. The Tordesillas complex genuinely solved an inter-crown coordination problem — it kept two Catholic powers from naval war over the partition — and a monolithic story would let that real coordination function launder the conquest license (rope-cover for a snare). Splitting the readings assigns the coordination credit to the sibling story and the extraction verdict to this one, each with its own stable epsilon. On the genealogy interview the mismatch signal fires cleanly: founding_problem_status is dead (the problem of a divinely-sanctioned two-crown partition dissolved once non-party powers rejected the premise and the crown itself shifted to occupation-and-just-war framing after 1573) while disappearance_verdict is world_rearranges (every colonial title hung from the grant) — the arrangement persisted for centuries on enforcement after its justification expired, which is the capture/zombie configuration the R5 consumer is built to flag. Identity-lock operates on the settler side: encomenderos fused status with grant-holding (the benemeritos self-conception), and the missionary orders fused institutional purpose with conversion totals; breaking either frame would have changed what the arrangement could demand of them, which is precisely what the New Laws crisis tested and the rebellion settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_structural_delta,
    'This constraint is one reading of the tordesillas_demarcation_kernel; what structurally changes under the sibling reading (portuguese_exploration_legitimation), and where exactly is the disagreement located?',
    'Author the sibling as its own story and compare computed classifications: the Portuguese reading''s parties are the two crowns, its contested object is rival European access east of the line, and its cost-bearing runs between European states rather than over indigenous populations. The disagreement is located in WHAT THE INSTRUMENT LICENSES — dominion over peoples versus exclusion of rivals.',
    'If the sibling computes as rope or tangled_rope between crowns while this reading computes as snare, the colloquial label ''Treaty of Tordesillas'' is confirmed to cover two structurally distinct constraints with widely separated epsilon values, and any aggregate judgment on ''the treaty'' is category error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_delta, conceptual, 'Committer-frame omega: kernel membership, reading identity, and the structural delta against the sibling reading.').

omega_variable(
    papal_title_validity,
    'Could the papal donation convey lawful dominion over non-Christian polities at all, or was it from issuance a constructed instrument serving identifiable recipients?',
    'Juridical-theological assessment from Vitoria forward, plus the revealed preferences of non-party powers: every polity outside the two grantees refused the premise, and the grantee crown itself stopped relying on the title when diplomatically costly.',
    'If the title was void ab initio, the legitimation layer contributes no coordination content and the arrangement rests wholly on force — strengthening the snare reading and raising the theater attribution; if some lawful content existed, part of the measured extraction is misattributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_title_validity, conceptual, 'Whether the grant''s authority was genuine warrant or constructed cover.').

omega_variable(
    grant_load_bearing_or_cover,
    'Did the legitimation causally sustain the arrangement, or merely decorate coercion that would have proceeded identically without it?',
    'Comparative trajectory analysis against expansions lacking any such title: northern-European Atlantic colonization reached comparable extraction intensities without a papal grant, suggesting the license was not load-bearing for the extraction itself.',
    'If cover, the measured epsilon attributes to the enforcement machinery alone and the theater share of the legitimation apparatus rises; if load-bearing, dismantling the title frame would have been a real lever, changing the fixing-cost assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grant_load_bearing_or_cover, empirical, 'Counterfactual test of whether the grant was constitutive or decorative.').

omega_variable(
    internalized_suppression_share,
    'Of the measured suppression on the paying seats, how much is structural (garrisons, labor drafts, legal disability) and how much internalized (evangelization-instilled deference, disrupted cosmologies persisting after direct coercion eased)?',
    'Post-colonial trajectory analysis: where deference structures and quota-shaped economic behavior persist generations after the enforcement machinery dissolved, the internalized share is substantial.',
    'A large internalized share means effective suppression exceeds the structural measure — the targets carried the arrangement''s pressure beyond the reach of any reform of the machinery itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_share, empirical, 'Structural versus internalized components of suppression on the victim seats.').

omega_variable(
    requerimiento_assent_record,
    'Did any documented performance of the Requerimiento ever produce cognizable assent from its audience?',
    'Archival survey of notarized requerimiento performances and contemporaneous testimony (including Las Casas'' report of not knowing whether to laugh or weep at the document); count performances with translated delivery, pause for response, or recorded reply.',
    'Uniform nullity fixes the consent ritual as pure theater, corroborating the rising theater_ratio trajectory and confirming that the arrangement''s claimed procedural fairness contributed zero coordination content.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(requerimiento_assent_record, empirical, 'Empirical status of the consent fiction at the arrangement''s core.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1493, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tdk_spanish_legit_tr_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1493, 0.14).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1493, observed).
narrative_ontology:measurement(tdk_spanish_legit_tr_t1512, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1512, 0.24).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1512, observed).
narrative_ontology:measurement(tdk_spanish_legit_tr_t1524, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1524, 0.4).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1524, observed).
narrative_ontology:measurement(tdk_spanish_legit_tr_t1537, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1537, 0.46).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1537, observed).
narrative_ontology:measurement(tdk_spanish_legit_tr_t1549, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1549, 0.53).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1549, observed).
narrative_ontology:measurement(tdk_spanish_legit_tr_t1561, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1561, 0.56).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1561, observed).
narrative_ontology:measurement(tdk_spanish_legit_tr_t1573, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1573, 0.57).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1573, observed).
narrative_ontology:measurement(tdk_spanish_legit_tr_t1585, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1585, 0.59).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1585, observed).
narrative_ontology:measurement(tdk_spanish_legit_tr_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1600, 0.6).
narrative_ontology:measurement_basis(tdk_spanish_legit_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(tdk_spanish_legit_be_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1493, 0.34).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1493, observed).
narrative_ontology:measurement(tdk_spanish_legit_be_t1512, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1512, 0.6).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1512, observed).
narrative_ontology:measurement(tdk_spanish_legit_be_t1524, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1524, 0.76).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1524, observed).
narrative_ontology:measurement(tdk_spanish_legit_be_t1537, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1537, 0.83).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1537, observed).
narrative_ontology:measurement(tdk_spanish_legit_be_t1549, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1549, 0.86).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1549, observed).
narrative_ontology:measurement(tdk_spanish_legit_be_t1561, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1561, 0.87).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1561, observed).
narrative_ontology:measurement(tdk_spanish_legit_be_t1573, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1573, 0.88).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1573, observed).
narrative_ontology:measurement(tdk_spanish_legit_be_t1585, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1585, 0.89).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1585, observed).
narrative_ontology:measurement(tdk_spanish_legit_be_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1600, 0.9).
narrative_ontology:measurement_basis(tdk_spanish_legit_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(tdk_spanish_legit_su_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1493, 0.5).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1493, observed).
narrative_ontology:measurement(tdk_spanish_legit_su_t1512, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1512, 0.64).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1512, observed).
narrative_ontology:measurement(tdk_spanish_legit_su_t1524, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1524, 0.74).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1524, observed).
narrative_ontology:measurement(tdk_spanish_legit_su_t1537, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1537, 0.79).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1537, observed).
narrative_ontology:measurement(tdk_spanish_legit_su_t1549, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1549, 0.81).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1549, observed).
narrative_ontology:measurement(tdk_spanish_legit_su_t1561, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1561, 0.83).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1561, observed).
narrative_ontology:measurement(tdk_spanish_legit_su_t1573, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1573, 0.84).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1573, observed).
narrative_ontology:measurement(tdk_spanish_legit_su_t1585, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1585, 0.85).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1585, observed).
narrative_ontology:measurement(tdk_spanish_legit_su_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1600, 0.86).
narrative_ontology:measurement_basis(tdk_spanish_legit_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resource_allocation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_exploration_legitimation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Treaty of Tordesillas / papal demarcation' decomposes under the epsilon-invariance principle into two structurally distinct constraints. This story (spanish_conquest_legitimation) authors the WESTERN grant as conquest license: victims are indigenous populations, the beneficiary is the Spanish colonial apparatus, epsilon is high (~0.9), and the claimed type is snare. The sibling (portuguese_exploration_legitimation) authors the EASTERN instrument as confirmation of exploration rights and exclusion of European rivals: its parties are the two crowns and its extraction, if any, runs between European states — a candidate rope or tangled_rope with far lower epsilon. The upstream/downstream edge runs from the shared instrument complex into each reading; contamination propagates between them because defenders of one reading routinely cite the other's coordination bona fides (the treaty prevented inter-crown war) to shield the conquest license from extraction scrutiny. Each file links the other via network.affects_constraints and documents the decomposition in this note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
