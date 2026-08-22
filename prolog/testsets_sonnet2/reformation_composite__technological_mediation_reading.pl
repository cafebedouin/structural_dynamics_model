% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Print-Mediated Diffusion of Reformation Dissent (Technological Mediation Reading)
 *   domain: Historical Epistemology / Religious History / Political Economy
 *
 * SUMMARY:
 *   This story instantiates the technological-mediation reading of the
 *   Reformation kernel: the printing press as the primary causal mechanism
 *   converting scattered local theological dissent (of a kind that had
 *   recurred for centuries — Hussites, Lollards, Waldensians — and been
 *   contained) into an unstoppable, continental, cross-territorial movement.
 *   The observable primitives here are publication rates, print-run sizes,
 *   and literacy trajectories, not doctrinal content
 *   (theological_fragmentation_reading) or state sovereignty assertions
 *   (political_realignment_reading) — those are separate constraints in the
 *   same kernel family, sharing this story's historical setting but authoring
 *   different ε, different beneficiaries, and different failure modes. The
 *   physical fact of movable-type reproduction functions here as a
 *   mountain-like enabling substrate (uncontestable, mechanically fixed
 *   cost-per-copy collapse) layered underneath a tangled-rope social
 *   arrangement: the reproduction technology coordinates genuine information
 *   diffusion while simultaneously generating asymmetric extraction — guild
 *   printers and literate elites capture durable advantage while unlicensed
 *   printers, illiterate populations, and suppressed Catholic presses bear
 *   disproportionate costs.
 *
 * KEY AGENTS:
 *   - printer_publisher_guilds: primary beneficiary (organized/arbitrage) — profits from reproduction economics and can relocate to escape local bans
 *   - reformist_clergy_networks: primary beneficiary (organized/mobile) — local dissent converted into continental doctrinal reach via print multiplication
 *   - unlicensed_pamphleteers: primary target (powerless/trapped) — bear sharpest enforcement costs with none of guild printers' mobility
 *   - illiterate_rural_laity: diffuse victim (powerless/trapped) — absorb downstream confessional violence without access to the driving texts
 *   - territorial_censorship_authorities: agenda-setter/enforcer (institutional/constrained) — structurally outmatched by decentralized print reproduction
 *   - book_historians: analytical observer (analytical) — reconstruct causal weight of print technology against competing kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.42).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.55).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Print-Mediated Diffusion of Reformation Dissent (Technological Mediation Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "Historical Epistemology / Religious History / Political Economy").

domain_priors:requires_active_enforcement(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '996c5cd8-b816-465b-a5b5-4d48aa27e531').
narrative_ontology:cs_kernel_codification('996c5cd8-b816-465b-a5b5-4d48aa27e531', distributed).
narrative_ontology:cs_authority_grounding('996c5cd8-b816-465b-a5b5-4d48aa27e531', distributed).
narrative_ontology:cs_reading_relation('996c5cd8-b816-465b-a5b5-4d48aa27e531', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('996c5cd8-b816-465b-a5b5-4d48aa27e531', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('996c5cd8-b816-465b-a5b5-4d48aa27e531', foundational, reproduction_technology_is_primary_causal_substrate).
narrative_ontology:cs_axiom_status(reproduction_technology_is_primary_causal_substrate, holdable).
narrative_ontology:cs_axiom_grounding('996c5cd8-b816-465b-a5b5-4d48aa27e531', reproduction_technology_is_primary_causal_substrate, empirically_contingent).
narrative_ontology:cs_axiom('996c5cd8-b816-465b-a5b5-4d48aa27e531', secondary, doctrinal_content_is_causally_secondary_to_distribution_mechanism).
narrative_ontology:cs_axiom_status(doctrinal_content_is_causally_secondary_to_distribution_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('996c5cd8-b816-465b-a5b5-4d48aa27e531', doctrinal_content_is_causally_secondary_to_distribution_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('996c5cd8-b816-465b-a5b5-4d48aa27e531', manuscript_scarcity_equilibrium).
narrative_ontology:cs_drift_state('996c5cd8-b816-465b-a5b5-4d48aa27e531', post_gutenberg_diffusion, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('996c5cd8-b816-465b-a5b5-4d48aa27e531', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, printer_publisher_guilds).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, vernacular_reading_public).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, reformist_clergy_networks).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, unlicensed_pamphleteers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, illiterate_rural_laity).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, suppressed_catholic_print_shops).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, vernacular_reading_public).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, print_capitalism_thesis).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, mass_literacy_acceleration_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own presses and type; can retool a shop to print Luther's tracts within weeks of a manuscript arriving, and profit from the scale economics of set type reused across thousands of copies. They select what gets printed based on marketability and can relocate operations across jurisdictions (Wittenberg, Basel, Strasbourg, Antwerp) to escape a single prince's or bishop's ban. The technology's reproducibility is the source of both their profit and the movement's velocity.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printer_publisher_guilds, beneficiary,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, printer_publisher_guilds, agenda_setter).

% Theologians and preachers whose local dissent becomes a continental argument because print multiplies a single sermon or disputation into thousands of identical copies distributed along trade routes. Their doctrinal reach now vastly exceeds their personal itinerancy; the press converts what would have been a suppressible local heresy into a distributed, unkillable text.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, reformist_clergy_networks, beneficiary,
    organized, generational, mobile, continental).

% Literate townspeople and burghers gain direct access to vernacular Bibles and polemical tracts without clerical mediation, for the first time able to read and judge theological claims themselves. They pay in the currency of exposure to legal risk (owning banned literature is prosecutable in Catholic territories) and in economic terms (pamphlets cost money, competing for scarce household resources).
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, vernacular_reading_public, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, vernacular_reading_public, payer).

% Small printers and authors operating without guild licenses or princely protection produce the most volatile, fastest-moving material — often the texts that most directly provoke authorities. They bear the sharpest end of the enforcement apparatus: seizure of presses, imprisonment, execution for printing unlicensed or heretical material, with none of the mobility or capital reserves that established guild printers use to relocate.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, unlicensed_pamphleteers, payer,
    powerless, immediate, trapped, local).

% The majority of the European population cannot read the pamphlets, tracts, and Bibles driving the controversy, yet becomes swept into the resulting wars, confessional violence, princely religious mandates, and social upheaval that print-accelerated schism produces. They absorb the costs of a conflict whose textual engine is inaccessible to them, mediated to them only through sermon and rumor.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_rural_laity, payer,
    powerless, biographical, trapped, local).

% Catholic printers operating in contested or Protestant-dominant territories find their presses seized, their counter-reformation tracts banned or burned, and their commercial viability undercut by the faster, more numerous reformist output. The same technology that empowers their rivals is turned against them through licensing regimes and confiscation once political authority shifts.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, suppressed_catholic_print_shops, payer,
    moderate, biographical, constrained, regional).

% Bishops, papal legates, and princely licensing offices attempt to control the press through indices of banned books, printing licenses, and capital punishment for unlicensed printing. Their enforcement capacity is structurally outmatched by the press's decentralized reproducibility — a text banned in Mainz is printed in Basel within months — which drives escalating and increasingly desperate suppression measures across the interval.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, territorial_censorship_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Later scholars (Eisenstein, Febvre, Pettegree) reconstruct publication rates, print-run sizes, and literacy trajectories to assess whether the printing press was a necessary and sufficient causal engine of the Reformation's continental scale, or one enabling factor among several structurally independent causes.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, book_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Movable-type printing solves a genuine reproduction-and-distribution problem: a single theological argument, once set in type, can be copied identically and cheaply at a scale no scriptorium could match, allowing dissent that would have remained local and suppressible to circulate faster than any single authority could contain it.
% TRANSFER_FUNCTION: The arrangement moves theological content, political attention, and economic risk: from clerical monopolists of scriptural interpretation to printers and readers; from centralized ecclesiastical authority to distributed vernacular literacy; and from the literate beneficiaries of the new information regime onto the illiterate and unlicensed who bear the resulting religious violence and enforcement costs without commensurate access to the texts driving them.
% ABSENT_VOICES: Illiterate rural populations, who constitute the overwhelming majority of Europe's population in this period, have no direct voice in the print-mediated theological argument yet bear enormous downstream costs (confessional wars, forced conversions, social disruption) — their absence from the textual record is near-total, reconstructable only through indirect sources like court records and sermon transcripts.
% DISAPPEARANCE_RATIONALE: Without movable-type printing, theological dissent of the kind Luther, Zwingli, and others articulated would very plausibly have remained containable as prior heresies (Hussite, Lollard, Waldensian movements) were contained — through localized suppression, execution of leaders, and absence of a durable, reproducible textual record. The scale, speed, and cross-territorial coordination of the Reformation as a continental phenomenon depends structurally on the reproduction technology; remove it and the movement's geography and pace both collapse.
% FOUNDING_PROBLEM: Movable-type printing was developed to solve a manuscript-reproduction bottleneck (Gutenberg's original commercial motivation was largely liturgical and commercial texts, indulgences, and administrative documents) — it was not built to enable theological insurgency, but its reproduction economics made insurgent circulation a low-marginal-cost byproduct once dissenting content entered the system.
% FOUNDING_PROBLEM_CORROBORATION: Book historians (Eisenstein's print-culture thesis, later qualified by Pettegree and Febvre/Martin) attest from outside any confessional beneficiary group that the press's original commercial function (liturgical and administrative reproduction) was rapidly overtaken and structurally repurposed by reformist and later counter-reformist content; this reading is independently corroborated by surviving print-shop ledgers and incunabula catalogs, not solely by partisan religious sources on either side.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).
:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 (rising from 0.15 at T0) because the core function — mechanical reproduction of text — is genuinely coordinative (a real bottleneck-solving technology), but the social arrangement built atop it increasingly channels durable advantage to printers, reformist elites, and literate urban populations while imposing enforcement costs and downstream violence onto the powerless. Suppression rises sharply (0.2 to 0.55) as territorial authorities escalate licensing regimes, indices of banned books, and capital punishment for unlicensed printing in a losing race against decentralized reproduction capacity — this is the enforcement machinery required for the tangled-rope classification. Theater ratio stays low throughout (0.05 to 0.18) because the suppression apparatus, while escalating, remains functionally aimed at genuine content control rather than performative compliance theater — book-burnings and press seizures had real, not merely symbolic, effect on circulation in this period.
 *
 * PERSPECTIVAL GAP:
 *   From the guild printer's seat, the press is a rope: a coordination technology solving a genuine reproduction bottleneck that they profit from without coercing anyone. From the unlicensed pamphleteer's seat facing seizure and execution, or the illiterate peasant swept into confessional war, the same technological substrate operates as an engine of asymmetric risk distribution requiring the state's active, escalating enforcement machinery to contain — a tangled rope, not a pure rope. The engine's per-seat computation should surface this divergence directly from the differing power/exit declarations rather than from any authored narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Printer-publisher guilds and reformist clergy sit at the beneficiary end: they capture the reproduction technology's cost collapse directly, and their exit options (relocating presses, itinerant preaching networks) let them escape localized suppression. The vernacular reading public benefits from information access but also pays in legal exposure — a genuinely mixed position captured by the dual role. Unlicensed pamphleteers and illiterate rural laity sit at the target end: trapped, powerless, absorbing enforcement violence and confessional war costs respectively, with no capacity to arbitrage across jurisdictions the way guild printers can. Suppressed Catholic print shops occupy an intermediate position — they possess capital and printing capacity (moderate power) but face constrained exit once political control in their territory shifts against them.
 *
 * MANDATROPHY ANALYSIS:
 *   The printing press's original coordination function — reproducing liturgical and administrative texts cheaply — is largely obsolete as a description of what the technology is doing by the Reformation's peak (T24-T40): it has been substantially repurposed into a theological-political weapon whose enforcement apparatus (indices, licensing, capital punishment) persists and escalates because authorities cannot let go of content control even as the technology outpaces their capacity to exercise it. This is a founding_problem_status of 'dead' — the manuscript-bottleneck problem the press solved is gone — while the disappearance_verdict remains world_rearranges, flagging the classic mismatch: an arrangement whose original justification is dead but whose downstream effects (confessional realignment, print capitalism, vernacular literacy) are so structurally entrenched that removing it would still rearrange the world. This is not itself evidence of a zombie/capture pattern in the pejorative sense — it is evidence that the technology's function shifted rather than atrophied, which the classification correctly registers as tangled_rope rather than piton (no pure inertial theater; the coordination function is live, just extractive).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_technological_primacy,
    'Is the printing press the primary causal driver of the Reformation''s continental scale (this reading), or one enabling factor whose causal weight is properly shared with theological doctrinal incompatibility (theological_fragmentation_reading) and emergent state sovereignty assertion (political_realignment_reading)?',
    'Comparative historical analysis of prior heresy movements (Hussite, Lollard, Waldensian) that lacked print technology but shared similar doctrinal content, cross-referenced against regions with comparable print penetration but different political/theological conditions, to isolate the technology''s independent causal contribution.',
    'If print technology is shown to be necessary but not sufficient — requiring the co-occurrence of doctrinal rupture and princely political incentive — this reading''s claim to primacy weakens and the kernel''s three readings become more genuinely coexisting rather than competing for explanatory priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_technological_primacy, conceptual, 'Whether the technological-mediation reading''s causal-primacy claim survives comparison with the sibling readings'' rival mechanisms.').

omega_variable(
    print_as_mountain_or_construct,
    'Is the press''s cost-collapse effect on reproduction a mountain-like physical constraint (fixed by the mechanics of movable type, unchangeable by any party), or does its Reformation-era impact depend on constructed, contestable conditions (vernacular literacy rates, trade route density, urbanization) that were themselves politically shaped?',
    'Compare print penetration and reproduction cost curves across regions with similar press availability but different literacy/urbanization profiles to see whether the ''mountain'' (mechanical reproduction) or the ''construct'' (social conditions for reception) does more explanatory work.',
    'If the physical reproduction technology is the mountain and literacy/urbanization are separately-authored variables, the FSM concern (beneficiaries declared under a mountain-adjacent claim) is addressed by keeping this story''s claimed_type at tangled_rope (not mountain) — the press enables but the social arrangement built on it is what extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(print_as_mountain_or_construct, conceptual, 'Whether the printing press functions as an unchangeable physical substrate or as one construct among several shaping the Reformation''s course.').

omega_variable(
    literacy_rate_reconstruction_uncertainty,
    'How reliable are early-modern European literacy rate estimates, given that most surviving evidence comes from urban, literate-adjacent records (guild registers, court documents, clerical correspondence)?',
    'Cross-reference surviving print-run figures, book ownership inventories, and signature-literacy studies (ability to sign one''s name in legal documents) across multiple regions and social strata to triangulate literacy estimates independent of the print industry''s own self-reporting.',
    'If literacy rates are substantially overestimated by urban-biased sources, the scale of the ''vernacular reading public'' beneficiary group shrinks and the diffuse-victim status of the illiterate majority becomes even more pronounced, strengthening the tangled_rope classification''s asymmetric-extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_rate_reconstruction_uncertainty, empirical, 'Uncertainty in reconstructing historical literacy rates from surviving, urban-biased documentary evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__technological_mediation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(refo_tr_t8, reformation_composite__technological_mediation_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement(refo_tr_t16, reformation_composite__technological_mediation_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(refo_tr_t24, reformation_composite__technological_mediation_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(refo_tr_t32, reformation_composite__technological_mediation_reading, theater_ratio, 32, 0.17).
narrative_ontology:measurement(refo_tr_t40, reformation_composite__technological_mediation_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__technological_mediation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(refo_be_t8, reformation_composite__technological_mediation_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(refo_be_t16, reformation_composite__technological_mediation_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(refo_be_t24, reformation_composite__technological_mediation_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(refo_be_t32, reformation_composite__technological_mediation_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(refo_be_t40, reformation_composite__technological_mediation_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__technological_mediation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(refo_su_t8, reformation_composite__technological_mediation_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(refo_su_t16, reformation_composite__technological_mediation_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(refo_su_t24, reformation_composite__technological_mediation_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(refo_su_t32, reformation_composite__technological_mediation_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(refo_su_t40, reformation_composite__technological_mediation_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_composite__technological_mediation_reading, 0.05).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, political_realignment_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the reformation_composite kernel. theological_fragmentation_reading takes competing soteriological/ecclesiological commitments as the primary causal engine (different ε, different beneficiary/victim structure centered on denominational elites and heretics). political_realignment_reading takes emerging nation-state sovereignty assertions against papal/imperial authority as primary (different ε, centered on princes and papal authority as beneficiary/victim). This story isolates the printing press's reproduction economics as the primary observable — publication rates, print-run sizes, literacy trajectories — and deliberately does not adjudicate whether technology, theology, or politics is 'truly' primary; that adjudication is exactly what the omega variable reformation_kernel_reading_technological_primacy leaves open. All three stories should be read together as a decomposed family, per the ε-invariance principle, rather than as competing single-file accounts of 'the Reformation.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
