% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Deployment of Print by the Reformer-Printer Alliance (1517-1555)
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   Between the Ninety-Five Theses (1517) and the Peace of Augsburg (1555),
 *   reformers and printers formed a strategic alliance that turned print from
 *   a trade into a weapon: Luther and his colleagues supplied content timed
 *   to imperial politics; printers supplied replication speed, the cheap
 *   pamphlet format, and distribution networks that outran every ban. The
 *   standing arrangement under contest — the referent for every metric in
 *   this file — is that deployment as it actually operated, assessed by this
 *   reading's own lights: deliberate weaponization of the medium for
 *   religious and economic gain. This story is ONE reading of the kernel
 *   press_reformation_causality, instantiating strategic_deployment;
 *   technological_determinism and co_constitution are separate constraints
 *   with their own epsilon values and beneficiary structures, linked through
 *   network.affects_constraints. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope — this reading's rope face
 *   (genuine coordination of dispersed dissent) and snare face (weaponized
 *   taking against Church authority) resolve into one structure — while the
 *   authored metrics describe its actual operation independently of that
 *   claim.
 *
 * KEY AGENTS:
 *   - reformation_leaders: strategic deployer and agenda-setter (organized/identity_locked) — supply content and timing, collect the movement, exit closed at excommunication
 *   - printshop_operators: alliance operators and primary economic beneficiaries (organized/mobile) — run presses and networks, collect the controversy revenue, hedge across confessions
 *   - vernacular_reading_public: mass beneficiary (moderate/constrained) — gain scriptural access and confessional identity, fund the deployment, carry possession risk
 *   - catholic_church_hierarchy: primary target (institutional/trapped) — loses doctrinal monopoly, revenue, and every enforcement race; cannot exit its own authority
 *   - monastic_scriptoria: secondary payer (moderate/constrained) — lose the manuscript economy the presses undercut
 *   - german_peasant_leagues: excluded voice (organized/trapped) — used the medium in 1525, were abandoned and crushed
 *   - reformation_historiographers: analytical observer (analytical/analytical) — see the full structure and adjudicate between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.65).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.6).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.65).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Deployment of Print by the Reformer-Printer Alliance (1517-1555)").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history of technology / religious history / media studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'd072fb69-2197-4f61-9e28-28c4e79f08e8').
narrative_ontology:cs_kernel_codification('d072fb69-2197-4f61-9e28-28c4e79f08e8', distributed).
narrative_ontology:cs_authority_grounding('d072fb69-2197-4f61-9e28-28c4e79f08e8', distributed).
narrative_ontology:cs_reading_relation('d072fb69-2197-4f61-9e28-28c4e79f08e8', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('d072fb69-2197-4f61-9e28-28c4e79f08e8', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('d072fb69-2197-4f61-9e28-28c4e79f08e8', foundational, print_deployment_was_deliberate_instrumentalization).
narrative_ontology:cs_axiom_status(print_deployment_was_deliberate_instrumentalization, holdable).
narrative_ontology:cs_axiom_grounding('d072fb69-2197-4f61-9e28-28c4e79f08e8', print_deployment_was_deliberate_instrumentalization, empirically_contingent).
narrative_ontology:cs_axiom('d072fb69-2197-4f61-9e28-28c4e79f08e8', secondary, economic_incentives_shaped_religious_dissemination).
narrative_ontology:cs_axiom_status(economic_incentives_shaped_religious_dissemination, holdable).
narrative_ontology:cs_axiom_grounding('d072fb69-2197-4f61-9e28-28c4e79f08e8', economic_incentives_shaped_religious_dissemination, empirically_contingent).
narrative_ontology:cs_reference_frame('d072fb69-2197-4f61-9e28-28c4e79f08e8', strategic_instrumental_agency).
narrative_ontology:cs_drift_state('d072fb69-2197-4f61-9e28-28c4e79f08e8', contemporary_book_history_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d072fb69-2197-4f61-9e28-28c4e79f08e8', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformation_leaders).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printshop_operators).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, vernacular_reading_public).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, monastic_scriptoria).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, strategic_agency_historiography).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Supply the deployment's content and timing: Luther's pamphlets and sermons, the September Testament translation, polemics synchronized to imperial diets and public debates. They collect the movement the print campaign builds — a dispersed, synchronized following that no clerical hierarchy of their own administers. Their exit closed at excommunication: after the Worms ban, returning to the Church means repudiating the public identity their own texts created, and their physical safety depends on the territories and patrons the campaign won over.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformation_leaders, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, reformation_leaders, beneficiary).

% Operate the presses, format innovations, and distribution networks that make the deployment physically possible: they choose editions and print runs, pioneer the cheap quarto pamphlet, and move texts between imperial cities faster than bans can travel. They collect the controversy economy's revenue — Luther's titles alone account for a large share of German-language output at the peak — and many hedge by printing for both confessions or relocating when a city's politics turn; the risk is prosecution under imperial bans, the reward is the fastest-growing market in Europe.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printshop_operators, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, printshop_operators, agenda_setter).

% Buy, read, and recirculate the pamphlets, hymn sheets, and vernacular New Testaments; gain direct scriptural access and a shared confessional identity without clerical mediation. They fund the deployment with every purchase and carry possession risk where owning evangelical books is banned; their aggregate demand decides which polemics get reprinted and which titles die.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, vernacular_reading_public, beneficiary,
    moderate, biographical, constrained, regional).

% Bears the deployment's costs: doctrinal gatekeeping erodes as vernacular scripture and polemic circulate beyond any ban's reach, indulgence and dispensation revenue falls in contested territories, and every countermeasure — the Worms edict, book burnings, early prohibited lists — fails to recall texts already in thousands of copies. It cannot exit its own position: the authority under attack is the institution itself, and suspending enforcement concedes the doctrine.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_church_hierarchy, payer,
    institutional, generational, trapped, continental).

% Lose the manuscript economy the presses undercut: copying houses that reproduced liturgical and devotional works for centuries cannot match press pricing or volume, so houses close their scriptoria, disperse libraries, or redirect labor to other work. Their displacement is the economic underside of the same arrangement that strips the hierarchy's authority.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, monastic_scriptoria, payer,
    moderate, generational, constrained, continental).

% Print the Twelve Articles in 1525 — the first mass vernacular use of the medium by commoners — and find the weapon is not theirs: reformer pamphleteers denounce the rising, princes crush it with evangelical backing, and the same network that amplified peasant grievances amplifies their condemnation. They would object that the deployment serves gospel and profit rather than the commons; they hold no seat in it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, german_peasant_leagues, excluded,
    organized, immediate, trapped, regional).

% Reconstruct the deployment from printer contracts, edition data, and correspondence; they see the whole structure — strategy, profit, suppression, exclusion — and adjudicate between rival accounts of why print mattered. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformation_historiographers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, printshop_operators).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of dispersed dissent: scattered critics of a continental hierarchy needed to synchronize a message across jurisdictions, replicate it faster than authorities could confiscate it, and reach literate laity in vernacular languages without any hierarchical apparatus of their own. Cheap standardized print answers all three at once, and the same presses gave the alliance a revenue engine that made the operation self-funding.
% TRANSFER_FUNCTION: Moves interpretive authority from the Church hierarchy to vernacular readers and reformist clergy; moves money from pamphlet-buying readers to printers and authors; and moves the Church's information-monopoly position — the right to say what scripture means — to the reformer-printer alliance.
% ABSENT_VOICES: German peasant leagues, whose Twelve Articles were the deployment's first popular vernacular use before reformers sided with the princes who crushed them; the illiterate rural majority, whose oral catechesis had no standing in a war fought in text; and loyal Catholic printers, who lost the controversy market they declined to enter and whose counter-print never matched evangelical distribution.
% DISAPPEARANCE_RATIONALE: Remove the strategic deployment — reformers decline print, printers decline the controversy market — and the Reformation survives as university critique and imperial politics, but not as a mass movement: no synchronized pamphlet waves, no vernacular scripture in tens of thousands of copies, no evangelical reading public, no self-funding dissent machine. The religious map of Europe, the print economy built on controversy, and the very existence of a lay reading public reorganize around slower channels. How much of this rearrangement the deployment caused rather than merely carried is exactly what the sibling readings dispute — that contest lives in the kernel's other constraints, not in this verdict.
% FOUNDING_PROBLEM: How to move religious dissent across fragmented jurisdictions faster than imperial and ecclesiastical suppression can respond, and how to make a press pay while doing it. The alliance formed because Luther needed replication speed he could not build alone and printers needed a product with effectively unlimited demand.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the alliance's enemies: Johannes Cochlaeus's complaint that Luther's works circulate in every shop and can no longer be suppressed, and the imperial legal record itself — the Edict of Worms (1521) failed to stop the presses, the 1548 Augsburg Interim failed to hold, and the Peace of Augsburg (1555) codifies the outcome the deployment produced. No testimony from inside the alliance is needed to establish that the suppression-breaking problem was solved.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.65 because the deployment's taking is real but competitive in form: it strips the Church of monopoly authority and revenue and transfers both to the alliance, rather than taxing a governed population. Suppression (0.60) is the arrangement's enforcement face: it survives only by outrunning censorship — smuggling, clandestine imprints, false title pages — and by restricting Catholic counter-print where the alliance holds power; it is authored as the raw structural property and is not scaled by scope or directionality, since only extractiveness is scaled in the engine's computation. Theater stays low (0.28) because the pamphlets did genuine coordination work; the performative share grows only as confessional identity display replaces persuasion in a movement that has already won its territories. Resistance is high (0.78): a weaponized arrangement meets the full counterforce of what it attacks — Worms, burnings, prohibited lists, the Interim. The three tracked series share one eight-point grid from 1517 to 1555; the 1548 Interim bump in suppression and dip in extraction is the Counter-Reformation's partial recovery, reversed and institutionalized by the 1555 settlement, which is where the base_properties values sit.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently by construction. From reformation_leaders the arrangement is providence operationalized — the medium supplied for the gospel's recovery. From catholic_church_hierarchy the identical structure is an existential attack that no countermeasure catches. printshop_operators see a market: the same controversy is demand. vernacular_reading_public experience liberation with a price — scripture without mediation, bought with money and risk. From the trapped target seat the structure should compute as enforced taking; from the mobile operator seat, closer to a coordination tool it profited from. The engine computes these divergences from the structural data; this reading's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: reformation_leaders collect the movement and are identity-locked to what they built; printshop_operators collect the revenue and hold mobile exit through hedging and relocation; vernacular_reading_public collect access and identity against funding and risk burdens. Targets: catholic_church_hierarchy sits near the full-target end — institutional power, but trapped, since the authority under extraction is the institution itself; monastic_scriptoria bear economic displacement with constrained exit. The Church's institutional power does not dampen its directionality: power without exit deepens the target position. gain_flow names printshop_operators because the measurable extraction — pamphlet revenue — demonstrably landed there; the authority transfer accrued to reformation_leaders and the movement collectively, a split capture the single-seat receipt field can only half-record.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both faces visible and blocks two opposite mislabels. Calling the arrangement pure extraction erases the genuine coordination function — dispersed dissent really did need cheap synchronized replication, and the reading public really did gain access no manuscript economy could have given it; extraction alone cannot explain the arrangement's form. Calling it pure coordination erases the weaponization — the alliance deliberately targeted the Church's authority and revenue, and the enforcement record (smuggling networks, suppression of Catholic print in evangelical cities) is the fingerprint of active taking. On mandatrophy: the founding problem was solved by the arrangement's own success — dead by the Augsburg settlement — but the arrangement did not decay into performance; it retooled into the commercial print economy, which is why theater_ratio stays low and the structure holds as a live tangled_rope rather than sliding toward inertia. The founding_problem_status x disappearance_verdict mismatch will flag; the cross-check against the low computed theater path should read that flag as retooled function, not zombie performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This story instantiates the strategic_deployment reading of kernel press_reformation_causality. Where in the causal structure do the three readings actually disagree — the locus of agency (strategist, affordance, or feedback loop), or only the weight each factor carries?',
    'Comparative historiographical analysis keyed to shared evidence: printer contracts and account books, publication timing relative to imperial political events, and Luther''s correspondence about print strategy — the same corpus each reading must explain.',
    'If the co_constitution sibling is adopted, this constraint''s beneficiary/victim structure blurs — printers were shaped by the movement as much as shaping it, and the agenda_setter seat splits into loop participants; if the technological_determinism sibling is adopted, the arrangement re-reads as affordance realization rather than deployment, extraction against the Church becomes a side effect of diffusion, and effective extraction falls.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of the inter-reading disagreement within the kernel; records that this file is one of three readings.').

omega_variable(
    church_extraction_vs_displacement,
    'Did the deployment take from the Catholic Church, or outcompete it — was the loss of authority and revenue a transfer to the alliance, or the ordinary defeat of a rival in open religious competition?',
    'Trace the specific transfers: indulgence revenue against pamphlet revenue in overlapping markets; interpretive rights moving from clergy to lay readers; enforcement expenditure by the hierarchy against alliance profit. Transfer shows as captured flows to named seats; displacement shows as diffuse competitive loss with no capturer.',
    'If displacement, extractiveness falls toward the coordination-tool reading and the Church is a defeated competitor rather than a target; if transfer, the tangled_rope structure holds and the weaponization face strengthens. This is the epsilon-critical ambiguity for this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_extraction_vs_displacement, conceptual, 'Whether the arrangement''s cost to the Church is captured transfer or competitive displacement.').

omega_variable(
    printer_alliance_depth,
    'Were printers genuine strategic allies in the weaponization, or opportunistic vendors recruited by a demand shock they did not author?',
    'Printer business records and edition economics: whether printers accepted below-market risk for movement titles and timed editions to religious-political events, or simply printed whatever sold fastest regardless of confessional alignment.',
    'If vendors, the strategic-deployment claim narrows to the reformer seat alone — printshop_operators drop from co-administering the arrangement to passive collection, the alliance framing weakens, and part of the coordination function was market response rather than strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_alliance_depth, empirical, 'Depth of the printer side of the strategic alliance.').

omega_variable(
    reading_public_net_position,
    'Is the vernacular reading public a net beneficiary or a double-burdened seat — did access gains outweigh funding the controversy economy and carrying possession risk?',
    'Distributional analysis: reader expenditure and prosecution records set against measurable access gains (vernacular Bibles per household, literacy effects), broken out by territory and confession.',
    'If net-burdened, readers belong on the cost-bearing ledger, the beneficiary structure thins toward the elite alliance, and the coordination function reads as serving the alliance rather than its audience — raising the reader seat''s effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_public_net_position, empirical, 'Net position of the reading public under the deployment.').

omega_variable(
    popular_uses_exclusion_scope,
    'Was the deployment''s coordination function general-purpose media capacity that any dissent could have seized, or captured from the start by the reformer-prince alliance — did the 1525 peasant exclusion reflect the arrangement''s design or its political moment?',
    'Compare the print network''s treatment of the Twelve Articles and peasant pamphlets against reformer titles: print runs, distribution reach, and the circulation of post-1525 condemnation literature.',
    'If captured by design, the excluded peasant seat is structural — the arrangement coordinates only its alliance, strengthening the weaponization reading; if a political moment, the function was general and the exclusion contingent, leaving the coordination function broader than the alliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(popular_uses_exclusion_scope, empirical, 'Whether popular uses were structurally excluded from the deployment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__strategic_deployment, theater_ratio, 1517, 0.1).
narrative_ontology:measurement_basis(pres_tr_t1517, observed).
narrative_ontology:measurement(pres_tr_t1521, press_reformation_causality__strategic_deployment, theater_ratio, 1521, 0.12).
narrative_ontology:measurement_basis(pres_tr_t1521, observed).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__strategic_deployment, theater_ratio, 1525, 0.16).
narrative_ontology:measurement_basis(pres_tr_t1525, observed).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causality__strategic_deployment, theater_ratio, 1530, 0.2).
narrative_ontology:measurement_basis(pres_tr_t1530, observed).
narrative_ontology:measurement(pres_tr_t1535, press_reformation_causality__strategic_deployment, theater_ratio, 1535, 0.24).
narrative_ontology:measurement_basis(pres_tr_t1535, observed).
narrative_ontology:measurement(pres_tr_t1542, press_reformation_causality__strategic_deployment, theater_ratio, 1542, 0.27).
narrative_ontology:measurement_basis(pres_tr_t1542, observed).
narrative_ontology:measurement(pres_tr_t1548, press_reformation_causality__strategic_deployment, theater_ratio, 1548, 0.3).
narrative_ontology:measurement_basis(pres_tr_t1548, observed).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__strategic_deployment, theater_ratio, 1555, 0.28).
narrative_ontology:measurement_basis(pres_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement_basis(pres_be_t1517, observed).
narrative_ontology:measurement(pres_be_t1521, press_reformation_causality__strategic_deployment, base_extractiveness, 1521, 0.52).
narrative_ontology:measurement_basis(pres_be_t1521, observed).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__strategic_deployment, base_extractiveness, 1525, 0.6).
narrative_ontology:measurement_basis(pres_be_t1525, observed).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causality__strategic_deployment, base_extractiveness, 1530, 0.66).
narrative_ontology:measurement_basis(pres_be_t1530, observed).
narrative_ontology:measurement(pres_be_t1535, press_reformation_causality__strategic_deployment, base_extractiveness, 1535, 0.68).
narrative_ontology:measurement_basis(pres_be_t1535, observed).
narrative_ontology:measurement(pres_be_t1542, press_reformation_causality__strategic_deployment, base_extractiveness, 1542, 0.7).
narrative_ontology:measurement_basis(pres_be_t1542, observed).
narrative_ontology:measurement(pres_be_t1548, press_reformation_causality__strategic_deployment, base_extractiveness, 1548, 0.68).
narrative_ontology:measurement_basis(pres_be_t1548, observed).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__strategic_deployment, base_extractiveness, 1555, 0.65).
narrative_ontology:measurement_basis(pres_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement_basis(pres_su_t1517, observed).
narrative_ontology:measurement(pres_su_t1521, press_reformation_causality__strategic_deployment, suppression_requirement, 1521, 0.55).
narrative_ontology:measurement_basis(pres_su_t1521, observed).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causality__strategic_deployment, suppression_requirement, 1525, 0.62).
narrative_ontology:measurement_basis(pres_su_t1525, observed).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causality__strategic_deployment, suppression_requirement, 1530, 0.7).
narrative_ontology:measurement_basis(pres_su_t1530, observed).
narrative_ontology:measurement(pres_su_t1535, press_reformation_causality__strategic_deployment, suppression_requirement, 1535, 0.68).
narrative_ontology:measurement_basis(pres_su_t1535, observed).
narrative_ontology:measurement(pres_su_t1542, press_reformation_causality__strategic_deployment, suppression_requirement, 1542, 0.66).
narrative_ontology:measurement_basis(pres_su_t1542, observed).
narrative_ontology:measurement(pres_su_t1548, press_reformation_causality__strategic_deployment, suppression_requirement, 1548, 0.72).
narrative_ontology:measurement_basis(pres_su_t1548, observed).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__strategic_deployment, suppression_requirement, 1555, 0.6).
narrative_ontology:measurement_basis(pres_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'print caused the Reformation' decomposes into three structurally distinct readings of one kernel, each with its own epsilon and beneficiary/victim structure per the epsilon-invariance principle. This file instantiates strategic_deployment (agent-led weaponization; epsilon 0.65 over the 1517-1555 deployment arrangement, reformers and printers as beneficiaries, Church hierarchy as target). technological_determinism holds diffusion as autonomous — its structure has no strategic beneficiary seat and its epsilon reflects affordance-driven inevitability. co_constitution holds mutual shaping through print-economy feedback — its structure blurs agenda-setter and beneficiary seats into loop participants. The edges here carry no upstream/downstream precedence claim: unlike a settled family such as the BGS decomposition, these three readings are rivals at the same evidentiary level, each cited against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
