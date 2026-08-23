% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Strategic Deployment of Print: Reformers and Printers as Upstream Agents
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This file instantiates the strategic_deployment reading of the
 *   press_reformation_causation kernel: the claim that reformers and printers
 *   purposively exploited print as an instrument, with human agency upstream
 *   and the artifact a neutral capacity awaiting use. The colloquial question
 *   'did the press cause the Reformation?' decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct causal
 *   claims with different referents and different epsilon values:
 *   technological_determinism locates causation in the artifact itself (its
 *   epsilon measures the press-as-agent's footprint on the communicative
 *   order); mutual_shaping distributes causation across a co-evolving
 *   technology-and-use system (its epsilon measures the coupled arrangement);
 *   THIS reading locates causation in purposive agents, so its epsilon
 *   measures the standing arrangement of strategic deployment — the
 *   reformer-printer alliances that flooded Europe with vernacular print
 *   between 1517 and 1600 — assessed by the reading's own lights as
 *   predominantly coordination carrying subordinate commercial extraction.
 *   The three stories form a constraint family linked via
 *   network.affects_constraints; each carries its own epsilon, its own
 *   beneficiary/victim structure, and its own classification, and none hedges
 *   across the others. KEY AGENTS (by structural relationship): -
 *   commercial_printers: Agenda-setter and principal pecuniary beneficiary
 *   (organized/mobile) — administer the medium, choose and finance titles,
 *   collect the margins - protestant_reformers: Primary strategic beneficiary
 *   (organized/identity_locked) — supply content and direction; recantation
 *   is existentially unavailable - evangelical_territorial_princes:
 *   Opportunist beneficiary-enabler (institutional/mobile) — legalize
 *   locally, harvest sovereignty and property - vernacular_reading_public:
 *   Net beneficiary with diffuse costs (moderate/constrained) — buys access,
 *   absorbs polemical saturation - catholic_ecclesiastical_hierarchy:
 *   Principal payer (institutional/trapped) — loses the information monopoly,
 *   funds eight decades of counter-mobilization -
 *   manuscript_scribes_and_stationers: Excluded voice (moderate/constrained)
 *   — displaced by the transition, outside the conversation -
 *   media_historians: Analytical observer (analytical/analytical) —
 *   adjudicates the causal contest from outside the confessional stakes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.28).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.58).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Deployment of Print: Reformers and Printers as Upstream Agents").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '3a7fbb2f-b64a-4487-9db1-f938f463c49a').
narrative_ontology:cs_kernel_codification('3a7fbb2f-b64a-4487-9db1-f938f463c49a', distributed).
narrative_ontology:cs_authority_grounding('3a7fbb2f-b64a-4487-9db1-f938f463c49a', expertise).
narrative_ontology:cs_interpretation_layer_present('3a7fbb2f-b64a-4487-9db1-f938f463c49a').
narrative_ontology:cs_reading_relation('3a7fbb2f-b64a-4487-9db1-f938f463c49a', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('3a7fbb2f-b64a-4487-9db1-f938f463c49a', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('3a7fbb2f-b64a-4487-9db1-f938f463c49a', foundational, purposive_agents_are_upstream_cause).
narrative_ontology:cs_axiom_status(purposive_agents_are_upstream_cause, holdable).
narrative_ontology:cs_axiom_grounding('3a7fbb2f-b64a-4487-9db1-f938f463c49a', purposive_agents_are_upstream_cause, empirically_contingent).
narrative_ontology:cs_axiom('3a7fbb2f-b64a-4487-9db1-f938f463c49a', foundational, media_are_neutral_instruments).
narrative_ontology:cs_axiom_status(media_are_neutral_instruments, holdable).
narrative_ontology:cs_axiom_grounding('3a7fbb2f-b64a-4487-9db1-f938f463c49a', media_are_neutral_instruments, empirically_contingent).
narrative_ontology:cs_reference_frame('3a7fbb2f-b64a-4487-9db1-f938f463c49a', agency_first_causal_order).
narrative_ontology:cs_drift_state('3a7fbb2f-b64a-4487-9db1-f938f463c49a', contemporary_post_eisenstein_debate, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('3a7fbb2f-b64a-4487-9db1-f938f463c49a', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, commercial_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, evangelical_territorial_princes).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, vernacular_reading_public).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_ecclesiastical_hierarchy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, vernacular_reading_public).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, agency_first_historiography).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, instrumental_neutrality_of_artifacts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians, preachers, and translators who supplied the content and much of the strategy: treatises timed to controversy cycles, vernacular scripture, hymns, catechisms, and woodcut satire, fed to printer-allies who multiplied each piece by the thousands. They gained reach and movement cohesion no pulpit network could match. Exit meant recantation or the stake, so commitment deepened rather than reversed; the network of correspondents and patrons they built became the movement's nervous system.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, beneficiary,
    organized, generational, identity_locked, continental).

% Printer-publishers who owned the presses, chose the titles, financed the editions, and ran distribution through the Frankfurt and Leipzig fairs and networks of colporteurs. They bet capital on reformist bestsellers, pocketed the margins, pivoted portfolios when censorship or demand shifted, and administered the medium the reformers rode. Their exit was real: a house that tired of heresy could print almanacs tomorrow or move cities.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, commercial_printers, agenda_setter,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, commercial_printers, beneficiary).

% Rulers such as the Saxon electors, the Hessian landgraves, and the Scandinavian crowns who sponsored reformers, tolerated or licensed the trade inside their territories, and harvested the proceeds: confiscated church property, doctrinal sovereignty, and administrative reach through printed church ordinances and visitation articles. Their sponsorship was revocable policy, not binding identity; they could suppress the trade locally whenever it stopped paying.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, evangelical_territorial_princes, beneficiary,
    institutional, generational, mobile, regional).

% Urban artisans, parish clergy, students, and magistrates who bought cheap quartos and broadsheets. They gained scripture, news, and a voice in pamphlet wars conducted in their own tongues. They paid per-item prices and absorbed a saturated, one-sided polemical environment; manuscript and pulpit alternatives were slower and costlier, so their practical choice narrowed to which print to read, not whether.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, vernacular_reading_public, beneficiary,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, vernacular_reading_public, payer).

% Rome, the bishops, and the university faculties that lost the doctrinal information monopoly the deployment dismantled. They bore the costs of counter-mobilization: ban lists, the Index machinery, inquisitorial review, licensed reprinting schemes, and counter-propaganda budgets, decade after decade. They could not exit the fight without concedating doctrine itself, so every escalation had to be matched.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_ecclesiastical_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Copyists, limners, and scriptoria whose livelihoods the deployment devalued as edition runs replaced hand duplication. They petitioned city councils and grumbled in the guilds but sat outside the pamphlet conversation that decided the medium's future; their objection never entered the strategic calculus of the reformer-printer alliance.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, manuscript_scribes_and_stationers, excluded,
    moderate, biographical, constrained, regional).

% Modern scholars of the book, the Reformation, and media change who weigh agent-centered against technology-centered accounts from outside the confessional stakes. They read printers' ledgers, fair catalogs, and confiscation inventories, and adjudicate which causal story the surviving record supports.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Moved reformist religious content — vernacular scripture, polemical treatises, sermons, catechisms, illustrated broadsheets, news — from a few production centers to dispersed literate audiences across political borders, faster and cheaper than manuscript copy or pulpit circuits could achieve, letting a geographically scattered movement act in loose concert and answer suppression within weeks rather than years.
% TRANSFER_FUNCTION: Transferred money from pamphlet buyers and edition patrons to printer-publishers, and onward to papermakers, colporteurs, and carters; transferred attention and allegiance from the clerical information establishment to reformist movements and their princely sponsors; transferred risk onto printers who staked capital on titles authorities could burn.
% ABSENT_VOICES: Manuscript scribes, limners, and stationers whose livelihoods the deployment devalued petitioned from outside the conversation that set the medium's course; the non-literate rural majority, whose religious world the pamphlet war restructured, had no seat at all; women, largely barred from the print trades and from public polemic, were absent from every decision that scaled the channel.
% DISAPPEARANCE_RATIONALE: If the deployment arrangement vanished overnight — the reformer-printer alliances dissolved and the channel reverted to manuscript speeds — the reform movements fragment back into regional pulpits, suppression regains the advantage it held against Wycliffe and Hus, printers lose the boom market that financed the industry's expansion, and the confessional map of Europe redraws along slower, more easily policed lines. Every named seat's position depends on the arrangement.
% FOUNDING_PROBLEM: Before 1517, religious dissent had a distribution problem: Wycliffe's and Hus's movements starved for want of a cheap, fast, repeatable channel to distant sympathizers, and every prior reformer was outlasted by authorities who controlled the slow channels. The deployment was built to break that bottleneck — to publish faster than suppression could react.
% FOUNDING_PROBLEM_CORROBORATION: Contested between seats. Beneficiary parties — the reformer succession and the print trade — attest the channel problem is perennially live, since every later dissent movement refounds it. Corroboration from outside the beneficiary set cuts the other way on the specific instance: imperial and papal archival records show the bottleneck broken by mid-century (the Edict of Worms and the 1559 Index legislate against a working channel, and Roman adoption of counter-print concedes its efficacy), and media historians across schools date the decisive deployment window to 1517–1540s. No wholly disinterested party attests that the founding problem remains open in its original form.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.28, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. The claimed type is rope because this reading's own lights describe a genuine collective-action solution — dispersed dissent coordinated through a cheap, fast, repeatable channel — whose participants were net beneficiaries and whose alternatives (manuscript circulation, preaching, congregational song) were complemented rather than suppressed by the arrangement itself. The metrics describe the arrangement's actual operation. Extractiveness 0.28: printer margins, edition speculation, and the unpriced costs of polemical saturation ride on top of a predominantly coordinative channel. Suppression 0.58: the arrangement operated inside a hardening coercive environment (Worms, the Index, licensing regimes) that it evaded strategically rather than dismantled. Theater ratio 0.14: nearly all print activity performed real communicative work, with only late-period confessional boilerplate drifting toward ritual. Accessibility collapse 0.35: alternatives visibly persisted, so understanding the channel did not annihilate its substitutes. Resistance 0.72: the deployment met some of the fiercest active resistance in the media record — burnings, bans, indices, counter-print campaigns. The three measurement series share one grid (1517, 1525, 1540, 1555, 1575, 1600). suppression_requirement is authored because the story specifically tracks enforcement-capacity change: censorship machinery ratcheted upward for eight decades while effective grip plateaued below full closure, because evasion innovation — false imprints, smuggling routes, anonymous publication — scaled with each escalation. Extractiveness drifts gently upward as rent-seeking layered onto coordination: the boom matured, major houses consolidated hot-title franchises, and prices firmed on confessional essentials — but never approached the regime where the coordination story becomes cover. Suppression is authored as a raw structural property of the coercive environment; only extractiveness is scaled by directionality and scope in the engine's computation. Receipt surface: the pecuniary gains demonstrably accrued to commercial_printers (edition margins and boom-market profits), while the reformers' gains were influence rather than receipts — hence gain_flow names the printer seat. fixing_cost is prohibitive: the seats positioned to remove the arrangement spent eight decades and escalating enforcement budgets failing to close it, with suppression costs outrunning any recoverable benefit; the arrangement could be outlived but not affordably removed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute differently from identical structural facts. From the catholic_ecclesiastical_hierarchy seat the arrangement is experienced as hostile displacement: a monopoly built over centuries dissolved within a decade by a channel it did not control and could not join without conceding doctrine — trapped exit, civilizational horizon, maximal exposure. From the commercial_printers seat the same arrangement is a portfolio: genres bought and sold, risk priced, markets rotated — mobile exit, minimal exposure. From the protestant_reformers seat it is vocation: identity lock converts strategic commitment into existential commitment. The fusion is simultaneously ideological (recantation equals damnation) and professional (recantation equals career annihilation); breaking that frame would convert the reformer seat from committed deployer into negotiable counterparty and lower deployment intensity. Same-level divergence is visible between printers and reformers, who operate at comparable organized power but face opposite exit conditions — mobility versus identity lock — which is why one seat treats the channel as inventory and the other as destiny. The vernacular_reading_public sits near symmetric: real access gains against real prices and saturation costs. The engine computes these divergences from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (reformers, printers, princes, reading public) derive low directionality for those seats; the victim declaration (catholic hierarchy) derives high directionality. Printers carry agenda_setter as primary role — they administered the medium — but appear among the beneficiaries because they collected from it, so their derived d sits near the beneficiary end, which is correct. Princes are beneficiaries whose enforcement power serves the arrangement regionally; their revocable patronage keeps them nearer the beneficiary end than their institutional power alone would suggest. The reading public's dual role places it near symmetric. No directionality_overrides are authored: no seat's derived d misdescribes its structural relationship, and the derivation chain handles the agenda-setter-who-also-collects case through the secondary role without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem instance — breaking the pre-1517 distribution bottleneck for religious dissent — was solved within a generation, and the arrangement outlived that instance. This is exactly where misclassification threatens in both directions. Read as mandate-outlived-function, the arrangement looks like a piton: an old channel maintained by inertia. But the function did not die; it transformed into the standing infrastructure of confessional and commercial print, and the theater ratio stays low (0.14) because the activity remained functional rather than performative. Read cynically, the printer margins and the propaganda saturation look like a snare wearing coordination as cover. But the coordination function is primary and verifiable against manuscript baselines — speed, cost, and border-crossing repeatability — and the extraction is subordinate and bounded. Classifying as rope preserves both guards: genuine coordination that accumulated mild rent, not performance and not predation. Mandatrophy is deliberately not declared resolved: the mandate (standing mass-communication capacity) remains live even though the founding problem instance is closed, and the R5 interview records the instance/status distinction as contested rather than forcing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the press_reformation_causation kernel (reading: strategic_deployment). Does the agency-first account hold cleanly, or does causal weight migrate to the artifact (toward technological_determinism) or into reciprocal shaping (toward mutual_shaping), collapsing this constraint into a sibling?',
    'Comparative counterfactual analysis across the three linked family stories: if the deployment''s success survives substitution of comparable-but-different media economics, agency carries the weight; if outcomes track affordances regardless of strategist intent, the reading collapses toward a sibling and its epsilon and beneficiary structure dissolve into that sibling''s referent.',
    'If the reading fails, its epsilon (0.28) and victim structure are redescribed by a sibling with a different referent arrangement, different payer set, and plausibly a different classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the press_reformation_causation kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    neutrality_of_medium_premise,
    'Was the press genuinely neutral capacity awaiting purposeful use, or did its affordances — fixity, multiplication, anonymity, speed — select which strategies succeeded, making neutrality a retrospective attribution by the winners?',
    'Trace failed deployments through equivalent channels — pre-1517 manuscript propaganda campaigns (Wycliffite, Hussite) and post-1600 censor-adapted print — and test whether strategist quality predicts outcome once affordances are held constant.',
    'If affordances steered outcomes, the neutrality axiom weakens and the reading drifts toward mutual_shaping; epsilon is unaffected because the referent arrangement is fixed, but the coordination-function attribution shifts from agents alone to the agent-artifact pair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_of_medium_premise, conceptual, 'Whether the medium-neutrality premise survives affordance analysis.').

omega_variable(
    rent_vs_displacement,
    'Are the catholic hierarchy''s losses extraction borne by a victim seat, or ordinary competitive displacement in a contested information market — is there a victim in this reading at all?',
    'Separate rents from returns: compare reformist-title margins against commodity print (almanacs, primers, schoolbooks) produced in the same houses; sustained above-market margins on confessional essentials indicate channel rents, parity indicates displacement without extraction.',
    'If no rents, the victims declaration overstates and the constraint is a purer rope; if rents concentrated in the major houses, the arrangement drifts toward tangled_rope with commercial_printers as the extracting seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_vs_displacement, empirical, 'Whether the payer seat bears extraction or mere competitive loss.').

omega_variable(
    survivorship_bias_in_imprint_record,
    'Do surviving imprint counts measure deployment strategy, or survivorship bias — do lost editions, ephemeral broadsheets, and suppressed titles distort the temporal series the measurements rest on?',
    'Cross-check the surviving-imprint series against printers'' account books, the Frankfurt fair catalogs, and confiscation inventories to bound the lost fraction per decade.',
    'If the lost fraction grows over the interval, the measured extractiveness and theater trajectories understate late-period activity and the drift conclusions weaken; the suppression ratchet, documented in edicts and indices, is less exposed to this bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_bias_in_imprint_record, empirical, 'Archival survivorship bias underlying the temporal measurements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1517, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__strategic_deployment, theater_ratio, 1517, 0.06).
narrative_ontology:measurement_basis(pres_tr_t1517, observed).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causation__strategic_deployment, theater_ratio, 1525, 0.08).
narrative_ontology:measurement_basis(pres_tr_t1525, observed).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__strategic_deployment, theater_ratio, 1540, 0.1).
narrative_ontology:measurement_basis(pres_tr_t1540, observed).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causation__strategic_deployment, theater_ratio, 1555, 0.11).
narrative_ontology:measurement_basis(pres_tr_t1555, observed).
narrative_ontology:measurement(pres_tr_t1575, press_reformation_causation__strategic_deployment, theater_ratio, 1575, 0.13).
narrative_ontology:measurement_basis(pres_tr_t1575, observed).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__strategic_deployment, theater_ratio, 1600, 0.14).
narrative_ontology:measurement_basis(pres_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__strategic_deployment, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement_basis(pres_be_t1517, observed).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causation__strategic_deployment, base_extractiveness, 1525, 0.2).
narrative_ontology:measurement_basis(pres_be_t1525, observed).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__strategic_deployment, base_extractiveness, 1540, 0.24).
narrative_ontology:measurement_basis(pres_be_t1540, observed).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causation__strategic_deployment, base_extractiveness, 1555, 0.26).
narrative_ontology:measurement_basis(pres_be_t1555, observed).
narrative_ontology:measurement(pres_be_t1575, press_reformation_causation__strategic_deployment, base_extractiveness, 1575, 0.27).
narrative_ontology:measurement_basis(pres_be_t1575, observed).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__strategic_deployment, base_extractiveness, 1600, 0.28).
narrative_ontology:measurement_basis(pres_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causation__strategic_deployment, suppression_requirement, 1517, 0.15).
narrative_ontology:measurement_basis(pres_su_t1517, observed).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__strategic_deployment, suppression_requirement, 1525, 0.32).
narrative_ontology:measurement_basis(pres_su_t1525, observed).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causation__strategic_deployment, suppression_requirement, 1540, 0.44).
narrative_ontology:measurement_basis(pres_su_t1540, observed).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causation__strategic_deployment, suppression_requirement, 1555, 0.52).
narrative_ontology:measurement_basis(pres_su_t1555, observed).
narrative_ontology:measurement(pres_su_t1575, press_reformation_causation__strategic_deployment, suppression_requirement, 1575, 0.56).
narrative_ontology:measurement_basis(pres_su_t1575, observed).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__strategic_deployment, suppression_requirement, 1600, 0.58).
narrative_ontology:measurement_basis(pres_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The colloquial label 'the press and the Reformation' covers three structurally distinct causal claims (epsilon-invariance decomposition): technological_determinism (artifact as operative cause), mutual_shaping (coupled co-evolution of technology and use), and this file's strategic_deployment (purposive agents wielding a neutral instrument). Each carries its own epsilon over its own referent arrangement and its own beneficiary/victim structure; they are linked as a constraint family via affects_constraints. No member is upstream of another — the three compete at the same explanatory level — but the determinist claim functions as the position this reading defines itself against, and mutual_shaping functions as the refinement proposed against both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
