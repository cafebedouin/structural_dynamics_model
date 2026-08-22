% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Weaponization of Print by Reformer-Printer-Prince Alliances (1517-1555)
 *   domain: historical/media-religious-economic
 *
 * SUMMARY:
 *   Between the Ninety-Five Theses (1517) and the Peace of Augsburg (1555),
 *   an alliance of reformers, commercial printers, and protective princes
 *   built and aimed a print apparatus at the Roman Church's information
 *   monopoly and revenue streams — and, in its later phase, at adjacent
 *   targets. Short vernacular pamphlets, illustrated broadsheets, and
 *   complete vernacular Bibles moved faster than any countermeasure;
 *   printer-ledger and dedication evidence shows format, timing, and pricing
 *   chosen deliberately for disruptive and commercial effect. This story
 *   instantiates ONLY the strategic_deployment reading of the
 *   press_reformation_causality kernel (see kernel_context); artifact-level
 *   questions belong to the sibling readings. KEY AGENTS (by structural
 *   relationship): - protestant_reformers: agenda-setting strategist
 *   (organized/identity_locked) — aims the apparatus, fused with the movement
 *   it carries - commercial_printer_publishers: primary commercial collector
 *   (organized/arbitrage) — takes the sales flow, bears capital and
 *   censorship risk, can switch markets - evangelical_territorial_princes:
 *   protective collector (powerful/constrained) — sequesters church property,
 *   enforces the arrangement territorially - vernacular_lay_readers:
 *   dual-positioned public (moderate/constrained) — gains access, pays prices
 *   and confessional discipline - roman_clergy_hierarchy: primary target
 *   (institutional/trapped) — loses information monopoly and revenue, cannot
 *   concede - monastic_scriptoria_copyists: displaced economic target
 *   (powerless/trapped) — manuscript livelihood priced out -
 *   jewish_german_communities: later-phase target (powerless/trapped) —
 *   polemic at scale, no reply channel - habsburg_imperial_authority:
 *   counter-enforcer turned bearer of costs (institutional/constrained) —
 *   enforcement fails at prohibitive cost - print_culture_historians:
 *   analytical observer (analytical/analytical) Constraint-family note: the
 *   colloquial label 'print and the Reformation' decomposes into three
 *   structurally distinct claims per the epsilon-invariance principle — this
 *   deployment arrangement (concentrated collectors, displaced bearers of
 *   cost, epsilon 0.64), a determinism reading in which an autonomous
 *   technology makes outcomes inevitable (no strategists left to collect,
 *   epsilon near zero), and a co-constitution reading of mutual feedback
 *   (directional asymmetry dissolved into loop participation). Each is a
 *   separate file; this one links both siblings via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.64).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.58).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.64).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Weaponization of Print by Reformer-Printer-Prince Alliances (1517-1555)").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "historical/media-religious-economic").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, '5b75cbd9-1842-4566-ba00-5cf18ecfd954').
narrative_ontology:cs_kernel_codification('5b75cbd9-1842-4566-ba00-5cf18ecfd954', distributed).
narrative_ontology:cs_authority_grounding('5b75cbd9-1842-4566-ba00-5cf18ecfd954', expertise).
narrative_ontology:cs_interpretation_layer_present('5b75cbd9-1842-4566-ba00-5cf18ecfd954').
narrative_ontology:cs_reading_relation('5b75cbd9-1842-4566-ba00-5cf18ecfd954', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('5b75cbd9-1842-4566-ba00-5cf18ecfd954', press_reformation_causality__co_constitution, influences).
narrative_ontology:cs_axiom('5b75cbd9-1842-4566-ba00-5cf18ecfd954', foundational, strategic_human_agency_sufficient_explanation).
narrative_ontology:cs_axiom_status(strategic_human_agency_sufficient_explanation, holdable).
narrative_ontology:cs_axiom_grounding('5b75cbd9-1842-4566-ba00-5cf18ecfd954', strategic_human_agency_sufficient_explanation, empirically_contingent).
narrative_ontology:cs_axiom('5b75cbd9-1842-4566-ba00-5cf18ecfd954', secondary, print_market_economics_shaped_message_form).
narrative_ontology:cs_axiom_status(print_market_economics_shaped_message_form, holdable).
narrative_ontology:cs_axiom_grounding('5b75cbd9-1842-4566-ba00-5cf18ecfd954', print_market_economics_shaped_message_form, empirically_contingent).
narrative_ontology:cs_reference_frame('5b75cbd9-1842-4566-ba00-5cf18ecfd954', strategic_instrumental_agency).
narrative_ontology:cs_drift_state('5b75cbd9-1842-4566-ba00-5cf18ecfd954', contemporary_post_eisenstein, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('5b75cbd9-1842-4566-ba00-5cf18ecfd954', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, commercial_printer_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, evangelical_territorial_princes).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, vernacular_lay_readers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, roman_clergy_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, monastic_scriptoria_copyists).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, jewish_german_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, commercial_printer_publishers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, vernacular_lay_readers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, habsburg_imperial_authority).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, sola_scriptura).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wittenberg and allied theologians decided what got printed: short vernacular pamphlets timed to controversies, prefaces telling readers how to read, catechisms and sermon aids for parish use, woodcuts commissioned for the semiliterate. Their standing, income, and physical safety came to depend on the apparatus continuing to run; leaving it meant recantation, and after 1521 leaving Wittenberg's protection meant arrest under the imperial ban.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, protestant_reformers, agenda_setter,
    organized, generational, identity_locked, continental).

% Urban press owners financed editions on speculation, collected the sales revenue of an unprecedented pamphlet boom, and bore the downside: confiscated stock, revoked privileges, exile. Many kept options open by serving more than one confession or moving shop between cities when a territory turned hostile; the trade's economics rewarded whoever could read demand fastest.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, commercial_printer_publishers, beneficiary,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, commercial_printer_publishers, payer).

% Electors and city councils granted printing privileges, shielded presses from imperial officers, endowed court preacheries and university posts, and took over church properties and endowments within their lands. Once committed they stood exposed to imperial ban and war; the Schmalkaldic defeat of 1547 showed what backing out cost.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, evangelical_territorial_princes, beneficiary,
    powerful, generational, constrained, regional).

% Townsmen, artisans, modest clergy, and some women bought or heard read the cheap quartos: scripture in their own language, polemic, news sheets, household devotion. They paid purchase prices, gained direct textual access previously mediated by clergy, and became visible members of a confession that visitations, consistories, and rival authorities then disciplined.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, vernacular_lay_readers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, vernacular_lay_readers, payer).

% Rome and the episcopate held a gatekeeping position over doctrine, office, and revenue that the pamphlet wave outflanked: indulgence income fell, disputes escaped Latin and the universities, and official replies appeared months after the attacks. Conceding doctrinal ground threatened the entire authority claim, so the hierarchy fought with bans, indexes, and inquisitorial machinery that arrived slower than the presses ran.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, roman_clergy_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Copyists, illuminators, parchment makers, and the scriptoria that employed them watched demand for hand-copied books collapse within a generation. Their skills were tied to the manuscript trade, many were vow-bound to houses whose incomes shrank, and few could move into the new trade's urban centers or meet its capital requirements.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, monastic_scriptoria_copyists, payer,
    powerless, biographical, trapped, regional).

% From the 1530s the same presses and distribution networks turned out large-scale anti-Jewish polemic alongside confessional literature, and tract campaigns hardened expulsion policies in several territories. The communities had no printing infrastructure or distribution network of comparable reach through which to answer.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, jewish_german_communities, payer,
    powerless, generational, trapped, regional).

% Charles V and the imperial estates' machinery spent two decades issuing mandates against unauthorized printing and unlicensed preaching, then fought a war to restore unitary religious order. Each ban was evaded faster than it could be enforced; by 1555 the empire accepted permanent confessional division, having spent its enforcement capacity for nothing durable.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, habsburg_imperial_authority, payer,
    institutional, generational, constrained, continental).

% Reconstruct the episode from imprints, printers' ledgers, dedications, and correspondence; time publication against diet sessions and controversy cycles; weigh deliberate strategy against emergent system effects. They sit entirely outside the arrangement they study.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, print_culture_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, commercial_printer_publishers).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the collective-action problem of coordinating a dispersed religious dissent: rapid, cheap, standardized replication of arguments, vernacular scripture, and visual propaganda across linguistic territories, faster than any authority could respond.
% TRANSFER_FUNCTION: Moved communicative authority and revenue away from the Roman Church's gatekeeping apparatus and the manuscript economy toward the reformer-printer-prince alliance: indulgence and altar-fees income redirected into pamphlet and Bible purchases, church properties sequestered by princes, legitimacy shifted from clerical mediation to direct text access.
% ABSENT_VOICES: Rural illiterate populations confessionalized from above, Jewish communities targeted by the apparatus's later phase, and the manuscript-trade workers whose livelihoods were displaced — none sat at the tables where print strategy was decided; the Catholic elite, by contrast, was present and answered in kind, so the absence runs along class and target lines, not confessional-elite lines.
% DISAPPEARANCE_RATIONALE: If the strategic deployment arrangement vanished overnight — presses running but nobody aiming them — the dissenting movement loses its speed advantage, the Church's information monopoly holds for decades longer, princely sequestration of church property does not occur on schedule, and the European confessional map rearranges around a much slower, more contested diffusion.
% FOUNDING_PROBLEM: How can a dissenting movement lacking institutional power reach a mass audience before authorities suppress it — specifically, how to break the Roman Church's communication monopoly and fund the movement while doing so.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the beneficiary set: the Peace of Augsburg's own text settles the conflict the arrangement opened; imperial diet records and Habsburg mandates treat unauthorized printing as the crisis mechanism, attesting both the problem's reality and its resolution; and book-history scholarship (Febvre-Martin, Pettegree) reconstructs printer-ledger evidence of deliberate strategy independent of any confessional allegiance. No paying seat attests a flattering genealogy, and none needs to.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.64: the arrangement transferred indulgence revenue, communicative authority, and the manuscript trade's livelihoods to the alliance while also delivering real goods — cheap scripture, news, rising literacy — that partially offset the transfer; heavy but not total. Suppression 0.58: the apparatus required continuous active protection (printing privileges, princely shields, evasion of imperial mandates) and lived inside a coercive envelope it provoked; suppression is authored as the raw structural fact, unscaled — only extractiveness gets scaled by directionality and scope in the engine. Theater 0.20: reproduction stayed mostly functional; the ritualized share (anniversary tracts, martyr pamphlets recycled as genre) rose as confessional lines hardened. Accessibility_collapse 0.45: alternatives — manuscript circulation, preaching, Latin scholarship — persisted throughout at rising cost, so understanding the arrangement did not close off other routes. Resistance 0.70: sustained institutional counterattack from Worms (1521) through the Schmalkaldic War. All three series run on one shared eight-point grid (1517-1555) so no metric borrows another's endpoint; the small 1555 dip in extractiveness marks the Augsburg settlement converting open seizure into legalized possession. Boltzmann type identity_coordination reflects the dominant function — vernacular Bibles, catechisms, and hymnals built durable confessional membership — with the gaming risk acknowledged: the identity frame was genuinely load-bearing here, not cover, but the same rails carried the transfer of wealth and authority.
 *
 * PERSPECTIVAL GAP:
 *   Payer seats and collector seats should compute differently. From the curia's chair the arrangement is an illegitimate assault that destroyed a millennium-old mediation office; from the printer's chair it is a market opportunity with risk pricing; from the princes' chair a sovereignty play; from the reader's chair mostly a windfall with discipline attached. Same-era actors diverge sharply on exit: printers hold arbitrage-grade mobility across jurisdictions while the curia is pinned by its own doctrine and the copyists by vows and skills — equal nominal century, opposite structural positions. The engine computes per-seat classifications from these structural facts; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Collector declarations (reformers, printers, princes, readers) derive low d — the arrangement subsidizes them — with arbitrage exit pushing printers nearest the beneficiary end. Target declarations (curia, scriptoria trades, Jewish communities) plus the imperial authority's failed counter-enforcement derive high d; trapped exits (doctrine for the curia, vows for the copyists, absence of reply infrastructure for Jewish communities) pin them near the full-target end. Readers sit intermediate: declared collector with a payer secondary role and constrained exit, placed moderately above pure beneficiary — the unresolved remainder is flagged in the omega on their net position. No directionality overrides are authored: declaration-plus-exit data already yields the correct ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a dissent lacking institutions reaches a mass audience before suppression, funded by the effort — was solved decisively by the 1530s and legally closed at Augsburg in 1555. The apparatus persisted afterward as routine confessional-state machinery with its mandate outlived: hence founding_problem_status dead paired with disappearance_verdict world_rearranges, the mismatch that flags the zombie path for downstream consumers. The classification guards both mislabels: reading the arrangement as pure predation erases the genuine coordination that built literate dissent communities and cheaper books; reading it as innocent toolmaking hides the sequestered property, the priced-out manuscript trades, and the later campaigns against targets that never attacked anyone. The tangled-rope claim is the reading's own structural hypothesis — hybrid function, hybrid cost — offered independently of the metrics for the engine to confirm or overturn.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates only the strategic_deployment reading of the press_reformation_causality kernel — what would the sibling readings change structurally?',
    'Author the sibling files: technological_determinism strips strategic collectors and cost-bearers (an inevitable outcome has no strategists to reward) and classifies the press as a near-mountain enabling condition; co_constitution replaces directional collector/target asymmetry with mutually constitutive feedback, damping chi asymmetry.',
    'Under determinism, epsilon collapses toward negligible — no agent collects from an inevitability; under co-constitution, collectors and cost-bearers blur into participants and the tangled-rope verdict softens toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption rewrites the beneficiary/victim surface.').

omega_variable(
    epsilon_referent_boundary,
    'Does epsilon''s referent — the standing strategic-deployment arrangement — include the apparatus''s later turns against non-Church targets (notably the anti-Jewish tract campaigns of the 1540s), or only the founding anti-clerical deployment?',
    'Trace imprint runs, financiers, and distribution channels: if the same alliance infrastructure carried the later campaigns, the referent widens to the whole apparatus.',
    'Widening the referent raises epsilon and strengthens the snare component; restricting it to the founding deployment keeps epsilon at tangled-rope levels and isolates the later campaigns as descendant constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_boundary, empirical, 'Scope of the extraction referent across the apparatus''s target shifts.').

omega_variable(
    lay_reader_net_position,
    'Were vernacular lay readers net gainers of access or conscripted bearers of confessional discipline and violence?',
    'Regional studies of reception, literacy, and confessionalization costs: visitation records, price series for pamphlets against wages, migration and refugee flows from contested territories.',
    'A net-gainer reading keeps readers near the beneficiary end (d roughly 0.15); a net-bearer reading pushes them toward symmetry and raises effective extraction across the class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_reader_net_position, empirical, 'Ambiguous dual position of the reading public between access gained and discipline incurred.').

omega_variable(
    suppression_attribution_boundary,
    'Does the suppression score measure the arrangement''s own coercive maintenance (princely protection, printing privileges, licensed orthodoxy) or the external counter-coercion it provoked (imperial bans, Index, war)?',
    'Separate enforcement directed at keeping the apparatus running from enforcement aimed at destroying it; count only the former in the constraint''s suppression, treating the rest as environmental resistance.',
    'Counting only self-maintenance lowers suppression toward 0.40; counting the full conflict envelope raises it toward 0.70 and shifts computed types toward snare at the target seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_attribution_boundary, conceptual, 'Attribution boundary for coercive force surrounding the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__strategic_deployment, theater_ratio, 1517, 0.08).
narrative_ontology:measurement_basis(pres_tr_t1517, observed).
narrative_ontology:measurement(pres_tr_t1521, press_reformation_causality__strategic_deployment, theater_ratio, 1521, 0.1).
narrative_ontology:measurement_basis(pres_tr_t1521, observed).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__strategic_deployment, theater_ratio, 1525, 0.12).
narrative_ontology:measurement_basis(pres_tr_t1525, observed).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causality__strategic_deployment, theater_ratio, 1530, 0.14).
narrative_ontology:measurement_basis(pres_tr_t1530, observed).
narrative_ontology:measurement(pres_tr_t1534, press_reformation_causality__strategic_deployment, theater_ratio, 1534, 0.15).
narrative_ontology:measurement_basis(pres_tr_t1534, observed).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__strategic_deployment, theater_ratio, 1540, 0.17).
narrative_ontology:measurement_basis(pres_tr_t1540, observed).
narrative_ontology:measurement(pres_tr_t1546, press_reformation_causality__strategic_deployment, theater_ratio, 1546, 0.19).
narrative_ontology:measurement_basis(pres_tr_t1546, observed).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__strategic_deployment, theater_ratio, 1555, 0.2).
narrative_ontology:measurement_basis(pres_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement_basis(pres_be_t1517, observed).
narrative_ontology:measurement(pres_be_t1521, press_reformation_causality__strategic_deployment, base_extractiveness, 1521, 0.47).
narrative_ontology:measurement_basis(pres_be_t1521, observed).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__strategic_deployment, base_extractiveness, 1525, 0.56).
narrative_ontology:measurement_basis(pres_be_t1525, observed).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causality__strategic_deployment, base_extractiveness, 1530, 0.61).
narrative_ontology:measurement_basis(pres_be_t1530, observed).
narrative_ontology:measurement(pres_be_t1534, press_reformation_causality__strategic_deployment, base_extractiveness, 1534, 0.63).
narrative_ontology:measurement_basis(pres_be_t1534, observed).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__strategic_deployment, base_extractiveness, 1540, 0.65).
narrative_ontology:measurement_basis(pres_be_t1540, observed).
narrative_ontology:measurement(pres_be_t1546, press_reformation_causality__strategic_deployment, base_extractiveness, 1546, 0.67).
narrative_ontology:measurement_basis(pres_be_t1546, observed).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__strategic_deployment, base_extractiveness, 1555, 0.64).
narrative_ontology:measurement_basis(pres_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement_basis(pres_su_t1517, observed).
narrative_ontology:measurement(pres_su_t1521, press_reformation_causality__strategic_deployment, suppression_requirement, 1521, 0.42).
narrative_ontology:measurement_basis(pres_su_t1521, observed).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causality__strategic_deployment, suppression_requirement, 1525, 0.5).
narrative_ontology:measurement_basis(pres_su_t1525, observed).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causality__strategic_deployment, suppression_requirement, 1530, 0.55).
narrative_ontology:measurement_basis(pres_su_t1530, observed).
narrative_ontology:measurement(pres_su_t1534, press_reformation_causality__strategic_deployment, suppression_requirement, 1534, 0.57).
narrative_ontology:measurement_basis(pres_su_t1534, observed).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__strategic_deployment, suppression_requirement, 1540, 0.56).
narrative_ontology:measurement_basis(pres_su_t1540, observed).
narrative_ontology:measurement(pres_su_t1546, press_reformation_causality__strategic_deployment, suppression_requirement, 1546, 0.6).
narrative_ontology:measurement_basis(pres_su_t1546, observed).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__strategic_deployment, suppression_requirement, 1555, 0.58).
narrative_ontology:measurement_basis(pres_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% Family decomposition of the 'print and the Reformation' label per the epsilon-invariance principle: strategic_deployment (this file, tangled_rope, epsilon 0.64, referent = the deployment arrangement as its proponents assess it), technological_determinism (near-mountain enabling-condition claim, no strategic collectors, epsilon near zero), co_constitution (feedback-loop claim, directional asymmetry dissolved into loop participation). Upstream/downstream: determinism and co-constitution accounts cite much of the same ledger and timing evidence this reading cites as proof of strategy; the disagreement is located in whether concentration of gain and deliberate targeting are explanatorily primary. Each member links the others; no orphan stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
